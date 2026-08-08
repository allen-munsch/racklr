#lang racket

;; eval-gsp-node.rkt — Cross-file getStaticProps/getStaticPaths via Node.js
;;
;; When a page's data-fetching functions reference imports from other
;; files or npm packages (gray-matter, etc.), shell out to Node for
;; evaluation. Node v24+ --experimental-strip-types handles .ts imports.
;;
;; Provides: has-cross-file-imports?, eval-gsp-via-node

(require racket/string
         racket/file
         racket/path
         racklr/tsx-preprocess)

(provide has-cross-file-imports?
         eval-gsp-via-node)

(define (has-cross-file-imports? source)
  (regexp-match? #px"from\\s+['\"](?:\\.\\./|./)" source))

(define (eval-gsp-via-node source project-root)
  ;; Evaluate getStaticProps or getStaticPaths via Node.js.
  ;; Returns JSON string for _pageData, or #f on failure.
  (define gsp-rx #px"(?:export )?(?:async )?(?:const |function )getStatic(Props|Paths)")
  (define gsp-m (regexp-match-positions gsp-rx source))
  (if gsp-m
      (let* ([fn-start (caar gsp-m)]
             [body-start
              (let loop ([pos fn-start])
                (and (< pos (string-length source))
                     (if (char=? (string-ref source pos) #\{)
                         (+ pos 1)
                         (loop (+ pos 1)))))]
             [is-paths? (regexp-match? #px"getStaticPaths"
                                       (substring source
                                                  fn-start
                                                  (min (+ fn-start 25)
                                                       (string-length source))))]
             [fn-body/inner
              (and body-start
                   (let loop ([pos body-start] [depth 1])
                     (cond [(= depth 0)
                            (substring source body-start (- pos 1))]
                           [(>= pos (string-length source)) #f]
                           [(char=? (string-ref source pos) #\{)
                            (loop (+ pos 1) (+ depth 1))]
                           [(char=? (string-ref source pos) #\})
                            (loop (+ pos 1) (- depth 1))]
                           [(or (char=? (string-ref source pos) #\")
                                (char=? (string-ref source pos) #\'))
                            (let ([end (advance-past-string source pos
                                                            (string-ref source pos))])
                              (loop end depth))]
                           [else (loop (+ pos 1) depth)])))]
             [fn-signature
              (let* ([header-end (if body-start (- body-start 1) fn-start)]
                     [header (substring source fn-start header-end)]
                     [sig (regexp-replace #px"^export\\s+" header "")])
                (string-trim sig))])
        (if (and fn-body/inner (> (string-length fn-body/inner) 0))
            (let* ([import-line-rx #px"import\\s+[^;]+;"]
                   [all-import-lines (regexp-match* import-line-rx source)]
                   [data-imports
                    (filter (lambda (line)
                              (not (or (regexp-match? #px"from\\s+['\"]react" line)
                                       (regexp-match? #px"from\\s+['\"]next" line)
                                       (regexp-match? #px"\\.css['\"]" line)
                                       (regexp-match? #px"components/" line))))
                            all-import-lines)]
                   [tmpfile (build-path project-root
                                        (format ".racklr-eval-~a.mjs" (random 100000)))]
                   [script-content
                    (string-join
                     (append
                      (map (lambda (imp)
                             (regexp-replace* #px"from\\s+['\"](\\.{1,2}[^'\"]+)['\"]" imp
                                              "from '\\1.ts'"))
                           data-imports)
                      (list ""
                             (format "~a { ~a };"
                                     fn-signature fn-body/inner)
                            ""
                            (if is-paths?
                                (string-join
                                 '("const result = getStaticPaths();"
                                   "console.log('__RACKLR_PATHS__' + JSON.stringify(result));")
                                 "\n")
                                (if (regexp-match? #px"async" fn-signature)
                                    "getStaticProps().then((r) => console.log('__RACKLR_PROPS__' + JSON.stringify(r)));"
                                    "const __r = getStaticProps();\nconsole.log('__RACKLR_PROPS__' + JSON.stringify(__r));"))))
                     "\n")]
                   [outfile (build-path project-root ".racklr-eval-out.txt")])
              (with-output-to-file tmpfile #:exists 'replace
                (lambda () (display script-content)))
              (system (format "cd ~a && node --experimental-strip-types ~a > ~a 2>&1"
                              (path->string (path->complete-path project-root))
                              (path->string (file-name-from-path tmpfile))
                              (path->string outfile)))
              (define stdout (file->string outfile))
              (delete-file tmpfile)
              (delete-file outfile)
              (define props-marker-rx #px"__RACKLR_PROPS__")
              (define props-pos (regexp-match-positions props-marker-rx stdout))
              (if props-pos
                  (let* ([json-start (cdar props-pos)]
                         [json-str (substring stdout json-start (string-length stdout))]
                         [trimmed (string-trim json-str)])
                    trimmed)
                  #f))
            #f))
      #f))
