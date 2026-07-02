#lang racket

(require racket/string
         racket/file
         racket/system)

(provide esbuild-bundle)

;; esbuild-bundle : resolve imports, strip types, preserve JSX
;; Returns a single bundled string with JSX intact.

(define (find-esbuild)
  (or (find-executable-path "esbuild")
      (let loop ([dir (normalize-path ".")])
        (let ([candidate (build-path dir "node_modules" ".bin" "esbuild")])
          (cond [(file-exists? candidate) candidate]
                [(string=? (path->string dir) "/") #f]
                [else (loop (normalize-path (build-path dir "..")))])))
      (error 'esbuild-bundle "esbuild not found; install with `npm install esbuild`")))

(define (esbuild-bundle #:entry entry
                        #:external [external null]
                        #:fmt [fmt "iife"]
                        #:minify [minify? #f]
                        #:working-dir [working-dir (current-directory)])
  (define args
    (append
     (list (find-esbuild)
           entry
           "--bundle"
           "--jsx=preserve"
           (string-append "--format=" fmt))
     (if minify? '("--minify") '())
     (map (lambda (e) (string-append "--external:" e)) external)))
  
  (define args-str
    (string-join (map (lambda (a)
                        (define s (if (path? a) (path->string a) a))
                        (if (string-contains? s " ")
                            (string-append "\"" s "\"")
                            s))
                      args)
                 " "))
  
  (define outfile (make-temporary-file "esbuild-out-~a.js"))
  
  (define working-dir-str
    (if (path? working-dir) (path->string working-dir) working-dir))
  
  (define success?
    (system (format "cd ~a && ~a > ~a 2>&1"
                    (if (string-contains? working-dir-str " ")
                        (string-append "\"" working-dir-str "\"")
                        working-dir-str)
                    args-str
                    outfile)))
  
  (define result
    (with-handlers ([exn:fail:filesystem? (lambda (e) "")])
      (file->string outfile)))
  
  (delete-file outfile)
  
  (unless success?
    (error 'esbuild-bundle "esbuild failed: ~a" result))
  
  result)
