#lang racket

(require racklr/tree
         "gend-parser/lexer.rkt"
         "gend-parser/parser.rkt")

(provide generate-parser-module)

;; Simple parser generator.
;; Given a grammar CST, returns Racket source string for a generated parser.
;; The generated parser provides: token struct, tokenize, parse.

(define (generate-parser-module grammar-cst #:source-path [source-path #f] #:indent-tokens? [indent-tokens? #f])
  (define rules-node (second (any-tree-children grammar-cst)))
  (define rules (any-tree-children rules-node))
  (define parser-rules (filter (lambda (r) (eq? (any-tree-tag r) 'parser-rule)) rules))
  (define lexer-rules (filter (lambda (r) (eq? (any-tree-tag r) 'lexer-rule)) rules))
  (define frag-rules (filter (lambda (r) (eq? (any-tree-tag r) 'fragment-rule)) rules))
  (define mode-nodes (filter (lambda (r) (eq? (any-tree-tag r) 'mode)) rules))

  ;; Handle tokenVocab: load lexer grammar from separate file
  (define-values (extra-lexer-rules extra-frag-rules extra-mode-nodes)
    (if source-path
        (let* ((options-node (findf (lambda (r) (eq? (any-tree-tag r) 'options)) rules))
               (token-vocab (and options-node (extract-option options-node "tokenVocab"))))
          (if token-vocab
              (let ((lexer-path (build-path (path-only source-path)
                                            (string-append token-vocab ".g4"))))
                (if (file-exists? lexer-path)
                    (load-lexer-grammar-rules lexer-path)
                    (begin (eprintf "Warning: tokenVocab ~a.g4 not found at ~a~n"
                                    token-vocab lexer-path)
                           (values '() '() '()))))
              (values '() '() '())))
        (values '() '() '())))

  ;; Merge lexer rules from tokenVocab
  (set! lexer-rules (append lexer-rules extra-lexer-rules))
  (set! frag-rules (append frag-rules extra-frag-rules))
  (set! mode-nodes (append mode-nodes extra-mode-nodes))

  ;; Extract lexer/fragment rules from mode nodes
  (define mode-lexer-rules
    (for*/list ([mn mode-nodes]
                [r (rest (any-tree-children mn))])
      r))

  ;; Check for left-recursive parser rules
  (detect-left-recursion parser-rules)
  (define all-lexer (append lexer-rules frag-rules mode-lexer-rules))
  (define parser-literals (collect-parser-literals parser-rules))
  ;; Build synthetic lexer rules for parser literals not already covered
  (define implicit-lexer-rules (build-implicit-lexer-rules parser-literals lexer-rules))
  (define all-token-rules (append lexer-rules implicit-lexer-rules))

  ;; Build mode map: mode-name -> list of token rules
  (define mode-map (build-mode-map lexer-rules mode-nodes implicit-lexer-rules))

  (define has-parser-rules (not (null? parser-rules)))
  (define has-newline? (lexer-has-newline-rule? all-lexer))

   (string-join
    (list (gen-header #:indent-tokens? indent-tokens?)
          (gen-match-helpers)
          (gen-lexer-matchers all-lexer)
          (gen-lexer-matchers implicit-lexer-rules)
          (gen-tokenizer mode-map #:has-newline? has-newline? #:indent-tokens? indent-tokens?)
          (gen-parser-helpers)
          (if has-parser-rules (gen-parser-rules parser-rules) "")
          (if has-parser-rules (gen-parser-provides parser-rules) "")
          (if has-parser-rules (gen-entry parser-rules) (gen-lexer-entry)))
    "\n"))
(module+ main
  (displayln "gend-parser loaded."))
