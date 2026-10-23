#lang racket

;; Grammar-generation + tokenizer smoke tests for the Effervesce front end.
;;
;; Effervesce is brace-delimited (not layout-sensitive), so it loads with
;; gen-and-load (no INDENT/DEDENT), and every keyword lexes as an ID token.

(require rackunit
         rackunit/text-ui
         racklr/tree
         racklr/gen-test
         racklr/g4-parse
         racklr/gend-parser)

(define-values (eff-parse eff-tokenize eff-tok-type eff-tok-value)
  (gen-and-load "../effervesce/grammar/Effervesce.g4"))

(define (tok-values in)
  (map eff-tok-value (eff-tokenize in)))

(define (parses? in)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (any-tree? (eff-parse in))))

(define suite
  (test-suite
   "Effervesce grammar/tokenizer"
   (test-case "keywords lex as ID tokens"
     (check-equal? (eff-tok-type (first (eff-tokenize "system"))) 'ID
                   "system is an ID token")
     (check-equal? (eff-tok-type (first (eff-tokenize "capability"))) 'ID
                   "capability is an ID token")
     (check-equal? (eff-tok-type (first (eff-tokenize "always"))) 'ID
                   "always is an ID token"))
   (test-case "parse smoke tests"
     (check-true (parses? "system X { }") "empty system")
     (check-true (parses? "system X { capability Read<A> { get() -> A } }")
                 "capability with type param")
     (check-true (parses? "system X { given Fs, Log }") "given row")
     (check-true (parses? "system X { Audit via Log { } }") "via handler")
     (check-true (parses? "system X { component C needs Read<Config>, Audit { } }")
                 "component needs row")
     (check-true (parses? "system X { state cache: Config = none }") "state")
     (check-true (parses? "system X { action A { cache := 1 } }") "action")
     (check-true (parses? "system X { always cache.port > 0 }") "invariant")
     (check-true (parses? "system X { never component needs Read and not component needs Audit }")
                 "policy predicate"))
   (test-case "all example files parse"
      (for ([name '("config-reader" "todo-store" "todo-memory" "todo-database" "todo-rules" "todo-peer")])
       (define src (file->string (format "../effervesce/examples/~a.fizz" name)))
       (check-true (parses? src) (format "~a.fizz parses" name))))))

(exit (run-tests suite))
