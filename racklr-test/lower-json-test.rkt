#lang racket

(require rackunit
         racklr/tree
         racklr/uir
         racklr/lower-json
         racklr/gen-json-parser)

(define (check-lower input-str expected-uir)
  "Parse JSON string, lower to UIR, compare with expected UIR."
  (define cst (parse input-str))
  (define actual (lower-json cst))
  (define expected-sexp (uir->sexp expected-uir))
  (define actual-sexp (uir->sexp actual))
  (check-equal? actual-sexp expected-sexp
                (format "lower ~s" input-str)))

(module+ main
  ;; null
  (check-lower "null" (uir-null))

  ;; booleans
  (check-lower "true" (uir-bool #t))
  (check-lower "false" (uir-bool #f))

  ;; number
  (check-lower "42" (uir-number "42"))
  (check-lower "-3.14" (uir-number "-3.14"))

  ;; string
  (check-lower "\"hello\"" (uir-string "hello"))

  ;; empty array
  (check-lower "[]" (uir-list '()))

  ;; single-element array
  (check-lower "[1]" (uir-list (list (uir-number "1"))))

  ;; multi-element array
  (check-lower "[1, true, null]"
               (uir-list (list (uir-number "1")
                               (uir-bool #t)
                               (uir-null))))

  ;; empty object
  (check-lower "{}" (uir-record '()))

  ;; single-pair object
  (check-lower "{\"a\": 1}"
               (uir-record (list (cons (uir-string "a") (uir-number "1")))))

  ;; multi-pair object
  (check-lower "{\"a\": 1, \"b\": true}"
               (uir-record (list (cons (uir-string "a") (uir-number "1"))
                                 (cons (uir-string "b") (uir-bool #t)))))

  ;; nested object
  (check-lower "{\"x\": {\"y\": 2}}"
               (uir-record
                (list (cons (uir-string "x")
                            (uir-record
                             (list (cons (uir-string "y") (uir-number "2"))))))))

  ;; nested array in object
  (check-lower "{\"a\": [1, 2]}"
               (uir-record
                (list (cons (uir-string "a")
                            (uir-list (list (uir-number "1") (uir-number "2")))))))

  ;; array of objects
  (check-lower "[{\"k\": \"v\"}, {}]"
               (uir-list
                (list (uir-record (list (cons (uir-string "k") (uir-string "v"))))
                      (uir-record '()))))

  (displayln "All lower-json tests passed."))
