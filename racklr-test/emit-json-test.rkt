#lang racket

(require rackunit
         racklr/tree
         racklr/uir
         racklr/lower-json
         racklr/emit-json
         racklr/gen-json-parser)

(define (test-emit-compact)
  (check-equal? (emit-json (uir-null)) "null")
  (check-equal? (emit-json (uir-bool #t)) "true")
  (check-equal? (emit-json (uir-bool #f)) "false")
  (check-equal? (emit-json (uir-number "42")) "42")
  (check-equal? (emit-json (uir-number "-3.14")) "-3.14")
  (check-equal? (emit-json (uir-string "hello")) "\"hello\"")
  (check-equal? (emit-json (uir-list '())) "[]")
  (check-equal? (emit-json (uir-list (list (uir-number "1")))) "[1]")
  (check-equal? (emit-json (uir-record '())) "{}"))

(define (test-emit-nested-compact)
  (define u1 (uir-record (list (cons (uir-string "a") (uir-number "1")))))
  (check-equal? (emit-json u1) "{\"a\": 1}")

  (define u2 (uir-record (list (cons (uir-string "x") (uir-bool #t))
                               (cons (uir-string "y") (uir-null)))))
  (check-equal? (emit-json u2) "{\"x\": true,\"y\": null}")

  (define u3 (uir-record
              (list (cons (uir-string "inner")
                          (uir-record (list (cons (uir-string "z")
                                                  (uir-number "0"))))))))
  (check-equal? (emit-json u3) "{\"inner\": {\"z\": 0}}")

  (define u4 (uir-list
              (list (uir-record (list (cons (uir-string "k") (uir-string "v"))))
                    (uir-record '()))))
  (check-equal? (emit-json u4) "[{\"k\": \"v\"},{}]"))

(define (test-emit-pretty)
  (define u1 (uir-record (list (cons (uir-string "a") (uir-number "1")))))
  (check-equal? (emit-json u1 #:indent 2)
                "{\n  \"a\": 1\n}")

  (define u2 (uir-record (list (cons (uir-string "a") (uir-number "1"))
                               (cons (uir-string "b") (uir-bool #t)))))
  (check-equal? (emit-json u2 #:indent 2)
                "{\n  \"a\": 1,\n  \"b\": true\n}"))

(define (test-roundtrip)
  (for ([json-str (in-list '("null" "true" "false" "42" "-3.14e10"
                              "\"hello\"" "[]" "[1, 2, 3]"))])
    (define cst1 (parse json-str))
    (define uir (lower-json cst1))
    (define emitted (emit-json uir))
    (define cst2 (parse emitted))
    (define uir2 (lower-json cst2))
    (check-equal? (uir->sexp uir2) (uir->sexp uir)
                  (format "round-trip for ~s, emitted: ~s" json-str emitted)))

  (define complex "{\"a\": 1, \"b\": [true, null], \"c\": {\"d\": \"x\"}}")
  (define cst1 (parse complex))
  (define uir (lower-json cst1))
  (define emitted (emit-json uir))
  (define cst2 (parse emitted))
  (define uir2 (lower-json cst2))
  (check-equal? (uir->sexp uir2) (uir->sexp uir)
                (format "complex round-trip: ~s" emitted)))

(module+ main
  (test-emit-compact)
  (test-emit-nested-compact)
  (test-emit-pretty)
  (test-roundtrip)
  (displayln "All emit-json tests passed."))
