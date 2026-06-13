#lang racket

(require "uir.rkt")

(provide emit-python)

;; ── Emit helpers ───────────────────────────────────────────────────

(define (emit-body uir indent)
  (cond [(uir-block? uir)
         (string-join (map (λ (s) (emit-stmt s indent)) (uir-block-stmts uir)) "\n")]
        [else (emit-stmt uir indent)]))

(define (emit-stmt uir indent)
  (define spc (make-string (* indent 4) #\space))
  (cond [(uir-null? uir) (string-append spc "pass")]
        [(uir-return? uir)
         (string-append spc "return " (emit-expr (uir-return-value uir)))]
        [(uir-set!? uir)
         (string-append spc (emit-expr (uir-set!-name uir)) " = " (emit-expr (uir-set!-value uir)))]
        [(uir-if? uir)
          ;; Emit as ternary if both branches are expressions (not blocks)
          (if (and (not (uir-block? (uir-if-then uir)))
                   (not (uir-block? (uir-if-else uir))))
              (string-append spc (emit-expr uir))
              (emit-if uir indent))]
        [(uir-for-each? uir)
         (emit-for-each uir indent)]
        [(uir-try? uir)
          (emit-try uir indent)]
        [(uir-with? uir)
          (emit-with uir indent)]
        [(uir-decorated? uir)
         (emit-decorated uir indent)]
        [(uir-fn? uir)
         (emit-funcdef uir indent)]
        [(uir-class? uir)
         (emit-classdef uir indent)]
        [(uir-import? uir)
         (emit-import uir spc)]
        [(uir-symbol? uir)
         (string-append spc (uir-symbol-name uir))]
        [(uir-var? uir)
         (string-append spc (uir-symbol-name (uir-var-name uir)))]
        [(uir-call? uir)
          (let ([callee (uir-call-callee uir)])
            (if (uir-symbol? callee)
                (let ([name (uir-symbol-name callee)])
                  (cond [(equal? name "del")
                         (string-append spc "del " (string-join (map emit-expr (uir-call-args uir)) ", "))]
                        [(equal? name "global")
                         (string-append spc "global " (string-join (map uir-symbol-name (uir-call-args uir)) ", "))]
                        [(equal? name "nonlocal")
                         (string-append spc "nonlocal " (string-join (map uir-symbol-name (uir-call-args uir)) ", "))]
                        [(equal? name "assert")
                         (let ([args (uir-call-args uir)])
                           (if (null? args)
                               (string-append spc "assert")
                               (string-append spc "assert " (emit-expr (car args))
                                              (if (>= (length args) 2)
                                                  (string-append ", " (emit-expr (cadr args)))
                                                  ""))))]
                        [(equal? name "raise")
                         (let ([args (uir-call-args uir)])
                           (if (null? args)
                               (string-append spc "raise")
                               (let ([expr-str (emit-expr (car args))])
                                 (if (>= (length args) 2)
                                     (string-append spc "raise " expr-str " from " (emit-expr (cadr args)))
                                     (string-append spc "raise " expr-str)))))]
                        [else (string-append spc (emit-expr uir))]))
                (string-append spc (emit-expr uir))))]
        [(uir-block? uir)
          (emit-body uir indent)]
        [(uir-yield? uir)
         (string-append spc (emit-yield uir))]
        [(uir-await? uir)
         (string-append spc (emit-await uir))]
        [(uir-list? uir)
         (string-append spc (emit-expr uir))]
        [(uir-record? uir)
          (string-append spc (emit-expr uir))]
        [(uir-get? uir)
          (string-append spc (emit-expr uir))]
        [(uir-string? uir)
          (string-append spc (emit-expr uir))]
        [(uir-fstring? uir)
          (string-append spc (emit-expr uir))]
        [else (format "~a# ?~a" spc (uir-tag uir))]))

(define (emit-if uir indent)
  (define spc (make-string (* indent 4) #\space))
  (string-append
   spc "if " (emit-expr (uir-if-test uir)) ":\n"
   (emit-body (uir-if-then uir) (+ indent 1))
   (if (uir-null? (uir-if-else uir))
       ""
       (string-append "\n" spc "else:\n" (emit-body (uir-if-else uir) (+ indent 1))))))

(define (emit-for-each uir indent)
  (define spc (make-string (* indent 4) #\space))
  (define var-str (uir-symbol-name (uir-for-each-var uir)))
  (define iter-str (emit-expr (uir-for-each-iterable uir)))
  (define body-str (emit-body (uir-for-each-body uir) (+ indent 1)))
  (define else-str
    (if (uir-null? (uir-for-each-else-body uir))
        ""
        (string-append "\n" spc "else:\n" (emit-body (uir-for-each-else-body uir) (+ indent 1)))))
  (string-append spc "for " var-str " in " iter-str ":\n" body-str else-str))

(define (emit-try uir indent)
  (define spc (make-string (* indent 4) #\space))
  (define body-str (emit-body (uir-try-body uir) (+ indent 1)))
  (define catches-str
    (string-join
     (map (lambda (cat)
            (define exc-type (car cat))
            (define exc-name (cadr cat))
            (define handler-body (caddr cat))
            (define exc-str
              (if (uir-null? exc-type)
                  ""
                  (string-append " " (emit-expr exc-type))))
            (define as-str
              (if (uir-null? exc-name)
                  ""
                  (string-append " as " (uir-symbol-name (uir-var-name exc-name)))))
            (string-append spc "except" exc-str as-str ":\n"
                           (emit-body handler-body (+ indent 1))))
          (uir-try-catches uir))
     "\n"))
  (define else-str
    (if (uir-try-else-body uir)
        (string-append "\n" spc "else:\n" (emit-body (uir-try-else-body uir) (+ indent 1)))
        ""))
  (define finally-str
    (if (uir-try-finally-body uir)
        (string-append "\n" spc "finally:\n" (emit-body (uir-try-finally-body uir) (+ indent 1)))
        ""))
  (string-append spc "try:\n" body-str
                 (if (string=? catches-str "") "" (string-append "\n" catches-str))
                 else-str
                 finally-str))

(define (emit-with uir indent)
  (define spc (make-string (* indent 4) #\space))
  (define items-str
    (string-join
     (for/list ([item (uir-with-items uir)])
       (define ctx-str (emit-expr (car item)))
       (if (cadr item)
           (string-append ctx-str " as " (uir-symbol-name (uir-var-name (cadr item))))
           ctx-str))
     ", "))
  (string-append spc "with " items-str ":\n"
                 (emit-body (uir-with-body uir) (+ indent 1))))

(define (emit-funcdef uir indent)
  (define spc (make-string (* indent 4) #\space))
  (define name (uir-fn-name uir))
  (define name-str (if name (uir-symbol-name name) "???"))
  (define params-str
    (string-join (map (λ (p) (if (uir-symbol? p) (uir-symbol-name p) (emit-expr p)))
                      (uir-fn-params uir))
                 ", "))
  (string-append
   spc "def " name-str "(" params-str "):\n"
   (emit-body (uir-fn-body uir) (+ indent 1))))

(define (emit-classdef uir indent)
  (define spc (make-string (* indent 4) #\space))
  (define name (uir-symbol-name (uir-class-name uir)))
  (string-append
   spc "class " name ":\n"
   (string-join
    (map (λ (m) (emit-method m name (+ indent 1))) (uir-class-methods uir))
    "\n")))

(define (emit-method uir class-name indent)
  (define spc (make-string (* indent 4) #\space))
  (define name (uir-symbol-name (uir-method-name uir)))
  (define params-str
    (string-join (map (λ (p) (if (uir-symbol? p) (uir-symbol-name p) (emit-expr p)))
                      (uir-method-params uir))
                 ", "))
  (string-append
   spc "def " name "(self" (if (string=? params-str "") "" (string-append ", " params-str)) "):\n"
   (emit-body (uir-method-body uir) (+ indent 1))))

(define (emit-import uir spc)
  (define source (uir-symbol-name (uir-import-source uir)))
  (define names (uir-import-names uir))
  (if (null? names)
      (string-append spc "import " source)
      (string-append spc "from " source " import "
                     (string-join (map uir-symbol-name names) ", "))))

;; ── Async and yield emitters ───────────────────────────────────────

(define (emit-yield uir)
  (define val-str (emit-expr (uir-yield-value uir)))
  (if (uir-yield-from? uir)
      (string-append "yield from " val-str)
      (string-append "yield " val-str)))

(define (emit-await uir)
  (string-append "await " (emit-expr (uir-await-expr uir))))

(define (emit-lambda uir)
  (define params-str
    (string-join (map (lambda (p) (if (uir-symbol? p) (uir-symbol-name p) (emit-expr p)))
                      (uir-fn-params uir))
                 ", "))
  (string-append "lambda " params-str ": " (emit-expr (uir-fn-body uir))))

;; Emit a comprehension: (uir-call (uir-symbol "TYPE-comp") (list result var iterable filter))
(define (emit-comp uir)
  (define callee-name (uir-symbol-name (uir-call-callee uir)))
  (define args (uir-call-args uir))
  (define result (if (>= (length args) 1) (car args) (uir-null)))
  (define loop-var (if (>= (length args) 2) (cadr args) (uir-null)))
  (define iterable (if (>= (length args) 3) (caddr args) (uir-null)))
  (define filter (if (>= (length args) 4) (cadddr args) (uir-null)))
  (define result-str (emit-expr result))
  (define var-str (if (uir-symbol? loop-var) (uir-symbol-name loop-var) (emit-expr loop-var)))
  (define iter-str (emit-expr iterable))
  (define body-str
    (string-append result-str " for " var-str " in " iter-str
                   (if (uir-null? filter) "" (string-append " if " (emit-expr filter)))))
  (cond [(equal? callee-name "list-comp") (string-append "[" body-str "]")]
        [(equal? callee-name "set-comp") (string-append "{" body-str "}")]
        [(equal? callee-name "dict-comp")
         ;; For dict comp, result-expr should be a key:value pair representation
         (string-append "{" body-str "}")]
        [else (string-append "[" body-str "]")]))

;; ── Decorated emitter ──────────────────────────────────────────────

(define (emit-decorated uir indent)
  (define spc (make-string (* indent 4) #\space))
  (define deco-lines
    (string-join
     (map (lambda (d)
            ;; d is a uir-call: emit as @name or @name(args)
            (if (null? (uir-call-args d))
                (string-append spc "@" (emit-expr (uir-call-callee d)))
                (string-append spc "@" (emit-expr d))))
          (uir-decorated-decorators uir))
     "\n"))
  (define inner-str
    (let ([inner (uir-decorated-inner uir)])
      (cond [(uir-fn? inner) (emit-funcdef inner indent)]
            [(uir-class? inner) (emit-classdef inner indent)]
            [else (emit-stmt inner indent)])))
  (string-append deco-lines "\n" inner-str))

;; ── Expression emitter ─────────────────────────────────────────────

(define (emit-expr uir)
  (cond [(uir-null? uir) "None"]
        [(uir-bool? uir) (if (uir-bool-value uir) "True" "False")]
        [(uir-number? uir) (uir-number-value uir)]
        [(uir-string? uir) (string-append "\"" (uir-string-value uir) "\"")]
        [(uir-fstring? uir) (string-append "f\"" (uir-fstring-value uir) "\"")]
        [(uir-list? uir)
         (string-append "[" (string-join (map emit-expr (uir-list-items uir)) ", ") "]")]
        [(uir-record? uir)
         (string-append "{" (string-join
                             (map (lambda (e)
                                    (string-append (emit-expr (car e)) ": " (emit-expr (cdr e))))
                                  (uir-record-entries uir))
                             ", ") "}")]
        [(uir-symbol? uir) (uir-symbol-name uir)]
        [(uir-var? uir) (uir-symbol-name (uir-var-name uir))]
        [(uir-call? uir)
          (let ([callee (uir-call-callee uir)])
            (if (and (uir-symbol? callee)
                     (regexp-match #rx"-comp$" (uir-symbol-name callee)))
                (emit-comp uir)
                (let ([args (uir-call-args uir)]
                      [name (and (uir-symbol? callee) (uir-symbol-name callee))])
                  (cond [(and name (equal? name "set"))
                         (string-append "{" (string-join (map emit-expr args) ", ") "}")]
                        [(and name (equal? name "tuple"))
                         (match (length args)
                           [0 "()"]
                           [1 (string-append "(" (emit-expr (first args)) ",)")]
                           [_ (string-append "(" (string-join (map emit-expr args) ", ") ")")])]
                        [(and name (= (length args) 2)
                              (member name '("is" "in" "<" ">" "==" "!=" ">=" "<="
                                             "+" "-" "*" "/" "//" "%" "**"
                                             "and" "or" "|" "^" "&" "<<" ">>")))
                         (string-append (emit-expr (first args)) " " name " " (emit-expr (second args)))]
                        [(and name (= (length args) 1) (equal? name "not"))
                         (string-append "not " (emit-expr (first args)))]
                        [else
                         (string-append (emit-expr callee)
                                        "(" (string-join (map emit-expr args) ", ") ")")]))))]
        [(uir-let? uir)
         (string-append "(" (uir-symbol-name (uir-let-name uir))
                        " := " (emit-expr (uir-let-value uir))
                        "; " (emit-expr (uir-let-body uir)) ")")]
        [(uir-block? uir)
         (string-join (map emit-expr (uir-block-stmts uir)) "\n")]
        [(uir-return? uir) (string-append "return " (emit-expr (uir-return-value uir)))]
        [(uir-if? uir)
          (string-append (emit-expr (uir-if-then uir))
                         " if " (emit-expr (uir-if-test uir))
                         " else " (emit-expr (uir-if-else uir)))]
        [(uir-fn? uir)
          (emit-lambda uir)]
        [(uir-await? uir) (emit-await uir)]
        [(uir-yield? uir) (emit-yield uir)]
        [(uir-get? uir)
         (let ([base-str (emit-expr (uir-get-base uir))]
               [field (uir-get-field uir)])
           (if (uir-string? field)
               (string-append base-str "." (uir-string-value field))
               (string-append base-str "[" (emit-expr field) "]")))]
        [else (format "<?~a>" (uir-tag uir))]))

;; ── Top-level ──────────────────────────────────────────────────────

(define (emit-python uir)
  (cond [(uir-block? uir)
         (emit-body uir 0)]
        [else (emit-stmt uir 0)]))

(module+ main
  (displayln "racklr/emit-python — UIR → Python3 text emitter loaded."))
