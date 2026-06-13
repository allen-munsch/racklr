#lang racket

(require "uir.rkt")

(provide emit-javascript)

;; ── UIR → JavaScript text emitter ────────────────────────────────────

(define infix-ops
  (set "+" "-" "*" "/" "%" "=" "==" "===" "!=" "!=="
       "<" ">" "<=" ">=" "&&" "||" "**"
       "+=" "-=" "*=" "/=" "**="))

(define postfix-ops (set "postfix++" "postfix--"))

(define prefix-ops (set "prefix++" "prefix--"))

(define (emit-javascript uir)
  (match uir
    [(uir-null) "null"]
    [(uir-bool v) (if v "true" "false")]
    [(uir-number v) v]
    [(uir-string v) (format "~s" v)]
    [(uir-symbol v) v]
    
    [(uir-list items)
     (format "[~a]" (string-join (map emit-javascript items) ", "))]

    [(uir-record entries)
     (format "{ ~a }"
             (string-join
              (for/list ([e entries])
                (define key-str (emit-key (car e)))
                (define val (cdr e))
                (cond
                  [(string=? key-str "...")
                   (emit-javascript val)]
                  [(and (uir-fn? val) (string-prefix? key-str "get "))
                   (format "get ~a() { ~a }"
                           (substring key-str 4)
                           (emit-body (uir-fn-body val)))]
                  [(and (uir-fn? val) (string-prefix? key-str "set "))
                   (format "set ~a(~a) { ~a }"
                           (substring key-str 4)
                           (string-join (map emit-javascript (uir-fn-params val)) ", ")
                           (emit-body (uir-fn-body val)))]
                  [(uir-fn? val)
                   (format "~a(~a) { ~a }"
                           key-str
                           (string-join (map emit-javascript (uir-fn-params val)) ", ")
                           (emit-body (uir-fn-body val)))]
                  [else
                   (format "~a: ~a" key-str (emit-javascript val))]))
              ", "))]
    
    [(uir-var name)
     (emit-javascript name)]
    
    [(uir-set! name val)
     (cond [(uir-fn? val)
            (format "function ~a(~a) { ~a }"
                    (emit-javascript name)
                    (string-join (map emit-javascript (uir-fn-params val)) ", ")
                    (emit-body (uir-fn-body val)))]
           [(and (uir-call? val) (uir-symbol? (uir-call-callee val))
                 (let ([op (uir-symbol-name (uir-call-callee val))])
                   (or (string=? op "async-fn") (string=? op "gen-fn"))))
            (define args (uir-call-args val))
            (define fn (first args))
            (define prefix (if (string=? (uir-symbol-name (uir-call-callee val)) "async-fn") "async function" "function*"))
            (format "~a ~a(~a) { ~a }"
                    prefix
                    (emit-javascript name)
                    (string-join (map emit-javascript (uir-fn-params fn)) ", ")
                    (emit-body (uir-fn-body fn)))]
           [else
            (format "~a = ~a" (emit-javascript name) (emit-javascript val))])]
    
    [(uir-fn _ params body)
     (define param-str
       (string-join (map emit-javascript params) ", "))
     (format "function(~a) { ~a }" param-str (emit-body body))]
    
    [(uir-call callee args)
     (define op-name (and (uir-symbol? callee) (uir-symbol-name callee)))
     (cond [(and op-name (string=? op-name "dot") (= (length args) 2))
            (format "~a.~a" (emit-javascript (first args)) (emit-javascript (second args)))]
           [(and op-name (string=? op-name "index") (= (length args) 2))
            (format "~a[~a]" (emit-javascript (first args)) (emit-javascript (second args)))]
           [(and op-name (string=? op-name "while") (= (length args) 2))
            (format "while (~a) { ~a }" (emit-javascript (first args)) (emit-body (second args)))]
            [(and op-name (string=? op-name "dowhile") (= (length args) 2))
             (format "do { ~a } while (~a)" (emit-body (first args)) (emit-javascript (second args)))]
            [(and op-name (string=? op-name "with") (= (length args) 2))
             (format "with (~a) { ~a }" (emit-javascript (first args)) (emit-body (second args)))]
           [(and op-name (string=? op-name "for") (= (length args) 4))
            (format "for (~a; ~a; ~a) { ~a }"
                    (if (uir-null? (first args)) "" (emit-javascript (first args)))
                    (if (uir-null? (second args)) "" (emit-javascript (second args)))
                    (if (uir-null? (third args)) "" (emit-javascript (third args)))
                    (emit-body (fourth args)))]
           [(and op-name (string=? op-name "forin") (= (length args) 3))
            (format "for (~a in ~a) { ~a }"
                    (emit-javascript (first args))
                    (emit-javascript (second args))
                    (emit-body (third args)))]
            [(and op-name (string=? op-name "forof") (= (length args) 3))
             (format "for (~a of ~a) { ~a }"
                     (emit-javascript (first args))
                     (emit-javascript (second args))
                     (emit-body (third args)))]
            [(and op-name (string=? op-name "switch") (= (length args) 3))
             (define test-str (emit-javascript (first args)))
             (define case-str
               (if (uir-null? (second args)) ""
                   (emit-switch-cases (second args))))
             (define default-str
               (if (uir-null? (third args)) ""
                   (format " default: ~a" (emit-switch-body (third args)))))
             (format "switch (~a) { ~a~a }" test-str case-str default-str)]
           [(and op-name (string=? op-name "break"))
            "break"]
            [(and op-name (string=? op-name "continue"))
             "continue"]
            [(and op-name (string=? op-name "debugger"))
             "debugger"]
            [(and op-name (string=? op-name "label") (= (length args) 2))
             (define label-name (emit-javascript (first args)))
             (define body-str (emit-javascript (second args)))
             (format "~a: ~a" label-name body-str)]
            [(and op-name (string=? op-name "regex") (= (length args) 1))
             (uir-string-value (first args))]
            [(and op-name (string=? op-name "function") (= (length args) 2))
             (define params-uir (first args))
             (define body-uir (second args))
             (define param-str
               (if (uir-list? params-uir)
                   (string-join (map emit-javascript (uir-list-items params-uir)) ", ")
                   ""))
             (format "function(~a) { ~a }" param-str (emit-body body-uir))]
            [(and op-name (string=? op-name "async-fn") (= (length args) 1))
             (define fn (first args))
             (format "async function(~a) { ~a }"
                     (string-join (map emit-javascript (uir-fn-params fn)) ", ")
                     (emit-body (uir-fn-body fn)))]
            [(and op-name (string=? op-name "gen-fn") (= (length args) 1))
             (define fn (first args))
             (format "function* (~a) { ~a }"
                     (string-join (map emit-javascript (uir-fn-params fn)) ", ")
                     (emit-body (uir-fn-body fn)))]
            [(and op-name (string=? op-name "import") (>= (length args) 1))
             (emit-import args)]
            [(and op-name (string=? op-name "export") (>= (length args) 1))
             (emit-export args)]
            [(and op-name (string=? op-name "spread") (= (length args) 1))
             (format "...~a" (emit-javascript (first args)))]
            [(and op-name (string=? op-name "rest") (= (length args) 1))
             (format "...~a" (emit-javascript (first args)))]
            [(and op-name (string=? op-name "throw") (= (length args) 1))
             (format "throw ~a" (emit-javascript (first args)))]
             [(and op-name (string=? op-name "try") (= (length args) 4))
              (define try-str (format "try { ~a }" (emit-body (first args))))
              (define catch-str
                (if (uir-null? (third args))
                    ""
                    (format " catch (~a) { ~a }"
                            (emit-javascript (second args))
                            (emit-body (third args)))))
              (define finally-str
                (if (uir-null? (fourth args))
                    ""
                    (format " finally { ~a }" (emit-body (fourth args)))))
              (string-append try-str catch-str finally-str)]
             [(and op-name (string=? op-name "=>") (= (length args) 2))
              (define params-uir (first args))
              (define body-expr (second args))
              (define param-str
                (if (uir-list? params-uir)
                    (string-join (map emit-javascript (uir-list-items params-uir)) ", ")
                    (emit-javascript params-uir)))
              (define body-str (emit-javascript body-expr))
              (if (uir-block? body-expr)
                  (format "(~a) => { ~a }" param-str (emit-body body-expr))
                  (format "(~a) => ~a" param-str body-str))]
            [(and op-name (set-member? (set "var" "let" "const") op-name) (= (length args) 1))
             (emit-declaration op-name (first args))]
            [(and op-name (= (length args) 1) (set-member? postfix-ops op-name))
            (format "~a~a" (emit-javascript (first args)) (substring op-name 7))]
           [(and op-name (= (length args) 1) (set-member? prefix-ops op-name))
            (format "~a~a" (substring op-name 6) (emit-javascript (first args)))]
           [(and op-name (= (length args) 1))
            (format "~a ~a" op-name (emit-javascript (first args)))]
           [(and op-name (set-member? infix-ops op-name) (= (length args) 2))
            (format "~a ~a ~a"
                    (emit-javascript (first args))
                    op-name
                    (emit-javascript (second args)))]
           [else
            (let ([fn-str (emit-javascript callee)]
                  [args-str (string-join (map emit-javascript args) ", ")])
              (format "~a(~a)" fn-str args-str))])]
    
    [(uir-if test conseq altern)
     (if (or (uir-block? conseq) (uir-block? altern))
         (format "if (~a) { ~a } else { ~a }"
                 (emit-javascript test)
                 (emit-body conseq)
                 (emit-body altern))
         (format "~a ? ~a : ~a"
                 (emit-javascript test)
                 (emit-javascript conseq)
                 (emit-javascript altern)))]
    
    [(uir-new class args)
     (format "new ~a(~a)"
             (emit-javascript class)
             (string-join (map emit-javascript args) ", "))]
    
    [(uir-class name super fields methods)
     (format "class ~a~a { }"
             (emit-javascript name)
             (if (uir-null? super) "" (format " extends ~a" (emit-javascript super))))]
    
    [(uir-block stmts)
     (string-join (map emit-statement stmts) " ")]
    
    [(uir-return val)
     (format "return ~a" (emit-javascript val))]
    
    [(uir-await expr)
     (format "await ~a" (emit-javascript expr))]
    
    [(uir-yield value from?)
     (format "~a ~a"
             (if from? "yield*" "yield")
             (emit-javascript value))]
    
    [(? uir? v)
     (format "/* unlowered: ~a */" (uir-tag v))]
    
    [else (format "~a" uir)]))

(define (emit-body node)
  (cond [(uir-null? node) ""]
        [(uir-block? node) (emit-javascript node)]
        [else (emit-javascript node)]))

(define (emit-statement node)
  (cond [(uir-null? node) "null;"]
        [(uir-block? node)
         (string-join (map emit-statement (uir-block-stmts node)) " ")]
        [(and (uir-set!? node)
               (or (uir-fn? (uir-set!-value node))
                   (and (uir-call? (uir-set!-value node))
                        (uir-symbol? (uir-call-callee (uir-set!-value node)))
                        (let ([op (uir-symbol-name (uir-call-callee (uir-set!-value node)))])
                          (or (string=? op "async-fn") (string=? op "gen-fn"))))))
          (emit-javascript node)]
        [(and (uir-if? node)
              (or (uir-block? (uir-if-then node))
                  (uir-block? (uir-if-else node))))
         (emit-javascript node)]
        [(loop-statement? node)
         (emit-javascript node)]
        [(try-statement? node)
         (emit-javascript node)]
        [(switch-statement? node)
         (emit-javascript node)]
        [(and (uir-call? node) (uir-symbol? (uir-call-callee node))
              (string=? (uir-symbol-name (uir-call-callee node)) "dowhile"))
         (format "~a;" (emit-javascript node))]
        [(declaration? node)
         (format "~a;" (emit-javascript node))]
        [(uir-var? node)
         (format "~a;" (emit-javascript node))]
        [else (format "~a;" (emit-javascript node))]))

(define (loop-statement? node)
  (and (uir-call? node)
       (uir-symbol? (uir-call-callee node))
       (let ([n (uir-symbol-name (uir-call-callee node))])
         (or (string=? n "while") (string=? n "for")
             (string=? n "forin") (string=? n "forof")))))

(define (try-statement? node)
  (and (uir-call? node)
       (uir-symbol? (uir-call-callee node))
       (string=? "try" (uir-symbol-name (uir-call-callee node)))))

(define (declaration? node)
  (and (uir-call? node)
       (uir-symbol? (uir-call-callee node))
       (let ([n (uir-symbol-name (uir-call-callee node))])
         (set-member? (set "var" "let" "const") n))))

(define (emit-declaration kind body)
  (cond [(uir-set!? body)
         (format "~a ~a = ~a" kind
                 (emit-javascript (uir-set!-name body))
                 (emit-javascript (uir-set!-value body)))]
        [(uir-var? body)
         (format "~a ~a" kind (emit-javascript (uir-var-name body)))]
        [else (format "~a ~a" kind (emit-javascript body))]))

(define (emit-switch-cases cases-block)
  (string-join
   (for/list ([stmt (uir-block-stmts cases-block)]
              #:when (and (uir-call? stmt)
                          (uir-symbol? (uir-call-callee stmt))
                          (string=? "case" (uir-symbol-name (uir-call-callee stmt)))))
     (match-define (list test body) (uir-call-args stmt))
     (format " case ~a: ~a" (emit-javascript test) (emit-switch-body body)))
   ""))

(define (emit-switch-body body)
  (if (uir-block? body)
      (string-join (map emit-statement (uir-block-stmts body)) " ")
      (emit-statement body)))

(define (switch-statement? node)
  (and (uir-call? node)
       (uir-symbol? (uir-call-callee node))
       (string=? "switch" (uir-symbol-name (uir-call-callee node)))))

(define (emit-key key)
  (if (uir-string? key)
      (uir-string-value key)
      (format "[~a]" (emit-javascript key))))

;; ── ES Modules emit ──────────────────────────────────────────────────

(define (emit-import args)
  (define source
    (let ([last-arg (last args)])
      (if (uir-string? last-arg)
          (format " from ~s" (uir-string-value last-arg))
          "")))
  (cond
    ;; import 'm';
    [(and (= (length args) 1) (uir-string? (first args)))
     (format "import ~s" (uir-string-value (first args)))]
    ;; import x from 'm';
    [(and (= (length args) 2) (uir-symbol? (first args)))
     (format "import ~a~a" (emit-javascript (first args)) source)]
    ;; import * as ns from 'm';
    [(and (= (length args) 2) (uir-list? (first args))
          (= (length (uir-list-items (first args))) 2))
     (define items (uir-list-items (first args)))
     (define ns-name (emit-javascript (second items)))
     (format "import * as ~a~a" ns-name source)]
    ;; import { x, y } from 'm';
    [(uir-record? (first args))
     (define entries (uir-record-entries (first args)))
     (define names
       (string-join
        (for/list ([e entries])
          (define export-name (uir-string-value (car e)))
          (define local-name (emit-javascript (cdr e)))
          (if (string=? export-name local-name)
              local-name
              (format "~a as ~a" export-name local-name)))
        ", "))
     (define src
       (if (and (= (length args) 2) (uir-string? (second args)))
           (format " from ~s" (uir-string-value (second args)))
           ""))
     (format "import { ~a }~a" names src)]
    [else (format "/* unlowered import */")]))

(define (emit-export args)
  (define first-arg (first args))
  (cond
    ;; export default expr
    [(and (uir-symbol? first-arg) (string=? (uir-symbol-name first-arg) "default"))
     (format "export default ~a" (emit-javascript (second args)))]
    ;; export const x = 1; / export function f() {} ...
    [(and (uir-symbol? first-arg) (string=? (uir-symbol-name first-arg) "decl"))
     (format "export ~a" (emit-javascript (second args)))]
    ;; export { x, y } or export { x, y } from 'm'
    [(uir-record? first-arg)
     (define entries (uir-record-entries first-arg))
     (define names
       (string-join
        (for/list ([e entries])
          (define export-name (uir-string-value (car e)))
          (define local-name (emit-javascript (cdr e)))
          (if (string=? export-name local-name)
              local-name
              (format "~a as ~a" local-name export-name)))
        ", "))
     (define src
       (if (and (= (length args) 2) (uir-string? (second args)))
           (format " from ~s" (uir-string-value (second args)))
           ""))
     (format "export { ~a }~a" names src)]
    [else (format "/* unlowered export */")]))

(module+ main
  (displayln "emit-javascript loaded."))
