#lang racket

;; ── emit-router — hub module ─────────────────────────────────────────
;; The factory (make-emit-pages-html) lives in emit-router/factory.rkt;
;; page discovery in emit-router/discovery.rkt. This module re-exports the
;; public API and keeps the auto-loading convenience wrapper, so external
;; callers keep requiring `racklr/emit-router` unchanged.

(require "emit-router/factory.rkt"
         "emit-router/discovery.rkt")

(provide emit-pages-html
         make-emit-pages-html
         discover-pages
         find-app-tsx)

;; ── Convenience: same API for callers that want auto-loading ──────────

(define emit-pages-html
  (let ([factory #f])
    (lambda args
      (unless factory
        (dynamic-require 'racklr/gen-test 'void) ;; ensure gen-test loaded
        (define g (dynamic-require 'racklr/gen-test 'gen-and-load))
        (define-values (p t tt tv)
          (g "grammars-v4/javascript/typescript-cleaned/TypeScriptParser.g4"))
        (define-values (jp jt jtt jtv)
          (g "grammars-v4/javascript/jsx-cleaned/JSXParser.g4"))
        (set! factory (make-emit-pages-html p t tt tv jp jt jtt jtv)))
      (apply factory args))))
