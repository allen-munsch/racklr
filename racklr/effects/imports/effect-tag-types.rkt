#lang racket

;; racklr/effects/imports/effect-tag-types.rkt
;;
;; The effect-capability data model: a structured capability (domain . ops),
;; a module-path → effect-domain rule, and a library (a named rule set). These
;; structs are split into a leaf module so the library-specific config can live
;; in its own module without a require cycle.

(provide
  effect-cap? effect-cap effect-cap-domain effect-cap-ops
  effect-rule? effect-rule effect-rule-prefix effect-rule-domain
  effect-library? effect-library effect-library-name effect-library-rules)

;; A structured capability: effect root + primitive-operation set.
(struct effect-cap (domain ops) #:transparent)   ; domain: symbol, ops: (listof symbol)

;; One rule: module-path-prefix → effect domain, or 'pure (no effect).
(struct effect-rule (prefix domain) #:transparent)  ; domain: symbol | 'pure

;; A library: a name (top-level package) and its ordered rules. Rules are
;; ordered most-specific-first; the first prefix match wins.
(struct effect-library (name rules) #:transparent)
