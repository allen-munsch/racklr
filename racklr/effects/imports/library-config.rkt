#lang racket

;; racklr/effects/imports/library-config.rkt
;;
;; The library-specific effect configuration: which module paths carry which
;; effect domain. This is DATA, kept apart from the generic tagging machinery
;; so the racklr core carries no library-specific tokens. Add a library by
;; adding rules here — no code changes.

(require racklr/effects/imports/effect-tag-types)

(provide effect-tag-config)

;; Default configuration. Extend by adding a library here — no code changes.
(define effect-tag-config
  (list
   (effect-library "django"
                    (list (effect-rule "django.test.client" 'network)
                          (effect-rule "django.core.mail" 'email)
                          (effect-rule "django.core.cache" 'cache)
                          (effect-rule "django.core.management.base" 'io)
                          (effect-rule "django.core.files" 'io)
                          (effect-rule "django.http" 'io)
                          (effect-rule "django.db" 'db)))
   (effect-library "sqlparse"
                    (list (effect-rule "sqlparse.cli" 'io)
                          (effect-rule "sqlparse" 'pure)))
   (effect-library "tzdata" (list (effect-rule "tzdata" 'pure)))))
