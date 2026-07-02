#lang racket

(require racklr/uir
         racklr/emit-javascript)

(provide emit-html)

;; ── UIR → HTML5 page emitter ─────────────────────────────────────────
;; Takes a UIR program and wraps the emitted JS in a self-contained HTML page.
;; Supports HTML DOM builder UIR as an alternative input path for the future.

(define (emit-html uir
                   #:title [title "Generated Page"]
                   #:lang  [lang "en"]
                   #:external-js [external-js #f])
  (define js (emit-javascript uir))
  (define script-tag
    (if external-js
        (format "<script type=\"module\" src=\"~a\"></script>" external-js)
        (format "<script type=\"module\">\n~a\n</script>" js)))
  (string-append
   "<!DOCTYPE html>\n"
   (format "<html lang=\"~a\">\n" lang)
   "<head>\n"
   "  <meta charset=\"UTF-8\">\n"
   "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
   (format "  <title>~a</title>\n" (html-escape title))
   "</head>\n"
   "<body>\n"
   "  " script-tag "\n"
   "</body>\n"
   "</html>\n"))

(define (html-escape s)
  (regexp-replace* #rx"<" 
    (regexp-replace* #rx">" 
      (regexp-replace* #rx"\"" 
        (regexp-replace* #rx"&" s "&amp;")
        "\\&quot;")
      "\\&gt;")
    "\\&lt;"))
