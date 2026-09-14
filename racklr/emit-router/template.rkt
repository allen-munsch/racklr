#lang racket

(require racket/string)

(provide polyfill-classnames
         polyfill-date-fns
         html-page-template
         spa-router-template)

;; ── Static output artifacts for emit-router ─────────────────────────
;; These are the HTML page skeleton and the SPA client-side routing JS,
;; extracted from emit-router.rkt so they can be read and edited without
;; navigating the pipeline logic. Each function takes only the dynamic
;; pieces (page data, layout, nav links) as arguments and returns the
;; assembled output string.

;; ── B65: npm polyfills (static JS) ──────────────────────────────────

(define polyfill-classnames
  (string-join
   '("var cn = function() {"
     "  var classes = [];"
     "  for (var i = 0; i < arguments.length; i++) {"
     "    var arg = arguments[i];"
     "    if (!arg) continue;"
     "    var argType = typeof arg;"
     "    if (argType === 'string' || argType === 'number') {"
     "      classes.push(arg);"
     "    } else if (Array.isArray(arg)) {"
     "      classes.push(cn.apply(null, arg));"
     "    } else if (argType === 'object') {"
     "      for (var key in arg) {"
     "        if (Object.prototype.hasOwnProperty.call(arg, key) && arg[key]) {"
     "          classes.push(key);"
     "        }"
     "      }"
     "    }"
     "  }"
     "  return classes.join(' ');"
     "};")
   "\n"))

(define polyfill-date-fns
  "var format = function(d, fmt) { return String(d); };")

;; ── HTML page skeleton ──────────────────────────────────────────────

(define (html-page-template
         #:title title
         #:head-content head-content
         #:style-tags style-tags
         #:nav-links nav-links
         #:router router)
  (string-append
   "<!DOCTYPE html>\n"
   "<html lang=\"en\">\n"
   "<head>\n"
   "  <meta charset=\"UTF-8\">\n"
   "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
   (format "  <title>~a</title>\n" title)
   (if (string=? head-content "")
       ""
       (string-append head-content "\n"))
   (if (string=? style-tags "")
       ""
       (string-append style-tags "\n"))
   "</head>\n"
   "<body>\n"
   "  <nav style=\"padding: 1rem; border-bottom: 1px solid #ccc; margin-bottom: 1rem;\">\n"
   nav-links "\n"
   "  </nav>\n"
   "  <div id=\"_app\"></div>\n"
   "  <script>\n"
   router "\n"
   "  </script>\n"
   "</body>\n"
   "</html>\n"))

;; ── SPA client-side routing JS skeleton ─────────────────────────────

(define (spa-router-template
         #:layout-fn layout-fn
         #:page-data-str page-data-str
         #:pages-str pages-str
         #:server-data-str server-data-str
         #:dynamic-match-js dynamic-match-js)
  (string-join
   (list
    "// B65: npm polyfills"
    polyfill-classnames
    polyfill-date-fns
    ""
    (if layout-fn
        (format "var _layout = ~a;" layout-fn)
        "var _layout = null;")
    ""
    "var _pageData = {"
    page-data-str
    "};"
    ""
    "var _pages = {"
    pages-str
    "};"
    ""
    "var _serverData = {"
    server-data-str
    "};"
    ""
    dynamic-match-js
    ""
    "function _mount(path) {"
    "  var app = document.getElementById(\"_app\");"
    "  var _cs = window._cleanups || [];"
    "  for (var _i = 0; _i < _cs.length; _i++) _cs[_i]();"
    "  window._cleanups = [];"
    "  app.innerHTML = \"\";"
    "  window._currentPath = path;"
    "  window._rerender = function() { _mount(window._currentPath); };"
    "  var pageFn = _pages[path];"
    "  var params = null;"
    "  if (!pageFn) {"
    "    params = _matchDynamic(path);"
    "    if (params) {"
    "      pageFn = _pages[path] || Object.keys(_pages).find(function(k) {"
    "        var r = new RegExp('^' + k.replace(/:[^/]+/g, '([^/]+)') + '$');"
    "        return r.test(path);"
    "      });"
    "      pageFn = pageFn ? _pages[pageFn] : null;"
    "    }"
    "  }"
    "  if (pageFn) {"
    "    var pageData = _pageData[path];"
    "    var serverData = _serverData[path];"
    "    if (pageData || serverData) pageData = Object.assign({}, serverData || {}, pageData || {});"
    "    if (params) pageData = Object.assign({}, pageData || {}, params);"
    "    var el = pageFn(pageData);"
    "    if (_layout) {"
    "      var merged = Object.assign({}, pageData || {}, { children: el });"
    "      el = _layout(merged);"
    "    }"
    "    if (el) app.appendChild(el);"
    "  }"
    "}"
    ""
    "window.addEventListener(\"DOMContentLoaded\", function() {"
    "  _mount(window.location.hash.slice(1) || \"/\");"
    "});"
    ""
    "window.addEventListener(\"hashchange\", function() {"
    "  _mount(window.location.hash.slice(1) || \"/\");"
    "});")
   "\n"))
