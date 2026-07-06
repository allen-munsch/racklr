#lang racket

(require racket/string
         rackunit
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/tsx-preprocess
         racklr/emit-javascript
         racklr/lower-jsx
         racklr/esbuild-resolve
         (prefix-in ts-lower: racklr/lower-typescript))

;; ── TSX → HTML + Vanilla JS integration test ──────────────────────

;; Load parsers
(define-values (ts-parse ts-tokenize ts-tok-type ts-tok-value)
  (gen-and-load "../grammars-v4/javascript/typescript-cleaned/TypeScriptParser.g4"))

(define-values (jsx-parse jsx-tokenize jsx-tok-type jsx-tok-value)
  (gen-and-load "../grammars-v4/javascript/jsx-cleaned/JSXParser.g4"))

(define (tsx->uir source)
  (define hookless (preprocess-hooks source))
  (define-values (processed jsx-map jsx-uir)
    (preprocess-tsx hookless
                    #:jsx-parse jsx-parse
                    #:jsx-lower-tk-type jsx-tok-type
                    #:jsx-lower-tk-value jsx-tok-value))
  
  (define ts-cst (ts-parse processed))
  (define ts-uir (ts-lower:lower-program ts-cst ts-tok-type ts-tok-value))
  (restore-jsx ts-uir jsx-uir))

(define (tsx->js source)
  (define uir (tsx->uir source))
  (emit-javascript uir))

(define (tsx->html source #:title [title "TSX App"])
  (define js (tsx->js source))
  (string-append
   "<!DOCTYPE html>\n"
   "<html lang=\"en\">\n"
   "<head>\n"
   "  <meta charset=\"UTF-8\">\n"
   "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
   (format "  <title>~a</title>\n" title)
   "</head>\n"
   "<body>\n"
   "  <script>\n"
   js "\n"
   "  </script>\n"
   "</body>\n"
   "</html>\n"))

;; ── Multi-file support ─────────────────────────────────────────────

(define (tsx-app->js files #:entry [entry "app.tsx"])
  (define bundled (resolve-imports files #:entry entry))
  (tsx->js bundled))

;; ── Tests ──────────────────────────────────────────────────────────

(define test1 "const el = <div>hello</div>;")
(define result1 (tsx->js test1))
(printf "Test 1 result: ~a\n" result1)
(check-true (string-contains? result1 "document.createElement"))
(check-true (string-contains? result1 "createTextNode"))

(define test2 "function App() { return <div className=\"app\"><h1>Title</h1></div>; }")
(define result2 (tsx->js test2))
(printf "Test 2 result: ~a\n" result2)
(check-true (string-contains? result2 "document.createElement(\"div\")"))
(check-true (string-contains? result2 "setAttribute(\"className\",\"app\")"))
(check-true (string-contains? result2 "document.createElement(\"h1\")"))

(define test3
  "const app = <div class=\"container\"><p>Hello, world!</p></div>;")
(define result3 (tsx->html test3 #:title "My App"))
(printf "Test 3 result:\n~a\n" result3)
(check-true (string-contains? result3 "<!DOCTYPE html>"))
(check-true (string-contains? result3 "<title>My App</title>"))
(check-true (string-contains? result3 "createTextNode(\"Hello, world!\")"))

;; ── B7+B8: Component calls + props ──────────────────────────────────
;; A component (uppercase tag) should emit as a function call, not document.createElement.
;; Props should become an object literal argument, not setAttribute.

(define test4 "const Header = ({title}) => <h1>{title}</h1>;
const app = <Header title=\"My App\" />;")
(define result4 (tsx->js test4))
(printf "Test 4 result: ~a\n" result4)
(check-true (string-contains? result4 "Header({"))
(check-true (string-contains? result4 "title: \"My App\""))
(check-false (string-contains? result4 "document.createElement(\"Header\")"))
(check-false (string-contains? result4 "setAttribute"))

;; ── B9: Expression children ─────────────────────────────────────────
;; Expression children like {title} must be wrapped in createTextNode.
;; NB: Test 4 already shows Header's <h1>{title}</h1> emits _el.appendChild(title)
;;     without createTextNode — that's the bug.
(check-true (string-contains? result4 "createTextNode(title)"))
(check-false (string-contains? result4 "_el.appendChild(title)"))

;; ── B10: React hooks (useState, useEffect) ──────────────────────────
;; useState destructuring → closure-based state
;; useEffect → direct callback call
;; React imports are stripped

(define test5
  "import { useState } from \"react\";
const [count, setCount] = useState(0);
const msg = <div>{count}</div>;")
(define result5 (tsx->js test5))
(printf "Test 5 result: ~a\n" result5)
(check-false (string-contains? result5 "import"))
(check-false (string-contains? result5 "react"))
(check-false (string-contains? result5 "useState"))
(check-true (string-contains? result5 "let count = 0"))
(check-true (string-contains? result5 "let setCount = function"))
(check-true (string-contains? result5 "document.createElement(\"div\")"))
(check-true (string-contains? result5 "createTextNode(count)"))

;; ── B11: Event handlers (onClick → addEventListener) ────────────────
;; React-style event attributes become addEventListener for HTML elements.

(define test6 "<button onClick={() => alert('hi')}>Click</button>")
(define result6 (tsx->js test6))
(printf "Test 6 result: ~a\n" result6)
(check-true (string-contains? result6 "addEventListener(\"click\""))
(check-false (string-contains? result6 "setAttribute(\"onClick\")"))
(check-true (string-contains? result6 "createTextNode(\"Click\")"))

;; ── B12: Conditional rendering ──────────────────────────────────────
;; {cond && <JSX/>} → cond ? <emission> : null
;; {cond ? <A/> : <B/>} → cond ? A_emission : B_emission

(define test-b12a
  "const show = true;
const el = <div>{show && <span>hello</span>}</div>;")
(define result-b12a (tsx->js test-b12a))
(printf "B12a result: ~a\n" result-b12a)
(check-true (string-contains? result-b12a "document.createElement(\"div\")"))
(check-true (string-contains? result-b12a "document.createElement(\"span\")"))
(check-true (string-contains? result-b12a "createTextNode(\"hello\")"))
(check-true (string-contains? result-b12a "? (function()"))
(check-not-false (string-contains? result-b12a ": null"))
(check-false (string-contains? result-b12a "createTextNode(show"))
(check-false (string-contains? result-b12a "createTextNode(show ?"))

(define test-b12b
  "const cond = true;
const Page = () => <div>{cond ? <h1>Yes</h1> : <h2>No</h2>}</div>;")
(define result-b12b (tsx->js test-b12b))
(printf "B12b result: ~a\n" result-b12b)
(check-true (string-contains? result-b12b "document.createElement(\"div\")"))
(check-true (string-contains? result-b12b "document.createElement(\"h1\")"))
(check-true (string-contains? result-b12b "document.createElement(\"h2\")"))
(check-true (string-contains? result-b12b "createTextNode(\"Yes\")"))
(check-true (string-contains? result-b12b "createTextNode(\"No\")"))
(check-true (string-contains? result-b12b "? (function()"))
(check-true (string-contains? result-b12b ": (function()"))
;; No createTextNode wrapping the complex ternaries
(check-false (string-contains? result-b12b "createTextNode(cond"))

;; ── B13: Style objects ──────────────────────────────────────────────

(define test7 "<div style={{color: 'red'}}>Red text</div>")
(define result7 (tsx->js test7))
(printf "Test 7 result: ~a\n" result7)
(check-true (string-contains? result7 "Object.assign(_el.style"))
(check-false (string-contains? result7 "setAttribute(\"style\""))

;; ── Multi-file tests ───────────────────────────────────────────────

(define multi-files
  (hash "app.tsx"  "import { Button } from \"./components/Button\";
const App = () => <div><Button /></div>;
export { App };"
        "components/Button.tsx"  "export const Button = () => <button>Click</button>;"))

(define multi-result (tsx-app->js multi-files #:entry "app.tsx"))
(printf "Multi-file test result:\n~a\n" multi-result)
(check-true (string-contains? multi-result "document.createElement(\"div\")"))
(check-true (string-contains? multi-result "document.createElement(\"button\")"))
(check-true (string-contains? multi-result "createTextNode(\"Click\")"))

;; ── B14: Multi-page routing ────────────────────────────────────

(require racklr/emit-router)

(define emit-pages
  (make-emit-pages-html ts-parse ts-tokenize ts-tok-type ts-tok-value
                        jsx-parse jsx-tokenize jsx-tok-type jsx-tok-value))

(define pages-b14
  (hash "/"       "export default () => <h1>Home</h1>;"
        "/about"  "export default () => <p>About page content</p>;"))

(define pages-b14-html (emit-pages pages-b14 #:title "B14 Test"))
(printf "B14 HTML (~a chars)\n" (string-length pages-b14-html))
(check-true (string-contains? pages-b14-html "<div id=\"_app\">"))
(check-true (string-contains? pages-b14-html "var _pages = {"))
(check-true (string-contains? pages-b14-html "\"/\":"))
(check-true (string-contains? pages-b14-html "\"/about\":"))
(check-true (string-contains? pages-b14-html "function _mount"))
(check-true (string-contains? pages-b14-html "hashchange"))
(check-true (string-contains? pages-b14-html "document.getElementById(\"_app\")"))
(check-true (string-contains? pages-b14-html "createElement(\"h1\")"))
(check-true (string-contains? pages-b14-html "createElement(\"p\")"))
;; No createTextNode wrapping the page function value
(check-false (string-contains? pages-b14-html "createTextNode(Home"))

;; ── B14b: Two self-contained pages with navigation ──────────────

(define pages-b14b
  (hash "/"       "export default () => <div><h1>Home</h1><a href=\"#/about\">About</a></div>;"
        "/about"  "export default () => <div><h1>About</h1><a href=\"#/\">Home</a></div>;"))

(define pages-b14b-html
  (emit-pages pages-b14b #:title "B14b Test"))

(printf "B14b HTML (~a chars)\n" (string-length pages-b14b-html))
(check-true (string-contains? pages-b14b-html "createElement(\"h1\")"))
(check-true (string-contains? pages-b14b-html "createTextNode(\"Home\")"))
(check-true (string-contains? pages-b14b-html "createTextNode(\"About\")"))
(check-true (string-contains? pages-b14b-html "setAttribute(\"href\""))
(check-true (string-contains? pages-b14b-html "\"#/about\""))
(check-true (string-contains? pages-b14b-html "\"#/\""))

;; ── B15: SSR / data fetching ──────────────────────────────────

(define pages-b15
  (hash "/" "export async function getStaticProps() { return { props: { title: \"Home\" } }; }
export default () => <h1>Home Page</h1>;"
        "/about" "export function getServerSideProps() { return { props: {} }; }
export default () => <p>About page</p>;"))

(define pages-b15-html (emit-pages pages-b15 #:title "B15 Test"))
(printf "B15 HTML (~a chars)\n" (string-length pages-b15-html))
;; getStaticProps and getServerSideProps must be stripped
(check-false (string-contains? pages-b15-html "getStaticProps"))
(check-false (string-contains? pages-b15-html "getServerSideProps"))
;; But the page components still render
(check-true (string-contains? pages-b15-html "createElement(\"h1\")"))
(check-true (string-contains? pages-b15-html "createElement(\"p\")"))
(check-true (string-contains? pages-b15-html "createTextNode(\"Home Page\")"))
(check-true (string-contains? pages-b15-html "createTextNode(\"About page\")"))

;; ── B17: getStaticProps data inlining ───────────────────────────

(define pages-b17
  (hash "/" "export async function getStaticProps() { return { props: { title: \"Home\" } }; }
export default (props) => <h1>{props.title}</h1>;"
        "/about" "export function getStaticProps() { return { props: { heading: \"About Us\" } }; }
export default (props) => <p>{props.heading}</p>;"))

(define pages-b17-html (emit-pages pages-b17 #:title "B17 Test"))
(printf "B17 HTML (~a chars)\n" (string-length pages-b17-html))
;; getStaticProps must be stripped from output
(check-false (string-contains? pages-b17-html "getStaticProps"))
;; _pageData must be present with the extracted props
(check-true (string-contains? pages-b17-html "var _pageData"))
(check-true (string-contains? pages-b17-html "title: \"Home\""))
(check-true (string-contains? pages-b17-html "heading: \"About Us\""))
;; pageFn must be called with pageData
(check-true (string-contains? pages-b17-html "pageFn(pageData)"))
;; The page component references props.title as a variable
(check-true (string-contains? pages-b17-html "createTextNode(props.title)"))
(check-true (string-contains? pages-b17-html "createTextNode(props.heading)"))
;; Page components still render DOM elements
(check-true (string-contains? pages-b17-html "createElement(\"h1\")"))
(check-true (string-contains? pages-b17-html "createElement(\"p\")"))

;; B18 helper alias
(define check-string-contains? (lambda (s sub) (check-true (string-contains? s sub))))

;; ── B18: Pages directory discovery ─────────────────────────────────

;; Test discover-pages with a temp directory
(define b18-tmp-dir (make-temporary-file "racklr-b18-~a" 'directory))
(display-to-file "export default () => <h1>Index Page</h1>;" (build-path b18-tmp-dir "index.tsx"))
(display-to-file "export default () => <p>About</p>;" (build-path b18-tmp-dir "about.tsx"))
;; Non-.tsx files should be ignored
(display-to-file "ignored" (build-path b18-tmp-dir "readme.md"))
(display-to-file "ignored" (build-path b18-tmp-dir ".hidden.tsx"))

(define b18-discovered (discover-pages b18-tmp-dir))
(check-equal? (hash-count b18-discovered) 2 "should discover exactly 2 pages")
(check-true (hash-has-key? b18-discovered "/") "should map index.tsx to /")
(check-true (hash-has-key? b18-discovered "/about") "should map about.tsx to /about")
(check-false (hash-has-key? b18-discovered "/readme") "should ignore .md files")
(check-false (hash-has-key? b18-discovered "/.hidden") "should ignore dotfiles")
(check-string-contains? (hash-ref b18-discovered "/") "<h1>Index Page</h1>")
(check-string-contains? (hash-ref b18-discovered "/about") "<p>About</p>")

;; End-to-end: discover + emit
(define b18-html (emit-pages b18-discovered #:title "B18 Test"))
(check-true (string-contains? b18-html "var _pages = {"))
(check-true (string-contains? b18-html "\"/\":"))
(check-true (string-contains? b18-html "\"/about\":"))

;; Cleanup
(delete-file (build-path b18-tmp-dir "index.tsx"))
(delete-file (build-path b18-tmp-dir "about.tsx"))
(delete-file (build-path b18-tmp-dir "readme.md"))
(delete-file (build-path b18-tmp-dir ".hidden.tsx"))
(delete-directory b18-tmp-dir)

;; ── B19: Shared layout wrapper ─────────────────────────────────────

(define b19-pages
  (hash "/" "export default () => <h1>Home Page</h1>;"
        "/about" "export default () => <p>About page content</p>;"))

(define b19-layout
  "export default ({ children, title }) => <div class=\"layout\"><header>Site</header><main>{children}</main><footer>Foot</footer></div>;")

(define b19-html-no-layout (emit-pages b19-pages #:title "No Layout"))
(check-true (string-contains? b19-html-no-layout "createElement(\"h1\")"))
(check-false (string-contains? b19-html-no-layout "Site"))

(define b19-html (emit-pages b19-pages #:title "With Layout" #:layout b19-layout))
;; Layout wrapper creates a div.layout, header, main, footer
(check-true (string-contains? b19-html "createElement(\"div\")"))
(check-true (string-contains? b19-html "createTextNode(\"Site\")"))
(check-true (string-contains? b19-html "createTextNode(\"Foot\")"))
;; _layout function must be present
(check-true (string-contains? b19-html "var _layout = "))
(check-false (string-contains? b19-html "var _layout = null"))
;; Page content still rendered
(check-true (string-contains? b19-html "createTextNode(\"Home Page\")"))
(check-true (string-contains? b19-html "createTextNode(\"About page content\")"))
;; _mount uses layout wrapping
(check-true (string-contains? b19-html "Object.assign({}, pageData || {}, { children: el })"))
