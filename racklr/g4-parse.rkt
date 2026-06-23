#lang racket

(require racklr/tree
         racklr/g4-lex)

(provide parse-g4 parse-g4-file)

;; ── Parser state ──────────────────────────────────────────────────

(struct parser-state (tokens pos input-str) #:transparent)

(define (make-parser toks input-str) (parser-state toks 0 input-str))

(define (current-tok ps)
  (define toks (parser-state-tokens ps))
  (define i (parser-state-pos ps))
  (if (< i (length toks))
      (list-ref toks i)
      (token 'eof "" (pos 0 0 0))))

(define (advance ps)
  (struct-copy parser-state ps (pos (+ (parser-state-pos ps) 1))))

(define (expect type ps)
  (define t (current-tok ps))
  (if (eq? (token-type t) type)
      (values (advance ps) t)
      (error 'parse "expected ~a, got ~a (~a) at ~a:~a"
             type (token-type t) (token-value t)
             (source-pos-line (token-pos t))
             (source-pos-col (token-pos t)))))

(define (expect/ps type ps)
  (define-values (next-ps dummy) (expect type ps))
  next-ps)

(define (expect/any types ps)
  (define t (current-tok ps))
  (if (member (token-type t) types)
      (values (advance ps) t)
      (error 'parse "expected one of ~a, got ~a (~a)" types (token-type t) (token-value t))))

(define (optional-semicolon ps)
  (if (eq? (token-type (current-tok ps)) 'semicolon) (advance ps) ps))

;; ── Grammar declaration ───────────────────────────────────────────

(define (parse-grammar-decl ps)
  (define t (current-tok ps))
  (define-values (grammar-type ps1)
    (cond
      ((eq? (token-type t) 'grammar) (values 'grammar (advance ps)))
       ((eq? (token-type t) 'lexer)
        (define ps2 (expect/ps 'grammar (advance ps)))
        (values 'lexer-grammar ps2))
      ((eq? (token-type t) 'parser)
        (define ps2 (expect/ps 'grammar (advance ps)))
        (values 'parser-grammar ps2))
      (else (error 'parse "expected grammar/lexer/parser declaration"))))
  (define-values (ps2 id-tok) (expect/any '(id token-id) ps1))
  (define ps3 (expect/ps 'semicolon ps2))
  (values ps3 grammar-type id-tok))

;; ── Options ───────────────────────────────────────────────────────

(define (parse-options ps)
  (define ps1 (expect/ps 'options ps))
  (define ps2 (expect/ps 'lbrace ps1))
  (let loop ((ps ps2) (opts (list)))
    (define t (current-tok ps))
    (cond
      ((eq? (token-type t) 'rbrace) (values (advance ps) (node 'options (reverse opts))))
      ((eq? (token-type t) 'id)
       (define name-tok t)
       (define ps3 (expect/ps 'assign (advance ps)))
       (define val-tok (current-tok ps3))
       (define-values (ps4 dummy) (expect/any '(id token-id) ps3))
       (define ps5 (expect/ps 'semicolon ps4))
       (loop ps5 (cons (node 'option
                             (list (leaf 'name (token-value name-tok)
                                         #:start (token-pos name-tok)
                                         #:end (token-pos name-tok))
                                   (leaf 'value (token-value val-tok)
                                         #:start (token-pos val-tok)
                                         #:end (token-pos val-tok))))
                       opts)))
      (else (error 'parse "expected option or }, got ~a" (token-type t))))))

;; ── Parser rules ──────────────────────────────────────────────────

(define (parse-parser-rule ps)
  (define name-tok (current-tok ps))
  (define ps1 (expect/ps 'id ps))
  (define ps2 (expect/ps 'colon ps1))
  (define-values (ps3 alts) (parse-alternatives ps2))
  (define ps4 (expect/ps 'semicolon ps3))
  (values ps4 (node 'parser-rule
                    (list (leaf 'name (token-value name-tok)
                                #:start (token-pos name-tok)
                                #:end (token-pos name-tok))
                          (node 'alternatives alts)))))

;; ── Lexer rules ───────────────────────────────────────────────────

(define (parse-lexer-rule ps (fragment? #f))
  (when fragment? (set! ps (advance ps)))
  (define name-tok (current-tok ps))
  (define ps1 (expect/ps 'token-id ps))
  (define ps2 (expect/ps 'colon ps1))
  (define-values (ps3 alts) (parse-alternatives ps2))
  (define-values (ps4 commands)
    (if (eq? (token-type (current-tok ps3)) 'arrow)
        (parse-lexer-commands (advance ps3))
        (values ps3 '())))
  (define ps5 (expect/ps 'semicolon ps4))
  (define children
    (list (leaf 'name (token-value name-tok)
                #:start (token-pos name-tok)
                #:end (token-pos name-tok))
          (node 'alternatives alts)))
  (define children2
    (if (null? commands) children (append children commands)))
  (values ps5 (node (if fragment? 'fragment-rule 'lexer-rule) children2)))

(define (parse-lexer-command ps)
  (define t (current-tok ps))
  (cond
    ((eq? (token-type t) 'skip)
     (values (advance ps) (leaf 'command "skip" #:start (token-pos t) #:end (token-pos t))))
    ((eq? (token-type t) 'more)
     (values (advance ps) (leaf 'command "more" #:start (token-pos t) #:end (token-pos t))))
    ((eq? (token-type t) 'channel)
      (define ps0 (expect/ps 'lparen (advance ps)))
      (define-values (ps1 ctok) (expect 'token-id ps0))
      (define ps2 (expect/ps 'rparen ps1))
      (values ps2 (leaf 'command (format "channel(~a)" (token-value ctok))
                        #:start (token-pos t) #:end (token-pos ctok))))
    ((eq? (token-type t) 'pushMode)
      (define ps0 (expect/ps 'lparen (advance ps)))
      (define-values (ps1 mtok) (expect 'token-id ps0))
      (define ps2 (expect/ps 'rparen ps1))
      (values ps2 (leaf 'command (format "pushMode(~a)" (token-value mtok))
                        #:start (token-pos t) #:end (token-pos mtok))))
    ((eq? (token-type t) 'popMode)
      (values (advance ps) (leaf 'command "popMode" #:start (token-pos t) #:end (token-pos t))))
    ((eq? (token-type t) 'type)
      (define ps0 (expect/ps 'lparen (advance ps)))
      (define-values (ps1 ttok) (expect 'token-id ps0))
      (define ps2 (expect/ps 'rparen ps1))
      (values ps2 (leaf 'command (format "type(~a)" (token-value ttok))
                        #:start (token-pos t) #:end (token-pos ttok))))
    (else (error 'parse "expected lexer command, got ~a" (token-type t)))))

(define (parse-lexer-commands ps)
  (define-values (ps1 cmd) (parse-lexer-command ps))
  (let loop ([ps ps1] [cmds (list cmd)])
    (if (eq? (token-type (current-tok ps)) 'comma)
        (let* ([ps2 (advance ps)]
               [ps3 (optional-semicolon ps2)])
          (define-values (nps next-cmd) (parse-lexer-command ps3))
          (loop nps (cons next-cmd cmds)))
        (values ps (reverse cmds)))))

;; ── Modes ─────────────────────────────────────────────────────────

(define (parse-mode ps)
  (define ps1 (expect/ps 'mode ps))
  (define name-tok (current-tok ps1))
  (define ps2 (expect/ps 'token-id ps1))
  (define ps3 (expect/ps 'semicolon ps2))
  (let loop ((ps ps3) (rules (list)))
    (define t (current-tok ps))
    (cond
      ((or (eq? (token-type t) 'mode) (eq? (token-type t) 'eof))
       (values ps (node 'mode
                        (cons (leaf 'name (token-value name-tok)
                                    #:start (token-pos name-tok)
                                    #:end (token-pos name-tok))
                              (reverse rules)))))
      ((eq? (token-type t) 'fragment)
       (define-values (nps rule) (parse-lexer-rule ps #t))
       (loop nps (cons rule rules)))
      ((eq? (token-type t) 'token-id)
       (define-values (nps rule) (parse-lexer-rule ps))
       (loop nps (cons rule rules)))
      (else
       (error 'parse "unexpected token ~a in mode" (token-type t))))))

;; ── Alternatives and elements ─────────────────────────────────────

(define (parse-alternatives ps)
  (define-values (ps1 alt) (parse-alternative ps))
  (let loop ((ps ps1) (alts (list alt)))
    (define t (current-tok ps))
    (if (eq? (token-type t) 'pipe)
        (let* ((ps1 (advance ps)))
           (define-values (ps2 next-alt) (parse-alternative ps1))
           (loop ps2 (cons next-alt alts)))
        (values ps (reverse alts)))))

(define (parse-alternative ps)
  (define-values (ps1 elements) (parse-rule-elements ps))
  ;; Handle optional #label for alternative labeling (ANTLR4)
  (define hash-tok (current-tok ps1))
  (define-values (ps2 label)
    (if (eq? (token-type hash-tok) 'hash)
        (let* ((ps-hash (advance ps1))
               (label-tok (current-tok ps-hash)))
          (if (member (token-type label-tok) '(id token-id))
              (values (advance ps-hash) (token-value label-tok))
              (values ps1 #f)))
        (values ps1 #f)))
  (values ps2 (if label
                  (node 'alternative (cons (leaf 'label label
                                                  #:start (token-pos hash-tok)
                                                  #:end (token-pos hash-tok))
                                           elements))
                  (node 'alternative elements))))

(define (parse-rule-elements ps)
  (let loop ((ps ps) (elems (list)))
    (define t (current-tok ps))
    (define tt (token-type t))
    (if (member tt '(semicolon pipe rparen eof arrow hash))
        (values ps (reverse elems))
        (let-values (((nps elem) (parse-element ps)))
          (loop nps (cons elem elems))))))

(define (parse-element ps)
  (define t (current-tok ps))
  (define tt (token-type t))

  (define-values (ps-base base-elem)
    (cond
      ;; Labeled element: id = element (must come before plain id)
      ;; Also handles append-label: id += element
      ((and (eq? tt 'id)
            (let ((nt (current-tok (advance ps))))
              (eq? (token-type nt) 'assign)))
       (let* ((ps-next (advance ps))
              (next-tok (current-tok ps-next))
              (label-name (token-value t))
              (ps2 (advance ps-next)))
         (let-values (((ps3 elem) (parse-element ps2)))
           (values ps3 (node 'labeled
                            (list (leaf 'label label-name
                                         #:start (token-pos t) #:end (token-pos t))
                                  elem))))))
      ;; Append-labeled element: id += element
      ((and (eq? tt 'id)
            (let ((ps1 (advance ps)))
              (and (eq? (token-type (current-tok ps1)) 'plus)
                   (let ((ps2 (advance ps1)))
                     (eq? (token-type (current-tok ps2)) 'assign)))))
       (let* ((ps-plus (advance ps))
              (ps-assign (advance ps-plus))
              (label-name (token-value t))
              (ps2 (advance ps-assign)))
         (let-values (((ps3 elem) (parse-element ps2)))
           (values ps3 (node 'append-labeled
                            (list (leaf 'label label-name
                                         #:start (token-pos t) #:end (token-pos t))
                                  elem))))))
      ((eq? tt 'string)
       (define ps1 (advance ps))
       ;; Check for range: 'a'..'z'
       (if (eq? (token-type (current-tok ps1)) 'range)
           (let* ((ps2 (advance ps1))
                  (end-tok (current-tok ps2)))
             (if (eq? (token-type end-tok) 'string)
                 (values (advance ps2)
                         (node 'range
                               (list (leaf 'literal (token-value t)
                                           #:start (token-pos t) #:end (token-pos t))
                                     (leaf 'literal (token-value end-tok)
                                           #:start (token-pos end-tok) #:end (token-pos end-tok)))))
                 (error 'parse "expected string after .., got ~a" (token-type end-tok))))
           (values ps1
                   (leaf 'literal (token-value t)
                         #:start (token-pos t) #:end (token-pos t)))))
      ((eq? tt 'char-class)
       (values (advance ps)
               (leaf 'char-class (token-value t)
                     #:start (token-pos t) #:end (token-pos t))))
      ((eq? tt 'token-id)
       (values (advance ps)
               (leaf 'token-ref (token-value t)
                     #:start (token-pos t) #:end (token-pos t))))
      ((eq? tt 'id)
       (values (advance ps)
               (leaf 'rule-ref (token-value t)
                     #:start (token-pos t) #:end (token-pos t))))
      ((eq? tt 'dot)
       (values (advance ps)
               (leaf 'any "." #:start (token-pos t) #:end (token-pos t))))
      ((eq? tt 'tilde)
       (define-values (ps1 next) (parse-element (advance ps)))
       (values ps1 (node 'negated (list next))))
      ((eq? tt 'lbrace)
        ;; Action block: capture content between { and }
        (define start-pos (source-pos-offset (token-pos t)))
        (define input (parser-state-input-str ps))
        (let skip ([ps (advance ps)] [depth 1])
          (define t2 (current-tok ps))
          (cond
            ((eq? (token-type t2) 'lbrace) (skip (advance ps) (+ depth 1)))
            ((eq? (token-type t2) 'rbrace)
             (if (= depth 1)
                 (let* ([end-pos (source-pos-offset (token-pos t2))]
                        [action-text (substring input (+ start-pos 1) end-pos)])
                   (values (advance ps)
                           (leaf 'action action-text
                                 #:start (token-pos t) #:end (token-pos t2))))
                 (skip (advance ps) (- depth 1))))
            ((eq? (token-type t2) 'eof) (error 'parse "unterminated action block"))
            (else (skip (advance ps) depth)))))
      ((eq? tt 'lparen)
       (define ps1 (expect/ps 'lparen ps))
       (define-values (ps2 alts) (parse-alternatives ps1))
       (define ps3 (expect/ps 'rparen ps2))
       (values ps3 (node 'group alts)))
      ;; Element options: <key = value> — skip them
      ((eq? tt 'langle)
       (let skip-opts ([ps (advance ps)])
         (define t2 (current-tok ps))
         (cond
           [(eq? (token-type t2) 'rangle) (parse-element (advance ps))]
           [(eq? (token-type t2) 'eof) (error 'parse "unterminated element options")]
           [else (skip-opts (advance ps))])))
      (else
       (error 'parse "unexpected token ~a (~a) in rule at ~a:~a"
              tt (token-value t)
              (source-pos-line (token-pos t))
              (source-pos-col (token-pos t))))))

  ;; Suffix operators (including non-greedy *? +? ??)
  (define suffix-t (current-tok ps-base))
  (define suffix-tt (token-type suffix-t))
  (cond
    ((eq? suffix-tt 'star)
     (define ps1 (advance ps-base))
     (if (eq? (token-type (current-tok ps1)) 'question)
         (values (advance ps1) (node 'star (list base-elem)))
         (values ps1 (node 'star (list base-elem)))))
    ((eq? suffix-tt 'plus)
     (define ps1 (advance ps-base))
     (if (eq? (token-type (current-tok ps1)) 'question)
         (values (advance ps1) (node 'plus (list base-elem)))
         (values ps1 (node 'plus (list base-elem)))))
    ((eq? suffix-tt 'question)
     (define ps1 (advance ps-base))
     (if (eq? (token-type (current-tok ps1)) 'question)
         (values (advance ps1) (node 'optional (list base-elem)))
         (values ps1 (node 'optional (list base-elem)))))
    (else (values ps-base base-elem))))

;; ── Import declarations ───────────────────────────────────────────

(define (parse-import-decl ps)
  ;; 'import' GrammarName ';'
  (define ps1 (expect/ps 'import ps))
  (define name-tok (current-tok ps1))
  (define-values (ps2 id-tok) (expect/any '(id token-id) ps1))
  (define ps3 (expect/ps 'semicolon ps2))
  (values ps3 (leaf 'import (token-value id-tok)
                    #:start (token-pos name-tok)
                    #:end (token-pos id-tok))))

;; ── Top-level grammar parser ──────────────────────────────────────

;; Helper: skip a brace-balanced block, used for tokens { ... } and options { ... }
(define (skip-brace-block ps)
  (let loop ((ps ps) (depth 1))
    (define t (current-tok ps))
    (cond
      ((eq? (token-type t) 'lbrace) (loop (advance ps) (+ depth 1)))
      ((eq? (token-type t) 'rbrace)
       (if (= depth 1)
           (advance ps)
           (loop (advance ps) (- depth 1))))
      ((eq? (token-type t) 'eof) (error 'parse "unterminated block"))
      (else (loop (advance ps) depth)))))

(define (parse-grammar-file ps)
  (define-values (ps0 grammar-type name-tok) (parse-grammar-decl ps))

  (define-values (ps1 options-node)
    (if (eq? (token-type (current-tok ps0)) 'options)
        (parse-options ps0)
        (values ps0 #f)))

  ;; Start rules list with options-node if present
  (let loop ((ps ps1) (rules (if options-node (list options-node) '())))
    (define t (current-tok ps))
    (cond
      ((eq? (token-type t) 'eof) (values ps (reverse rules)))
      ((eq? (token-type t) 'fragment)
       (define-values (nps rule) (parse-lexer-rule ps #t))
       (loop nps (cons rule rules)))
      ((eq? (token-type t) 'token-id)
       (define-values (nps rule) (parse-lexer-rule ps))
       (loop nps (cons rule rules)))
      ((eq? (token-type t) 'mode)
       (define-values (nps rule) (parse-mode ps))
       (loop nps (cons rule rules)))
      ((eq? (token-type t) 'import)
        (define-values (nps import-node) (parse-import-decl ps))
        (loop nps (cons import-node rules)))
      ((and (eq? (token-type t) 'id) (equal? (token-value t) "tokens"))
        ;; Skip `tokens { ... }` block (implicit token declarations)
        (define ps0 (expect/ps 'id ps))
        (define ps1 (skip-brace-block (advance ps0)))
        (loop ps1 rules))
      ((eq? (token-type t) 'options)
        ;; Skip subsequent options blocks (e.g., lexer options in combined grammar)
        (define-values (ps0 _) (parse-options ps))
        (loop ps0 rules))
      ((eq? (token-type t) 'id)
       (define-values (nps rule) (parse-parser-rule ps))
       (loop nps (cons rule rules)))
      (else
       (error 'parse "unexpected token ~a (~a) in grammar at ~a:~a"
              (token-type t) (token-value t)
              (source-pos-line (token-pos t))
              (source-pos-col (token-pos t)))))))

;; ── Public API ────────────────────────────────────────────────────

(define (parse-g4 input-str)
  (define tokens (g4-lex input-str))
  (define ps (make-parser tokens input-str))
  (define gtype
    (match (token-type (first tokens))
      ('grammar 'grammar)
      ('lexer 'lexer-grammar)
      ('parser 'parser-grammar)
      (_ 'unknown)))
  (define gname
    (match tokens
      ((list _ (and (? token?) t) _ ...)
       (if (member (token-type t) '(id token-id))
           (token-value t)
           "unknown"))
      (_ "unknown")))
  (define-values (ps-final rules) (parse-grammar-file ps))
  (node gtype
        (list (leaf 'name gname #:start (pos 0 0 0) #:end (pos 0 0 0))
              (node 'rules rules))))

(define (parse-g4-file path)
  (parse-g4 (file->string path)))

(module+ main
  (displayln "racklr/g4-parse — ANTLR4 grammar parser loaded."))
