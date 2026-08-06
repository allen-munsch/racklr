# Plan: Rust Lowering & Raising Pipeline

## Overview

Build a complete Rust pipeline: `lower-rust.rkt` (Rust CST → UIR) and `emit-rust.rkt` (UIR → Rust source), with tests. Enables cross-compilation (Python → UIR → Rust) and Rust round-tripping.

## Current State

- **Parser**: Working. `gen-rust-test.rkt` verifies tokenizer + parser on ~20 tokenizer tests and 13 parser tests.
- **Grammar**: `RustParser.g4` (1198 lines, ~100 rules), `RustLexer.g4` (262 lines). Entry rule: `crate → innerAttribute* item* EOF`.
- **Lowering**: None. `lower-rust.rkt` does not exist.
- **Emission**: None. `emit-rust.rkt` does not exist.
- **UIR**: 47 struct types (Tier 0–2). Well-matched to Python/TS. Gaps for Rust: no `uir-enum`, no `uir-impl`.

## UIR Gaps

Rust needs two new UIR structs:

- **`uir-enum`** — `(struct uir-enum (name variants) #:transparent)` where variants is `(listof uir-enum-variant)`
- **`uir-enum-variant`** — `(struct uir-enum-variant (name fields discriminant) #:transparent)` — fields as `(listof uir-typed-param)`, discriminant as `#f` or uir?
- **Maybe `uir-impl`** — separate from uir-class since Rust decouples struct definition from method implementation. OR: fold impl methods into uir-class during lowering, then split back during emission. **Recommendation**: fold into uir-class (simpler, avoids new UIR struct; the struct + its impls become one uir-class during lowering).

## Scope: What We Handle (Pass 1)

These features cover the common 90% of real Rust code:

- Functions (`fn`), typed params, return types
- `let` bindings with type annotations
- Expressions: literals, variables, calls, binary/unary ops, field access, indexing
- Control flow: `if`/`else`, `while`, `loop`, `for`, `match`
- `struct` (named-field, tuple, unit)
- `enum` (variants, with data)
- `impl` blocks (methods)
- `trait` → `uir-interface`
- `mod` → `uir-module`
- `use` → `uir-import`
- `return`, `break`, `continue`
- Closures (basic: `|x| x + 1`)
- Blocks (`{ stmt; expr }`)
- `pub` visibility on items
- `self` parameter in methods

## Scope: What We Skip (Pass 1)

- Macros (`macro_rules!`, `macro!()`)
- Lifetimes (`'a`)
- Generics (parse but drop type params)
- Async/await
- Unsafe blocks
- Raw pointers, references (`&T`, `*const T`)
- Attributes (`#[...]`)
- Trait bounds, where clauses
- `extern` blocks / FFI
- `const`/`static` items (lower as uir-ann-set! with `const`/`static` naming)
- `type` aliases (lower as a call to `uir-symbol "type-alias"`)
- `union`
- Range expressions (`..`, `..=`)
- Tuple indexing (`x.0`)
- Struct update syntax (`Foo { ..bar }`)
- If-let, while-let (complex pattern matching in control flow)

---

## Pre-Reorganization: Module Split Plan (B16)

**Goal**: No source file exceeds 400 lines. Use nested directories per language/module.

**Principle**: Entry-point file stays flat (e.g., `lower-python.rkt`) for backward compat — `(require racklr/lower-python)` still works. Implementation splits into a same-named subdirectory (e.g., `lower-python/expr.rkt`). The entry point re-requires and re-provides everything.

**Directory layout after split**:

```
racklr/
  uir.rkt                       # dispatcher (~320 lines)
  uir/
    types.rkt                    # all struct defs (~300 lines)
  
  lower-python.rkt              # dispatcher (~80 lines)
  lower-python/
    helpers.rkt                  # shared CST-walking utilities (~40 lines)
    expr.rkt                     # expression lowering (~400 lines)
    stmt.rkt                     # statement lowering (~300 lines)
    compound.rkt                 # compound statement lowering (~290 lines)
    pattern.rkt                  # pattern lowering (~270 lines)
    item.rkt                     # function/class/item lowering (~360 lines)
  
  lower-typescript.rkt          # dispatcher (~80 lines)
  lower-typescript/
    helpers.rkt                  (~30 lines)
    expr.rkt                     (~400 lines)
    stmt.rkt                     (~300 lines)
    compound.rkt                 (~200 lines)
    pattern.rkt                  (~100 lines)
    item.rkt                     (~350 lines)
  
  lower-javascript.rkt          # dispatcher (~50 lines)
  lower-javascript/
    helpers.rkt                  (~30 lines)
    expr.rkt                     (~380 lines)
    stmt.rkt                     (~300 lines)
    compound.rkt                 (~150 lines)
    item.rkt                     (~150 lines)
  
  emit-python.rkt               # dispatcher (~270 lines)
  emit-python/
    expr.rkt                     # expression emission (~200 lines)
  
  emit-javascript.rkt           # dispatcher (~270 lines)
  emit-javascript/
    expr.rkt                     # expression emission (~200 lines)
  
  gend-parser.rkt               # dispatcher (~190 lines)
  gend-parser/
    lexer.rkt                    # lexer generation (~340 lines)
    grammar.rkt                  # parser rule generation (~390 lines)
  
  g4-parse.rkt                  # dispatcher (~250 lines)
  g4-parse/
    walk.rkt                     # grammar CST walking (~200 lines)
  
  # Rust (new, starts in B17 — uses nested structure from day 1)
  lower-rust.rkt                # dispatcher (~60 lines)
  lower-rust/
    helpers.rkt                  (~40 lines)
    expr.rkt                     (~350 lines)
    stmt.rkt                     (~200 lines)
    item.rkt                     (~350 lines)
    pattern.rkt                  (~150 lines)
    type.rkt                     (~150 lines)
  
  emit-rust.rkt                 # dispatcher (~40 lines)
  emit-rust/
    expr.rkt                     (~250 lines)
    stmt.rkt                     (~350 lines)
```

### Detailed Split Plans

#### 1. `uir.rkt` (619 lines) → `uir/types.rkt` + trim `uir.rkt`
- `uir/types.rkt`: All struct definitions (uir-null through uir-component, plus new uir-enum/uir-enum-variant)
- `uir.rkt`: `uir?`, `uir-tag`, `uir-tag-set`, `uir->sexp`, `sexp->uir`, `uir-proc`, `uir-proc?`, `provide` + `require "uir/types.rkt"`

#### 2. `lower-python.rkt` (1744 lines) → `lower-python/` directory (5 files + 1 helpers)
- **`lower-python/helpers.rkt`**: `node-children`, `token-like?`, `cst-tokens`, `cst-tokens-deep`, `first-token`
- **`lower-python/expr.rkt`**: `lower-expr`, `lower-atom-expr`, `lower-trailer`, `lower-atom`, `lower-name`, `lower-string-token`, `lower-number-token`, `lower-binop`, `lower-comparison`, `lower-not-test`, `lower-collection-items`, `lower-dictorsetmaker`, `lower-tuple-items`, `lower-comp-for`, `lower-comprehension`, `lower-group-expr`, `lower-arglist`, `lower-argument`, `lower-subscriptlist`
- **`lower-python/stmt.rkt`**: `lower-stmt`, `lower-simple-stmts`, `lower-simple-stmt`, `lower-return`, `lower-expr-stmt`, `lower-import-name`, `lower-import-from`, `lower-assert`, `lower-raise`, `lower-del`, `lower-global`, `lower-nonlocal`, `lower-yield`, `lower-async`, `lower-exprlist`, `lower-dotted-name`
- **`lower-python/compound.rkt`**: `lower-compound`, `lower-if`, `lower-if-rest`, `lower-while`, `lower-for`, `lower-match`, `lower-case-block`, `lower-try`, `lower-except-clause`, `lower-with`, `lower-suite`, `lower-block`, `lower-block-or-stmt`
- **`lower-python/pattern.rkt`**: `lower-patterns`, `lower-pattern`, `lower-or-pattern`, `lower-closed-pattern`, `lower-literal-pattern`, `lower-capture-pattern`, `lower-value-pattern`, `lower-sequence-pattern`, `lower-maybe-sequence-pattern`, `lower-maybe-star-pattern`, `lower-star-pattern`, `lower-mapping-pattern`, `lower-items-pattern`, `lower-class-pattern`, `lower-keyword-patterns`, `lower-keyword-pattern`, `lower-group-pattern`, `lower-as-pattern`, `lower-open-sequence`
- **`lower-python/item.rkt`**: `lower-funcdef`, `lower-parameters`, `lower-tfpdef`, `lower-lambdef`, `lower-varargslist`, `lower-classdef`, `lower-method`, `lower-decorated`, `lower-decorator`, `lower-get-path`
- **`lower-python.rkt`**: trimmed to dispatcher — requires all `"lower-python/*.rkt"` files, re-provides

**lower-python.rkt** becomes:
```racket
#lang racket
(require "lower-python/helpers.rkt"
         "lower-python/expr.rkt"
         "lower-python/stmt.rkt"
         "lower-python/compound.rkt"
         "lower-python/pattern.rkt"
         "lower-python/item.rkt")
(provide lower-python lower-expr lower-stmt lower-compound lower-funcdef lower-classdef
         lower-return lower-import-name lower-import-from lower-match lower-patterns
         lower-pattern lower-block lower-block-or-stmt)
```

**lower-python/expr.rkt** requires:
```racket
(require racklr/uir "helpers.rkt" "../lower-python/helpers.rkt")
```

#### 3. `lower-typescript.rkt` (1564 lines) → `lower-typescript/` (same pattern as Python)
5 specialization files + 1 helpers file. Same function groupings.

#### 4. `lower-javascript.rkt` (1058 lines) → `lower-javascript/` (4 files + helpers)
expr, stmt, compound, item — no pattern file needed for JS.

#### 5. `emit-python.rkt` (470 lines) → `emit-python/expr.rkt` + trim `emit-python.rkt`
- `emit-python/expr.rkt`: `emit-expr`, `emit-match`, `emit-case`, `emit-pattern`, `emit-pat-literal`, `emit-pat-seq-elements`, `emit-pat-mapping`, `emit-pat-class`, `emit-lambda`, `emit-comp`, `emit-yield`, `emit-await`
- `emit-python.rkt`: `emit-python`, `emit-body`, `emit-stmt`, `emit-if`, `emit-for-each`, `emit-while`, `emit-try`, `emit-with`, `emit-import`, `emit-funcdef`, `emit-classdef`, `emit-method`, `emit-decorated` + require/provide

#### 6. `emit-javascript.rkt` (467 lines) → `emit-javascript/expr.rkt` + trim

#### 7. `gend-parser.rkt` (913 lines) → `gend-parser/lexer.rkt` + `gend-parser/grammar.rkt` + trim
- `gend-parser/lexer.rkt`: `gen-lexer-matchers`, `gen-one-lexer-matcher`, `gen-match-helpers`, `gen-match-alt`, `gen-match-seq`, `gen-match-seq-fn`, `gen-match-elem`, all `m*` helpers, `g4-char->racket`, `gen-tokenizer`, `gen-mode-section`, `gen-token-clause`, `gen-command-body`, `build-mode-map`, `gen-lexer-entry`
- `gend-parser/grammar.rkt`: `extract-option`, `load-lexer-grammar-rules`, `lexer-has-newline-rule?`, `collect-parser-literals`, `build-implicit-lexer-rules`, `detect-left-recursion`, `gen-parser-rules`, `gen-one-parser-rule`, `gen-leftrec-rule`, `gen-leftrec-clause`, `lrec-elem-expr`, `elem-expr`, `gen-parser-alt`, `gen-parser-seq`, `gen-parser-single`, `gen-parser-provides`, `gen-entry`, `gen-parser-helpers`, `ctok`, `expect-tok`, `expect-lit`, `parse-star`, `parse-plus`, `parse-opt`, `parse-group`, `child-range`, `child-start`, `child-end`
- `gend-parser.rkt`: `generate-parser-module`, `gen-header`, `gen-indent-helpers`, `compute-indent`, `paren-open?`, `paren-close?`, `split-newline-value`, `insert-indents`, `mangle`, `unescape-char-class`, `unescape-g4-literal`, `parse`, `gen-parser-exports` + require/provide

#### 8. `g4-parse.rkt` (454 lines) → `g4-parse/walk.rkt` + trim

### Test Files

| File | Lines | Action |
|------|-------|--------|
| `lower-python-test.rkt` | 602 | Leave as-is for now |
| `uir-test.rkt` | 565 | Leave as-is for now |
| `lower-javascript-test.rkt` | 516 | Leave as-is for now |

### Reorganization Implementation Order

1. Split `uir.rkt` → `uir/types.rkt` + trim `uir.rkt` (everyone depends on it, do first)
2. Split `lower-python.rkt` → `lower-python/` (largest, sets the pattern)
3. Split `emit-python.rkt` → `emit-python/`
4. Split `lower-typescript.rkt` → `lower-typescript/`
5. Split `lower-javascript.rkt` → `lower-javascript/`
6. Split `emit-javascript.rkt` → `emit-javascript/`
7. Split `gend-parser.rkt` → `gend-parser/`
8. Split `g4-parse.rkt` → `g4-parse/`

**Verification after each split**: `raco test racklr-test/*.rkt` — all ~1345 tests must pass.

---

## Beads (Implementation Order)

### B16 — Module reorganization + uir-enum structs

**Why**: Foundation. Clean nested module structure before adding Rust pipeline. uir-enum structs needed for Rust.

**Files**:
- Create `racklr/uir/types.rkt` (all struct defs + new `uir-enum`/`uir-enum-variant`), trim `racklr/uir.rkt`
- Create `racklr/lower-python/` (5 files + helpers), trim `racklr/lower-python.rkt`
- Create `racklr/emit-python/expr.rkt`, trim `racklr/emit-python.rkt`
- Create `racklr/lower-typescript/` (5 files + helpers), trim `racklr/lower-typescript.rkt`
- Create `racklr/lower-javascript/` (4 files + helpers), trim `racklr/lower-javascript.rkt`
- Create `racklr/emit-javascript/expr.rkt`, trim `racklr/emit-javascript.rkt`
- Create `racklr/gend-parser/` (lexer + grammar), trim `racklr/gend-parser.rkt`
- Create `racklr/g4-parse/walk.rkt`, trim `racklr/g4-parse.rkt`

**Tasks**:

#### Task 1: Add `uir-enum` + `uir-enum-variant` to `uir/types.rkt`
```racket
;; Enum definition
(struct uir-enum (name variants) #:transparent)
;; Enum variant with optional fields and discriminant
(struct uir-enum-variant (name fields discriminant) #:transparent)
;; fields: (listof uir-typed-param), discriminant: #f or uir?
```
- Add to `uir?`, `uir-tag`, `uir->sexp`, `sexp->uir`, `uir-tag-set`
- Add round-trip test in `uir-test.rkt`

#### Task 2: Split `uir.rkt` → `uir/types.rkt` + trim
#### Task 3: Split `lower-python.rkt` → `lower-python/` (5+1 files)
#### Task 4: Split `emit-python.rkt` → `emit-python/` (1 file + trim)
#### Task 5: Split `lower-typescript.rkt` → `lower-typescript/` (5+1 files)
#### Task 6: Split `lower-javascript.rkt` → `lower-javascript/` (4+1 files)
#### Task 7: Split `emit-javascript.rkt` → `emit-javascript/` (1 file + trim)
#### Task 8: Split `gend-parser.rkt` → `gend-parser/` (2 files + trim)
#### Task 9: Split `g4-parse.rkt` → `g4-parse/walk.rkt` (1 file + trim)

**Verification**: `raco test racklr-test/*.rkt` — all existing tests pass.

---

### B17 — Rust lowering: crate-level items (functions, structs, enums)

**Why**: Top-level items are the entry point. Must be lowered before expressions inside them.

**Files**: Create `racklr/lower-rust.rkt`, `racklr/lower-rust/helpers.rkt`, `racklr/lower-rust/item.rkt`. Create `racklr-test/lower-rust-test.rkt`.

**Depends on**: B16 (uir-enum structs in uir/types.rkt)

**Uses nested structure from day 1** — `lower-rust/item.rkt` for items, `lower-rust.rkt` as dispatcher.

**Tasks**:

#### Task 1: Create `lower-rust.rkt` module skeleton
```racket
#lang racket
(require racklr/tree
         racklr/uir)

(provide lower-rust)

;; ── CST-walking helpers ──
;; (same helpers as lower-python.rkt: node-children, token-like?, cst-tokens, cst-tokens-deep, first-token)
```

#### Task 2: Create `lower-rust-test.rkt` skeleton
- Load Rust parser via `gen-and-load`
- Define `check-rs-lower` helper (parse → lower → check UIR shape)
- Define `check-rs-roundtrip` helper (parse → lower → emit → verify)
- Start with empty crate test

```racket
;; Empty crate
(let ([cst (rs-parse "")])
  (define uir (lower-rust cst rs-tok-type rs-tok-value))
  (check-true (uir-block? uir))
  (check-equal? (length (uir-block-stmts uir)) 0))
```

#### Task 3: Lower `function_` CST node
Rust grammar: `function_ : functionQualifiers KW_FN identifier genericParams? '(' functionParameters? ')' functionReturnType? whereClause? (blockExpression | SEMI)`

CST children after parsing will be flattened — need to inspect actual structure. Likely children: identifier, parameters (optional), return-type-group (optional), blockExpression.

```racket
(define (lower-function cst tk-type tk-value)
  (define toks (cst-tokens-deep cst tk-type tk-value))
  (define name (for/or ([t toks] #:when (eq? (first t) 'NON_KEYWORD_IDENTIFIER)) (second t)))
  (define kids (node-children cst))
  ;; Find params-node (tag: functionParameters or similar)
  ;; Find block-node (tag: blockExpression)
  ;; Find return-type (group with RARROW token)
  (define params ...)
  (define body (if block-node (lower-block block-node tk-type tk-value) (uir-null)))
  (define return-type #f) ; or lowered type expression
  (uir-fn (if name (uir-symbol name) #f) params body return-type))
```

#### Task 4: Lower `functionParameters` and `functionParam`
- `selfParam` → uir-typed-param named "self" with no type (or `Self` type)
- `functionParamPattern` → `pattern ':' type_` → uir-typed-param
- `type_` alone (unnamed param) → uir-typed-param with generated name `_pN`

```racket
(define (lower-function-param cst tk-type tk-value)
  ;; Tag will be functionParam or selfParam
  ;; selfParam: shorthandSelf (just 'self') or typedSelf ('self: Type')
  ;; functionParam: functionParamPattern (pattern ':' type) or bare type
  ...)
```

#### Task 5: Lower `struct_` CST node
- `structStruct` → uir-class (named-field struct)
- `tupleStruct` → uir-class with positional fields named `_0`, `_1`, etc.
- Unit struct → uir-class with empty fields

```racket
(define (lower-struct cst tk-type tk-value)
  ;; Tag: struct_
  ;; Kids: identifier, optional genericParams, optional whereClause, 
  ;;       LCURLYBRACE ... RCURLYBRACE (named) or LPAREN ... RPAREN (tuple) or SEMI (unit)
  ;; Detect struct type by looking at raw children for delimiter tokens
  ...
  (uir-class (uir-symbol name) (uir-null) fields '()))
```

#### Task 6: Lower `enumeration` CST node
- Produces `uir-enum` with `uir-enum-variant` items
- Variants can be unit (`Red`), tuple (`Red(i32, i32)`), struct (`Red { x: i32 }`), or with discriminant (`Red = 5`)

#### Task 7: Lower `module` CST node
- `mod name;` (declared, body elsewhere) vs `mod name { items }` (inline)
- Inline: lower to uir-module, body is uir-block of lowered items

#### Task 8: Wire top-level dispatch: `lower-rust`
```racket
(define (lower-rust cst tk-type tk-value)
  ;; Entry: crate
  (define tag (any-tree-tag cst))
  (cond [(eq? tag 'crate)
         (define kids (node-children cst))
         ;; items are list-wrapped from the * in `innerAttribute* item*`
         (define stmts ...)
         (uir-block stmts)]
        [else (uir-symbol (format "?top-~a" tag))]))
```

**Verification**: 
- `fn foo() {}` → UIR with uir-fn named "foo", empty params, uir-null body
- `fn add(x: i32, y: i32) -> i32 { x + y }` → uir-fn with 2 typed-params, return-type
- `struct Foo { x: i32 }` → uir-class with 1 field
- `enum Color { Red, Green }` → uir-enum with 2 variants
- `mod m { fn f() {} }` → uir-module containing uir-fn

---

### B18 — Rust lowering: statements and expressions

**Why**: Functions need bodies. Bodies contain statements and expressions.

**Files**: Create `racklr/lower-rust/expr.rkt`, `racklr/lower-rust/stmt.rkt`, `racklr/lower-rust/pattern.rkt`. Extend `racklr/lower-rust.rkt`.

**Depends on**: B17

**Tasks**:

#### Task 1: Lower `blockExpression` (statements + optional trailing expression)
- `LCURLYBRACE innerAttribute* statements? RCURLYBRACE`
- `statements` → `statement+ expression? | expression`
- Each `statement` lowered to a stmt; trailing expression is the block's value
- Produce `uir-block` containing all statements; if there's a trailing expression, add it as uir-return? No — blocks are expressions in Rust. Return the last expression's value implicitly. **Decision**: produce `uir-block` wrapping all statements. Treat trailing expression as an implicit return — append `(uir-return trailing-expr)` as final stmt.

#### Task 2: Lower `letStatement`
- `KW_LET patternNoTopAlt (':' type_)? ('=' expression)? ';'`
- `let x = 1;` → `(uir-set! (uir-var (uir-symbol "x")) (uir-number "1"))`
- `let x: i32 = 1;` → `(uir-ann-set! (uir-var (uir-symbol "x")) (uir-symbol "i32") (uir-number "1"))`
- `let x: i32;` → `(uir-ann-set! (uir-var (uir-symbol "x")) (uir-symbol "i32") #f)`
- `let (a, b) = ...;` → destructuring (skip for Pass 1, lower as ?pattern)

#### Task 3: Lower `expression` rule (the big one — ANTLR4 labeled alternatives)
Rust expression rule uses `# LabelName` alternatives. Generated parser produces CST tags like:
- `# LiteralExpression_` → `'LiteralExpression_`
- `# PathExpression_` → `'PathExpression_`
- `# CallExpression` → `'CallExpression`
- `# MethodCallExpression` → `'MethodCallExpression`
- `# FieldExpression` → `'FieldExpression`
- `# IndexExpression` → `'IndexExpression`
- `# ArithmeticOrLogicalExpression` → `'ArithmeticOrLogicalExpression`
- `# ComparisonExpression` → `'ComparisonExpression`
- `# LazyBooleanExpression` → `'LazyBooleanExpression`
- `# AssignmentExpression` → `'AssignmentExpression`
- `# CompoundAssignmentExpression` → `'CompoundAssignmentExpression`
- `# NegationExpression` → `'NegationExpression` (unary - and !)
- `# DereferenceExpression` → `'DereferenceExpression` (unary *)
- `# BorrowExpression` → `'BorrowExpression` (& and &&)
- `# TypeCastExpression` → `'TypeCastExpression`
- `# RangeExpression` → `'RangeExpression`
- `# ReturnExpression` → `'ReturnExpression`
- `# BreakExpression` → `'BreakExpression`
- `# ContinueExpression` → `'ContinueExpression`
- `# GroupedExpression` → `'GroupedExpression`
- `# ArrayExpression` → `'ArrayExpression`
- `# TupleExpression` → `'TupleExpression`
- `# StructExpression_` → `'StructExpression_`
- `# ClosureExpression_` → `'ClosureExpression_`
- `# ExpressionWithBlock_` → `'ExpressionWithBlock_`
- `# ErrorPropagationExpression` → `'ErrorPropagationExpression`
- `# AwaitExpression` → `'AwaitExpression`
- `# AttributedExpression` → `'AttributedExpression` (skip for Pass 1)

Implementation: single `lower-expr` function that dispatches on the label tag. For binary ops, each operand + operator comes as siblings in the CST — need to walk and reconstruct left-associative chains.

```racket
(define (lower-expr cst tk-type tk-value)
  (define tag (any-tree-tag cst))
  (match tag
    ['LiteralExpression_ (lower-literal cst tk-type tk-value)]
    ['PathExpression_ (lower-path-expr cst tk-type tk-value)]
    ['CallExpression (lower-call-expr cst tk-type tk-value)]
    ['MethodCallExpression (lower-method-call cst tk-type tk-value)]
    ['FieldExpression (lower-field-expr cst tk-type tk-value)]
    ['IndexExpression (lower-index-expr cst tk-type tk-value)]
    ['ArithmeticOrLogicalExpression (lower-arithmetic cst tk-type tk-value)]
    ['ComparisonExpression (lower-comparison cst tk-type tk-value)]
    ['LazyBooleanExpression (lower-lazy-boolean cst tk-type tk-value)]
    ['AssignmentExpression (lower-assignment cst tk-type tk-value)]
    ['NegationExpression (lower-negation cst tk-type tk-value)]
    ['ReturnExpression (lower-return-expr cst tk-type tk-value)]
    ['GroupedExpression (uir-paren (lower-expr (first (node-children cst)) tk-type tk-value))]
    ['ArrayExpression (lower-array cst tk-type tk-value)]
    ['TupleExpression (lower-tuple cst tk-type tk-value)]
    ['StructExpression_ (lower-struct-expr cst tk-type tk-value)]
    ['ClosureExpression_ (lower-closure cst tk-type tk-value)]
    ['ExpressionWithBlock_ (lower-expr-with-block cst tk-type tk-value)]
    ;; ... etc
    [_ (uir-symbol (format "?expr-~a" tag))]))
```

#### Task 4: Lower literals
- `CHAR_LITERAL`, `STRING_LITERAL`, `RAW_STRING_LITERAL` → uir-string
- `INTEGER_LITERAL`, `FLOAT_LITERAL` → uir-number
- `KW_TRUE`, `KW_FALSE` → uir-bool
- `BYTE_LITERAL`, `BYTE_STRING_LITERAL`, `RAW_BYTE_STRING_LITERAL` → uir-string (with b-prefix noted)

#### Task 5: Lower path expressions (identifiers and qualified paths)
- `pathInExpression`: `foo::bar::Baz` → uir-symbol or uir-var
- For simple paths (one segment): `foo` → uir-var (uir-symbol "foo")
- For multi-segment: `std::collections::HashMap` → `uir-get` chain or `uir-symbol "std::collections::HashMap"`. **Decision**: use uir-symbol with "::" separator preserved (simplest for round-trip).

#### Task 6: Lower call expressions
- `expression '(' callParams? ')'` → uir-call
- `expression '.' pathExprSegment '(' callParams? ')'` → method call: `uir-call (uir-get base (uir-string method-name)) args`

#### Task 7: Lower binary operations
- Arithmetic, comparison, boolean — same pattern as Python: find operator token, fold left-associative
- Operators: `+ - * / % & | ^ << >> == != < > <= >= && ||`
- Produce `(uir-call (uir-symbol "+") (list left right))`

#### Task 8: Lower assignment
- `expression '=' expression` → `(uir-set! lhs rhs)` where lhs is a uir-var or uir-get

#### Task 9: Lower return/break/continue
- `return expr?` → `(uir-return expr-or-null)`
- `break` → `(uir-call (uir-symbol "break") '())`
- `continue` → `(uir-call (uir-symbol "continue") '())`

#### Task 10: Lower expressionStatement
- `expression ';'` → lowered expression (semicolon is a statement separator, not part of expression)
- `expressionWithBlock ';'?` → lowered block expression (if/loop/match, no semicolon needed)

#### Task 11: Lower control flow expressions
- `ifExpression`: `if cond { then } else { else_ }` → uir-if
- `loopExpression`: `loop { body }` → `(uir-while (uir-bool #t) body (uir-null))` — infinite loop
- `predicateLoopExpression`: `while cond { body }` → uir-while
- `iteratorLoopExpression`: `for pattern in expr { body }` → uir-for-each
- `matchExpression` → uir-match (with uir-case, patterns)

#### Task 12: Lower closures
- `|x, y| expr` or `|x, y| { stmts }` → uir-fn with `#f` name, params from closure params, body

**Verification**: Tests for each expression type lowering to correct UIR shape.

---

### B19 — Rust lowering: types, visibility, imports

**Why**: Type annotations, visibility modifiers, and use declarations are pervasive in Rust.

**Files**: Create `racklr/lower-rust/type.rkt`, extend `racklr/lower-rust/item.rkt` and `racklr/lower-rust.rkt`.

**Depends on**: B17

**Tasks**:

#### Task 1: Lower type_ expressions

- `typePath`: `i32`, `String`, `std::collections::HashMap` → uir-symbol
- `referenceType`: `&T`, `&'a T`, `&mut T` → `(uir-call (uir-symbol "ref") (list T))` or `(uir-call (uir-symbol "ref-mut") (list T))`
- `tupleType`: `(T1, T2)` → `(uir-call (uir-symbol "tuple") (list T1 T2))`
- `arrayType`: `[T; N]` → `(uir-call (uir-symbol "array") (list T N))`
- `sliceType`: `[T]` → `(uir-call (uir-symbol "slice") (list T))`
- `bareFunctionType`, `implTraitType`, `traitObjectType` → passthrough as uir-symbol with special name

```racket
(define (lower-type cst tk-type tk-value)
  (define tag (any-tree-tag cst))
  (match tag
    ['typePath (lower-type-path cst tk-type tk-value)]
    ['referenceType (lower-ref-type cst tk-type tk-value)]
    ['tupleType (lower-tuple-type cst tk-type tk-value)]
    [else (uir-symbol (format "?type-~a" tag))]))
```

#### Task 2: Lower `visibility`
- `pub` → `'public`
- `pub(crate)` → `'crate`
- `pub(super)` → `'super`
- `pub(self)` → `'private` (equivalent)
- Attach to uir-fn, uir-class, uir-enum, uir-method, uir-field via existing visibility fields.

#### Task 3: Lower `useDeclaration`
- `use std::collections::HashMap;` → `(uir-import (uir-symbol "std::collections::HashMap") '())`
- `use std::collections::{HashMap, HashSet};` → `(uir-import (uir-symbol "std::collections") (list (uir-symbol "HashMap") (uir-symbol "HashSet")))`
- `use std::collections::*;` → `(uir-import (uir-symbol "std::collections") (list (uir-symbol "*")))`

#### Task 4: Lower `impl` blocks
- `impl Foo { fn bar(&self) { ... } }` → lower each method as uir-method, then either:
  - **Option A**: Create a uir-impl struct
  - **Option B**: Find the matching uir-class and attach methods to it
  
  **Recommendation: Option B** — accumulate methods and attach to the corresponding uir-class. This means we need to collect all items first, then post-process: match impl blocks to their types. Simplest approach: during lowering of `crate`, collect items in two passes — first pass lowers structs/enums, second pass lowers impls and attaches methods.

  Actually, simpler: just lower impl methods as uir-method and store alongside the uir-class. For the top-level dispatcher, after lowering all items, do a merge pass.

  **Even simpler for Pass 1**: Lower impl methods as standalone uir-fn items with names like `impl-Foo-bar`. Skip the merge pass.

#### Task 5: Lower `trait_`
- `trait Foo { fn bar(&self) -> i32; }` → `(uir-interface (uir-symbol "Foo") (list (uir-method ...)))`

**Verification**: Tests for type lowering, visibility, use declarations, impl blocks, traits.

---

### B20 — Rust emitter: expressions and statements

**Why**: UIR → Rust text. Core of the raising pipeline.

**Files**: Create `racklr/emit-rust.rkt`, `racklr/emit-rust/expr.rkt`, `racklr/emit-rust/stmt.rkt`. Create `racklr-test/emit-rust-test.rkt`.

**Depends on**: B16 (uir-enum)

**Tasks**:

#### Task 1: Create `emit-rust.rkt` module skeleton
```racket
#lang racket
(require racklr/uir)

(provide emit-rust)

(define (emit-body uir indent)
  ...)

(define (emit-stmt uir indent)
  ...)

(define (emit-expr uir)
  ...)

(define (emit-rust uir)
  (cond [(uir-block? uir)
         (emit-body uir 0)]
        [else (emit-stmt uir 0)]))
```

#### Task 2: Emit `emit-expr` for all Tier-0 UIR types
Rust expression syntax:
- `uir-null` → `()` (unit)
- `uir-bool` → `true` / `false`
- `uir-number` → literal string (e.g., `"42"`)
- `uir-string` → `"..."` with proper escaping
- `uir-symbol` → symbol name (for type names, path segments)
- `uir-var` → variable name (for variable references)
- `uir-call` → `callee(args)` for function calls; infix operators: `(call + a b)` → `a + b`; `(call not a)` → `!a`; prefix special forms like `(call ref T)` → `&T`
- `uir-if` → `if test { then } else { else_ }` (Rust requires blocks, not statements)
- `uir-block` → `{ stmt1; stmt2; expr }`
- `uir-return` → `return value` (expression in Rust, not statement)
- `uir-for-each` → `for var in iterable { body }`
- `uir-while` → `while test { body }` (or `loop { body }` if test is uir-bool true)
- `uir-get` → `base.field` or `base[key]`
- `uir-paren` → `(inner)`
- `uir-fn` (anonymous) → `|params| body`
- `uir-match` → `match subject { cases }`
- `uir-list` → `[items]` (array literal)
- `uir-record` → `StructName { field: value, ... }` — but we need to know struct name. **Decision**: If uir-record has entries, emit as anonymous struct literal? Rust requires a type. Emit as tuple `(a, b)` for now, or add a type annotation convention.
- `uir-let` → not directly representable (Rust `let` is statement-level). Emit as `{ let name = value; body }` block expression.

#### Task 3: Emit `emit-stmt` for all UIR types in statement position
Rust requires `;` after expression statements unless the expression diverges or is a block.

- `uir-null` → `();`
- `uir-set!` → `let mut name = value;` (Rust requires `mut` for reassignable bindings)
- `uir-ann-set!` → `let name: type = value;` or `let name: type;`
- `uir-if` → `if test { then } else { else_ }` — if-else is an expression, add `;` if in statement position
- `uir-fn` (named) → `fn name(params) -> ReturnType { body }`
- `uir-return` → `return value;`
- `uir-call` with special names: `(call break)` → `break;`, `(call continue)` → `continue;`
- `uir-block` → `{ ... }` (no trailing semicolon)
- `uir-class` → `struct Name { fields }` + (if methods) `impl Name { methods }`
- `uir-enum` → `enum Name { variants }`
- `uir-interface` → `trait Name { methods }`
- `uir-import` → `use path;`
- `uir-module` → `mod name { body }`
- `uir-for-each` → `for var in iterable { body }`
- `uir-while` → `while test { body }` (or `loop { body }`)
- `uir-match` → `match subject { cases }`

#### Task 4: Emit functions (`uir-fn` → Rust `fn`)
```racket
(define (emit-funcdef uir indent)
  (define spc (make-string (* indent 4) #\space))
  (define name (uir-fn-name uir))
  (define name-str (if name (uir-symbol-name name) "???"))
  (define params-str
    (string-join (map emit-fn-param (uir-fn-params uir)) ", "))
  (define return-str
    (let ([rt (uir-fn-return-type uir)])
      (if rt (string-append " -> " (emit-expr rt)) "")))
  (define body-str (emit-body (uir-fn-body uir) (+ indent 1)))
  (string-append spc "fn " name-str "(" params-str ")" return-str " {\n"
                 body-str "\n" spc "}"))

(define (emit-fn-param p)
  (cond [(uir-symbol? p) (uir-symbol-name p)]
        [(uir-typed-param? p)
         (let* ([pn (uir-symbol-name (uir-typed-param-name p))]
                [pt (uir-typed-param-type p)]
                [pt-str (if pt (string-append ": " (emit-expr pt)) "")])
           (string-append pn pt-str))]
        [else (emit-expr p)]))
```

#### Task 5: Emit structs (`uir-class` → Rust `struct` + `impl`)
```racket
(define (emit-struct uir indent)
  (define spc (make-string (* indent 4) #\space))
  (define name (uir-symbol-name (uir-class-name uir)))
  (define fields (uir-class-fields uir))
  (define methods (uir-class-methods uir))
  (define fields-str
    (if (null? fields)
        ";"
        (string-append " {\n"
                       (string-join (map emit-field (uir-class-fields uir) (make-list (length fields) (+ indent 1))) "\n")
                       "\n" spc "}")))
  (define struct-def (string-append spc "struct " name fields-str))
  (if (null? methods)
      struct-def
      (string-append struct-def "\n\n" (emit-impl name methods indent))))
```

#### Task 6: Emit enums (`uir-enum` → Rust `enum`)
```racket
(define (emit-enum uir indent)
  (define spc (make-string (* indent 4) #\space))
  (define name (uir-symbol-name (uir-enum-name uir)))
  (define variants-str
    (string-join (map emit-enum-variant (uir-enum-variants uir) (make-list ...)) ",\n"))
  (string-append spc "enum " name " {\n" variants-str "\n" spc "}"))
```

#### Task 7: Emit match/case
Rust match arms use `pattern => expr,` syntax. Our UIR match was designed for Python `match/case` but the structure is compatible.
- `uir-match` with subject and cases
- Each `uir-case` has pattern, optional guard, body
- Emit: `match subject {\n  pattern [if guard] => body,\n}`

#### Task 8: Emit patterns
Rust patterns are similar to Python but different syntax:
- `uir-pat-literal` → literal value
- `uir-pat-capture` → variable name
- `uir-pat-wildcard` → `_`
- `uir-pat-or` → `pat1 | pat2`
- `uir-pat-struct` → map to Rust struct pattern `Name { field: pat, .. }` — we don't have uir-pat-struct. Use uir-pat-class convention.
- `uir-pat-tuple` → `(pat1, pat2, ...)` — we don't have uir-pat-tuple. Use uir-pat-sequence.
- Range patterns → not in UIR, skip for Pass 1

#### Task 9: Handle Rust-specific expression quirks
- All `if` branches must be blocks `{ ... }` — if the UIR then/else is not a uir-block, wrap it
- Blocks evaluate to their last expression (no semicolon on final expression)
- Semicolons after let,表达式 statements, etc.
- `self` parameter detection for methods

**Verification**: 
- `(emit-rust (uir-number "42"))` → `"42"`
- `(emit-rust (uir-call (uir-symbol "+") (list (uir-number "1") (uir-number "2"))))` → `"1 + 2"`
- `(emit-rust (uir-fn (uir-symbol "foo") '() (uir-block '()) #f))` → `"fn foo() {\n}"`
- `(emit-rust (uir-if (uir-bool #t) (uir-block (list (uir-return (uir-number "1")))) (uir-null)))` → `"if true {\n    return 1;\n}"`

---

### B21 — Rust round-trip integration tests

**Why**: End-to-end verification that the full pipeline works.

**Files**: Create `racklr-test/lower-rust-test.rkt` (extends B17's skeleton)

**Depends on**: B17, B18, B19, B20

**Tasks**:

#### Task 1: Define `check-rs-roundtrip` helper
```racket
(define (check-rs-roundtrip input-str expected-output)
  (define cst (rs-parse input-str))
  (check-true (any-tree? cst) (format "parse ~s" input-str))
  (define uir (lower-rust cst rs-tok-type rs-tok-value))
  (check-true (uir? uir) (format "lower ~s" input-str))
  (define emitted (emit-rust uir))
  (check-true (string? emitted) (format "emit ~s" input-str))
  (check-equal? emitted expected-output (format "round-trip ~s" input-str)))
```

#### Task 2: Round-trip tests
```
check-rs-roundtrip "" → ""
check-rs-roundtrip "fn foo() {}" → "fn foo() {\n}"
check-rs-roundtrip "fn add(x: i32, y: i32) -> i32 { x + y }" → "fn add(x: i32, y: i32) -> i32 {\n    x + y\n}"
check-rs-roundtrip "struct Foo { x: i32 }" → "struct Foo {\n    x: i32\n}"
check-rs-roundtrip "enum Color { Red, Green }" → "enum Color {\n    Red,\n    Green\n}"
check-rs-roundtrip "fn f(x: i32) -> i32 { if x > 0 { 1 } else { 0 } }" → "..."
check-rs-roundtrip "fn f() { let x = 1; }" → "fn f() {\n    let mut x = 1;\n}"
check-rs-roundtrip "fn f() { let x: i32 = 1; }" → "fn f() {\n    let x: i32 = 1;\n}"
check-rs-roundtrip "fn f() { return 42; }" → "fn f() {\n    return 42;\n}"
check-rs-roundtrip "fn f() { loop { break; } }" → "fn f() {\n    loop {\n        break;\n    }\n}"
check-rs-roundtrip "fn f() { match x { 1 => 2, _ => 0 } }" → "..."
check-rs-roundtrip "use std::collections::HashMap;" → "use std::collections::HashMap;"
check-rs-roundtrip "mod m { fn f() {} }" → "mod m {\n    fn f() {\n    }\n}"
check-rs-roundtrip "fn f() { for x in y { } }" → "fn f() {\n    for x in y {\n    }\n}"
check-rs-roundtrip "fn f() { while true { } }" → "fn f() {\n    while true {\n    }\n}"
check-rs-roundtrip "fn f() { let x = (1 + 2) * 3; }" → "..."
check-rs-roundtrip "fn f() { let x = a.b; }" → "fn f() {\n    let mut x = a.b;\n}"
check-rs-roundtrip "fn f() { let x = a[0]; }" → "fn f() {\n    let mut x = a[0];\n}"
check-rs-roundtrip "fn f() { let x = g(1, 2); }" → "fn f() {\n    let mut x = g(1, 2);\n}"
check-rs-roundtrip "trait Foo { fn bar(&self) -> i32; }" → "..."
```

#### Task 3: Add test for empty crate and multi-item crate

**Verification**: `raco test racklr-test/lower-rust-test.rkt` — all tests pass.

---

### B22 — Python → Rust cross-compilation integration test

**Why**: The user's stated goal: "taking mypy python and lowering to CST, and then raising it up back into rust".

**Files**: Create `racklr-test/python-to-rust-test.rkt`

**Depends on**: B21 (Rust round-trip working), existing lower-python and emit-python

**Tasks**:

#### Task 1: End-to-end Python→Rust tests
```racket
#lang racket
(require rackunit
         racklr/tree
         racklr/uir
         racklr/gen-test
         racklr/lower-python
         racklr/emit-rust)

(define-values (py-parse py-tokenize py-tok-type py-tok-value)
  (gen-and-load-py "../grammars-v4/python/python3/Python3Parser.g4"))

(define (check-py-to-rs input-py expected-rs)
  (define cst (py-parse input-py))
  (check-true (any-tree? cst))
  (define uir (lower-python cst py-tok-type py-tok-value))
  (check-true (uir? uir))
  (define emitted (emit-rust uir))
  (check-equal? emitted expected-rs))

;; Simple cases
(check-py-to-rs "pass\n" "();")
(check-py-to-rs "return 42\n" "return 42;")
(check-py-to-rs "x = 1\n" "let mut x = 1;")
(check-py-to-rs "x: int = 1\n" "let x: int = 1;")
(check-py-to-rs "import os\n" "use os;")
(check-py-to-rs "True\n" "true")
(check-py-to-rs "False\n" "false")
(check-py-to-rs "None\n" "()")

;; Functions
(check-py-to-rs "def foo(): pass\n\n" "fn foo() {\n}")
(check-py-to-rs "def add(x: int, y: int) -> int:\n    return x + y\n\n"
                "fn add(x: int, y: int) -> int {\n    return x + y;\n}")
```

#### Task 2: Handle Python→Rust semantic mismatches
- Python methods have explicit `self` first param → Rust methods also have `self` but Python emitter strips it, Rust emitter should add it
- Python `class` → Rust `struct` + `impl`
- Python `for x in y:` → Rust `for x in y { ... }`
- Python `while cond:` → Rust `while cond { ... }`
- Python `if/elif/else:` → Rust `if ... { ... } else if ... { ... } else { ... }`
- Python `try/except` → Rust has no try/catch — emit as `panic!()` or skip
- Python `with` → no Rust equivalent — skip
- Python `match/case` → Rust `match { ... }` — good match!
- Python `import X` → Rust `use X;`
- Python `from X import Y` → Rust `use X::Y;`
- Python decorators → Rust attributes `#[...]`? No, skip for now.
- Python string/list/dict/set literals → Rust equivalents (vec!, HashMap, HashSet)

**Verification**: `raco test racklr-test/python-to-rust-test.rkt` — tests pass.

---

## Summary: Bead Dependencies

```
B16 (module reorg + uir-enum structs)
 └─ B17 (lower: crate-level items — fn, struct, enum, mod)
     ├─ B18 (lower: statements, expressions)
     │   └─ B19 (lower: types, visibility, imports, impl, trait)
     │       └─ B21 (Rust round-trip tests)
     └─ B20 (emit: expressions, statements, items)
         └─ B21 (Rust round-trip tests)
             └─ B22 (Python→Rust cross-compilation)
```

## Files Created/Modified

| File | Action | Bead |
|------|--------|------|
| `racklr/uir/types.rkt` | **Create** (split + uir-enum structs) | B16 |
| `racklr/uir.rkt` | Modify (trim to dispatcher) | B16 |
| `racklr-test/uir-test.rkt` | Modify (add uir-enum tests) | B16 |
| `racklr/lower-python/helpers.rkt` | **Create** (split) | B16 |
| `racklr/lower-python/expr.rkt` | **Create** (split) | B16 |
| `racklr/lower-python/stmt.rkt` | **Create** (split) | B16 |
| `racklr/lower-python/compound.rkt` | **Create** (split) | B16 |
| `racklr/lower-python/pattern.rkt` | **Create** (split) | B16 |
| `racklr/lower-python/item.rkt` | **Create** (split) | B16 |
| `racklr/lower-python.rkt` | Modify (trim to dispatcher) | B16 |
| `racklr/emit-python/expr.rkt` | **Create** (split) | B16 |
| `racklr/emit-python.rkt` | Modify (trim) | B16 |
| `racklr/lower-typescript/helpers.rkt` | **Create** (split) | B16 |
| `racklr/lower-typescript/expr.rkt` | **Create** (split) | B16 |
| `racklr/lower-typescript/stmt.rkt` | **Create** (split) | B16 |
| `racklr/lower-typescript/compound.rkt` | **Create** (split) | B16 |
| `racklr/lower-typescript/pattern.rkt` | **Create** (split) | B16 |
| `racklr/lower-typescript/item.rkt` | **Create** (split) | B16 |
| `racklr/lower-typescript.rkt` | Modify (trim) | B16 |
| `racklr/lower-javascript/helpers.rkt` | **Create** (split) | B16 |
| `racklr/lower-javascript/expr.rkt` | **Create** (split) | B16 |
| `racklr/lower-javascript/stmt.rkt` | **Create** (split) | B16 |
| `racklr/lower-javascript/compound.rkt` | **Create** (split) | B16 |
| `racklr/lower-javascript/item.rkt` | **Create** (split) | B16 |
| `racklr/lower-javascript.rkt` | Modify (trim) | B16 |
| `racklr/emit-javascript/expr.rkt` | **Create** (split) | B16 |
| `racklr/emit-javascript.rkt` | Modify (trim) | B16 |
| `racklr/gend-parser/lexer.rkt` | **Create** (split) | B16 |
| `racklr/gend-parser/grammar.rkt` | **Create** (split) | B16 |
| `racklr/gend-parser.rkt` | Modify (trim) | B16 |
| `racklr/g4-parse/walk.rkt` | **Create** (split) | B16 |
| `racklr/g4-parse.rkt` | Modify (trim) | B16 |
| `racklr/lower-rust/helpers.rkt` | **Create** | B17 |
| `racklr/lower-rust/item.rkt` | **Create** | B17, B19 |
| `racklr/lower-rust/expr.rkt` | **Create** | B18 |
| `racklr/lower-rust/stmt.rkt` | **Create** | B18 |
| `racklr/lower-rust/pattern.rkt` | **Create** | B18 |
| `racklr/lower-rust/type.rkt` | **Create** | B19 |
| `racklr/lower-rust.rkt` | **Create** | B17 |
| `racklr/emit-rust/expr.rkt` | **Create** | B20 |
| `racklr/emit-rust/stmt.rkt` | **Create** | B20 |
| `racklr/emit-rust.rkt` | **Create** | B20 |
| `racklr-test/lower-rust-test.rkt` | **Create** | B21 |
| `racklr-test/python-to-rust-test.rkt` | **Create** | B22 |
| `bd` | Modify | All (add beads) |

## Implementation Order

1. **B16** — Module reorganization: split all >400-line files into nested directories. Add uir-enum + uir-enum-variant to `uir/types.rkt`. Verify all existing tests pass.
2. **B17** — Create `lower-rust/helpers.rkt`, `lower-rust/item.rkt`, `lower-rust.rkt` — crate-level items (fn, struct, enum, mod)
3. **B18** — Create `lower-rust/expr.rkt`, `lower-rust/stmt.rkt`, `lower-rust/pattern.rkt` — expressions, statements, patterns
4. **B19** — Create `lower-rust/type.rkt`, extend `lower-rust/item.rkt` — types, visibility, imports, impl, trait
5. **B20** — Create `emit-rust/expr.rkt`, `emit-rust/stmt.rkt`, `emit-rust.rkt` — UIR → Rust text
6. **B21** — Create `lower-rust-test.rkt` with comprehensive round-trip tests
7. **B22** — Create `python-to-rust-test.rkt` with cross-compilation tests

## Design Decisions

1. **uir-enum vs reuse uir-class**: New struct. Enums are semantically distinct and need different emission syntax.
2. **impl folding**: During lowering, merge `impl Foo` methods into the `uir-class` for `Foo`. During emission, split them back out as `impl Foo { ... }`. Simplest approach: maintain side table during lowering, do merge in post-pass.
3. **Generics**: Strip for Pass 1. The grammar parses them but we drop type parameters during lowering. Round-tripped Rust won't have generics. Add in Pass 2.
4. **Ownership/references**: Map `&T` → `(uir-call (uir-symbol "ref") (list T))` and `&mut T` → `(uir-call (uir-symbol "ref-mut") (list T))`. This lets the emitter reconstruct them.
5. **Self parameter**: Detect `self`/`&self`/`&mut self` in method params. Emit as Rust `self`/`&self`/`&mut self`.
6. **Path separators**: Rust uses `::`, Python uses `.`. In UIR, store as uir-symbol with `::` preserved. Python `import os.path` emits as `use os::path;`.
7. **mut keyword**: Rust requires `mut` for mutable bindings. Since UIR doesn't track mutability, we emit `let mut` for all `uir-set!` bindings (conservative, always correct).
8. **Nested directory structure**: Entry-point files stay flat (`lower-python.rkt`) for backward compat. Implementation files go in same-named subdirectory (`lower-python/expr.rkt`). This keeps the root `racklr/` directory < 25 entries and groups related code by language.
