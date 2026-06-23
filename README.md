# racklr — Multi-Language Transpiler Toolkit

Racklr is a parser generator and universal intermediate representation (UIR) toolkit built in Racket. It reads ANTLR4 grammar definitions (.g4 files), generates working lexer+parser modules, parses source text into Concrete Syntax Trees (CSTs), lowers CSTs to a language-independent UIR, and emits target code from UIR nodes.

**Experimental release — Python 3 and JavaScript round-trips are working. Cross-compilation and additional languages are planned for future releases.**

## Status

| Language | Parser | Lowering (CST → UIR) | Emit (UIR → source) | Round-trip |
|----------|--------|---------------------|----------------------|------------|
| JSON | ✓ | ✓ | ✓ | ✓ |
| Python 3 | ✓ | ✓ | ✓ | ✓ |
| JavaScript | ✓ | ✓ | ✓ | ✓ |

**Planned (not yet implemented):** Rust, TypeScript, HTML, CSS, React/JSX, Svelte, cross-compilation pipeline.

## Architecture

```
grammars-v4/*.g4          ← ANTLR4 grammar files (vendor)
        │
        ▼
  g4-lex.rkt              ← Tokenizes .g4 grammar files into token streams
  g4-parse.rkt            ← Parses token streams into grammar CSTs
        │
        ▼
  gend-parser.rkt         ← Generates Racket lexer+parser source from grammar CST
        │
        ▼
  Generated parser.rkt    ← Standalone Racket module (lexer + parser)
        │
        ├── tokenize: string → (listof token)
        └── parse:   string → cst-node
                │
                ▼
  tree.rkt                ← CST/UIR node types (node, leaf, any-tree?)
                │
                ▼
  lower-<lang>.rkt        ← CST → UIR lowering pass (per language)
  emit-<lang>.rkt         ← UIR → language text code generator (per language)
                │
                ▼
  uir.rkt                 ← UIR struct definitions (Tier 0 + Tier 1 types)
```

## Component Reference

### g4-lex.rkt
Grammar lexer. Reads ANTLR4 grammar source text, produces tokens (identifiers, keywords, operators, strings, char-classes, comments). Handles ANTLR4-specific syntax: `->` commands, `?*+` suffixes, `..` ranges, `~` negation, `<` `>` element options, `#` alternative labels, `=` and `+=` element labels.

### g4-parse.rkt
Grammar parser. Consumes token stream from g4-lex, produces a grammar CST with nodes for: `parser-rule`, `lexer-rule`, `fragment-rule`, `mode`, `options`, `alternative`, and element types (`literal`, `token-ref`, `rule-ref`, `char-class`, `star`, `plus`, `optional`, `group`, `negated`, `labeled`, `append-labeled`, `action`).

### gend-parser.rkt
Parser generator. Takes a grammar CST and produces a complete Racket module source string. The generated module provides `token` struct (type, value, start, end), `tokenize` (lexer), and `parse` (parser).

Supports: lexer modes, tokenVocab imports (auto-loads lexer grammar from same directory), left-recursion detection (warns and rewrites to accumulator loops), INDENT/DEDENT injection for Python-style layout languages.

### tree.rkt
Core tree types: `source-pos` struct (line, col, offset), `cst-node` (tag + children + range), `cst-leaf` (tag + text + range), and `any-tree?` predicate.

### uir.rkt
Universal Intermediate Representation. Defines 40+ node types across Tier 0 (computational) and Tier 1 (OOP/module): `uir-null`, `uir-bool`, `uir-number`, `uir-string`, `uir-symbol`, `uir-list`, `uir-record`, `uir-var`, `uir-fn`, `uir-call`, `uir-if`, `uir-block`, `uir-return`, `uir-set!`, `uir-let`, `uir-while`, `uir-for-each`, `uir-try`, `uir-throw`, `uir-with`, `uir-class`, `uir-define-class`, `uir-get`, `uir-import`, `uir-export`, `uir-decorated`, `uir-fstring`, `uir-paren`, `uir-ternary`, `uir-spread`, `uir-new`, `uir-unary-op`, and 14 match/case pattern types. Schema documented in `docs/uir-schema.md`.

### lower-python.rkt / emit-python.rkt (1,744 + 470 lines)
Python 3 lowering and emit. Handles: functions (def, lambda, async, decorated), classes, for/while loops, try/except/finally, with, if/elif/else, match/case, return/yield/await, assignments (including augmented), imports, list/set/dict comprehensions, generators, f-strings, ternary expressions, all operators.

Known limitations: walrus operator (`:=`) not in grammar; match/case class patterns and dotted-name value patterns hit parser edge cases.

### lower-javascript.rkt / emit-javascript.rkt (1,058 + 399 lines)
JavaScript (ES2020) lowering and emit. Handles: function/arrow/async/generator functions, classes with methods/getters/setters, object/array literals, spread/rest, for/for-in/for-of/while/do-while, try/catch/throw, switch/case, break/continue, import/export, ternary, unary operators, `new`/`this`.

Known limitations: template literals and tagged templates not yet implemented; `debugger`, `with`, and labeled statements not yet lowered.

### lower-json.rkt / emit-json.rkt
JSON pipeline proof-of-concept: parse JSON → lower to UIR → emit JSON text → parse again.

## Generated Parser Design

### Lexer (tokenize)
Recursive descent with character-level matching. Supports lexer modes with a mode stack. Whitespace and newlines update position tracking (line/col/offset). For Python, a post-processing pass injects INDENT/DEDENT tokens based on NEWLINE + leading whitespace.

### Parser (parse)
Recursive descent with backtracking. Each grammar rule becomes a `parse-XXX` function returning `(list new-pos cst-node)` or `#f`.

### Known Limitations
- `~(...)` negated token sets in parser rules are skipped (treated as always matching)
- Char-class `\uXXXX` codepoint escapes not handled in generated `cc-match`
- Element labels (`id = element`, `id += element`) are parsed but not used in generated code
- Unicode property classes (`\p{XX}`) in char-classes may have limited support
- Left-recursive alternatives are rewritten to accumulator loops with a warning

## Adding a New Language

1. Find or create ANTLR4 `.g4` grammar files in `grammars-v4/<lang>/`
2. If using separate lexer/parser grammars, ensure `tokenVocab = <LexerName>` is set in the parser grammar
3. Generate and test: `(parse-g4-file "grammars-v4/<lang>/<Lang>Parser.g4")` → `(generate-parser-module cst #:source-path path)`
4. Write lowering pass: CST → UIR
5. Write code generator: UIR → target language text

## Testing

All 1,129 tests pass.

- `raco test racklr-test/gen-json-test.rkt` — JSON parser generation and parsing tests
- `raco test racklr-test/gen-python3-test.rkt` — Python 3 parser generation and parsing tests
- `raco test racklr-test/gen-javascript-test.rkt` — JavaScript parser generation and parsing tests
- `raco test racklr-test/lower-python-test.rkt` — Python lowering + emit round-trip tests
- `raco test racklr-test/lower-javascript-test.rkt` — JavaScript lowering + emit round-trip tests
- `raco test racklr-test/gend-parser-test.rkt` — Parser generator unit tests
- `raco test racklr-test/g4-parse-test.rkt` — Grammar parser tests
- `raco test racklr-test/uir-test.rkt` — UIR type and serialization tests
- `raco test racklr-test/tree-test.rkt` — Core tree operations

Run all tests: `raco test racklr-test/*.rkt`

## Dependencies

- Racket 9.x (tested with 9.2)
- No external Racket packages required
- ANTLR4 grammar files from [grammars-v4](https://github.com/antlr/grammars-v4) (vendored in `grammars-v4/`)

## Setup

```bash
# Clone the repo
git clone https://github.com/allen-munsch/racklr.git
cd racklr

# Install the package (linked mode)
raco pkg install --link .

# Fetch grammar files
git clone https://github.com/antlr/grammars-v4.git

# Run all tests
raco test racklr-test/*.rkt
```

## Roadmap

**Future releases:**
- Cross-compilation: Python ↔ JavaScript via UIR
- Rust: parser + lowering + emit
- TypeScript: parser + lowering + emit
- HTML, CSS: Tier 2 UIR lowering + emit
- React/JSX, Svelte: Tier 2 UIR lowering + emit
- Universal transpilation pipeline with language registry
