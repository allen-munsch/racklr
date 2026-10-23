# racklr Python front end: exhaustive blocker catalogue (Django 907-file scan)

Goal: effervesce the entire `django/django` tree (907 files, 165,826 lines) through
Python3 -> UIR -> effect inference -> Effervesce bridge/verify. This document lists
every construct racklr currently cannot handle, measured by a full-tree scan plus a
construct-level probe battery. It is the checklist of front-end blockers between the
current state and full-Django effervescing.

Measured with:

- `effervesce/tools/scan-python.rkt ../django/django --verbose` (whole-tree, 26.7s)
- `effervesce/tools/probe-python.rkt` (construct-level, 3-stage classification)

## Whole-tree totals

| bucket | files |
| ------ | ----- |
| ok (parses + lowers) | 207 |
| PARSE-FAIL | 572 |
| INDENT-ERROR | 124 |
| LOWER-ERROR | 3 |
| FALLBACK (lowers, but emits `?`-unhandled nodes) | 1 |

Files that cannot be effervesced today: **699 of 907 (77%)**.

The bottleneck is the Python front end, not the hypergraph/Effervesce layer. The
Effervesce substrate was proven working end to end on a toy Django example (clean
verify, and correct `db`-effect detection on a leaky variant).

---

## 1. NEWLINE tokens inside brackets are not suppressed (dominant PARSE-FAIL)

**Reproducers (all PARSE-FAIL):**

```python
from a.b import (       # paren-import-multi
    CASCADE,
    SET,
)
x = obj.method(         # call-multiline
    a,
    b,
)
d = { "a": 1, "b": 2 }  # dict-multiline  (when split across lines)
x = [1, 2]              # list-multiline
x = (1, 2)              # tuple-multiline
x = {1, 2}              # set-multiline
if (a and               # cond-multiline
        b):
    pass
x = d["k"]              # subscript-multiline
x = g(a, )              # call-trailing-comma
```

Single-line forms (`import-paren-single`, `dict` on one line, `backslash-cont`) are OK.

**Root cause.** CPython does implicit line joining: NEWLINE is never emitted while the
lexer is inside `( ) [ ] { }`. racklr's generated lexer does not track bracket depth, so
it emits NEWLINE (and, via `insert-indents`, INDENT/DEDENT) inside brackets. The grammar
rules already allow parenthesized forms (e.g. `import_from : ... '(' import_as_names ')'`
in `grammars-v4/python/python3/Python3Parser.g4:224-244`), so this is purely a lexical
gap, not a missing rule.

**Fix location.** `racklr/gend-parser/lexer.rkt` (`tokenize-raw` / `insert-indents`):
track `(` `[` `{` depth and suppress NEWLINE/INDENT/DEDENT while depth > 0.

**Impact.** This is the single largest cause of PARSE-FAIL: it breaks every multi-line
function call, dict/list/tuple/set literal, subscript, conditional, and parenthesized
import in the Django codebase.

---

## 2. Blank lines inside nested blocks break `insert-indents` (INDENT-ERROR, 124 files)

**Reproducer (RAISE):**

```python
def f():
    if x:
        a()

        b()      # blank line above raises
    else:
        c()
```

Error: `insert-indents: unindent does not match any outer indentation level`.

**Root cause.** A blank line is tokenized at column 0, which `insert-indents` reads as an
outdent. The captured token stream shows the blank line producing `DEDENT [x2]` followed
by a re-`INDENT` — the indent stack pops two levels, and the second pop has no matching
outer level. Blank lines must be ignored entirely (no NEWLINE contribution to the indent
stack).

**Fix location.** `racklr/gend-parser/lexer.rkt` `insert-indents` (~line 134; raise at
~186): skip empty/whitespace-only lines before computing indent.

**Impact.** 124 files. Blank lines inside any nested block are ubiquitous in real Python.

---

## 3. Comment-only and whitespace-only lines inside nested blocks (PARSE-FAIL)

**Reproducers (PARSE-FAIL):**

```python
def f():
    if x:
        a()
        # note      # comment-line-in-nested
        b()
    else:
        c()
```

```python
def f():
    if x:
        a()
                   # ws-only-line-in-nested (a line of spaces)
        b()
    else:
        c()
```

**Root cause.** Same family as #2. A comment-only line still emits NEWLINE with the
line's leading whitespace, and the trailing content of the comment disrupts the indent
machinery, so it surfaces as a parse failure rather than a raise.

**Fix location.** Same as #2: the indent pass must skip comment-only and whitespace-only
lines.

---

## 4. Non-decimal numeric literals (PARSE-FAIL)

**Reproducers (all PARSE-FAIL):**

```python
x = 0xFF        # hex
x = 0o17        # octal
x = 0b1010      # binary
x = 1.5         # float
x = 1.5e10      # float with exponent
x = 1j          # complex
```

Plain decimal (`12345`, `0`, `-5`) and `.5` are OK.

**Root cause.** The generated lexer emits distinct token types — `HEX_INTEGER`,
`OCT_INTEGER`, `BIN_INTEGER`, `FLOAT_NUMBER`, `IMAG_NUMBER` — but the parser grammar's
number rules (`Python3Parser.g4` ~lines 401-415) accept only `NUMBER` (plain decimal)
and `STRING`. The specialized integer/float/complex tokens have no parser rule.

**Fix.** Either canonicalize all numeric token types to `NUMBER` in the lexer, or extend
the parser's `atom`/number rule to accept `HEX_INTEGER | OCT_INTEGER | BIN_INTEGER |
FLOAT_NUMBER | IMAG_NUMBER`.

---

## 5. Underscore numeric literals, PEP 515 (PARSE-FAIL)

**Reproducer (PARSE-FAIL):**

```python
x = 1_000        # num-underscore
x = 0xFF_FF      # underscore in hex
x = 1_000.5      # underscore before fraction
```

**Root cause.** The lexer splits `1_000` into `NUMBER="1"` + `NAME="_000"` — the integer
fragment does not allow `_` inside the literal, so `_000` lexes as an identifier.
`1_000.5` becomes `NUMBER="1" NAME="_000" NUMBER=".5"`.

**Fix.** Allow `_` in the decimal/hex/octal/binary integer fragments of
`Python3Lexer.g4`.

---

## 6. Multi-line triple-quoted strings (PARSE-FAIL)

**Reproducers (PARSE-FAIL):**

```python
x = """a
b"""
def f():
    """a
    b"""            # multi-line docstring
    return 1
```

Single-line `"""abc"""` is OK.

**Root cause.** The lexer tokenizes a multi-line triple-quoted string as
`UNKNOWN_CHAR` + `NAME` + `NEWLINE` + `STRING` — the triple-quoted string rule does not
span newlines.

**Fix.** `Python3Lexer.g4` `STRING_LITERAL` / triple-quote fragments must accept
multi-line content.

---

## 7. Keyword-only marker `*` in parameter lists (LOWER-ERROR, 3 files)

**Reproducer (RAISE):**

```python
def f(a, *, b=None):
    return b
```

Error: `uir-typed-param-name: contract violation; expected: uir-typed-param?; given:
(uir-symbol "b")`.

**Root cause.** `lower-parameters` in `racklr/lower-python/stmt.rkt` (~line 650) does not
handle the bare `*` (STAR) separator in `typedargslist`. The following parameter `b` is
then misread as a typed param name.

**Fix.** In `lower-parameters`, treat the standalone `STAR` token as the keyword-only
separator and continue lowering subsequent params as keyword-only.

**Files in Django hitting this:** `contrib/messages/test.py`,
`core/checks/database.py`, `utils/translation/trans_null.py`.

---

## 8. Positional-only marker `/` (PARSE-FAIL)

**Reproducer (PARSE-FAIL):**

```python
def f(a, /, b):
    return b
```

**Root cause.** The `typedargslist` rule in the grammar does not accept the `/` separator
(Python 3.8 positional-only parameters).

**Fix.** Add `,? '/' (',' tfpdef)*` to `typedargslist`, and handle the marker in
`lower-parameters`.

---

## 9. Walrus operator `:=` (PARSE-FAIL)

**Reproducer (PARSE-FAIL):**

```python
def f():
    if (n := len(x)) > 0:
        return n
```

**Root cause.** The grammar is pre-3.8 and lacks the assignment expression rule
(`namedexpr_test`).

**Fix.** Add `namedexpr_test : test [':=' test]` and wire it into `if`/`while`
conditions.

---

## 10. `type` alias statement, PEP 695 (PARSE-FAIL)

**Reproducer (PARSE-FAIL):**

```python
type T = int | str
```

**Root cause.** The grammar is pre-3.12 and has no `type` statement rule.

**Fix.** Add the `type_alias` rule (Python 3.12) and lower it.

---

## 11. Class bases with keyword arguments (PARSE-FAIL)

**Reproducers (PARSE-FAIL):**

```python
class A(B, metaclass=M):
    pass
class A(metaclass=M):
    pass
```

Positional bases (`class A(B, C)`, `class A(*bases)`, `class A(**kw)`) are OK.

**Root cause.** The `classdef` arglist path does not accept the `name '=' test` keyword
form the way a function call does. (Reproducer verified at parse; the precise grammar
spot needs a look at the `arglist`/`argument` handling in the `classdef` context.)

---

## 12. Lowering gaps that emit `?`-unhandled nodes (FALLBACK)

These parse and lower without raising, but produce `?`-prefixed unhandled symbols. They
do not block the file, but lose information for effect inference.

- `a < b < c` (chained comparison) -> `?comp_op ?op`
- `a ** b` (power) -> `?expr`
- `...` (ellipsis) -> `?atom-ELLIPSIS`
- `int | None` (PEP 604 union annotation) -> `?expr`

The single FALLBACK file in Django is `tasks/exceptions.py`.

---

## Fix priority

Ranked by blast radius in the Django scan:

1. NEWLINE-inside-brackets (#1) — unlocks most of the 572 PARSE-FAIL files.
2. Blank/comment/whitespace-only lines in nested blocks (#2, #3) — unlocks the 124
   INDENT-ERROR files plus a share of PARSE-FAIL.
3. Non-decimal + underscore numeric literals (#4, #5) — pervasive in real code.
4. Multi-line triple-quoted strings (#6) — every docstring.
5. Keyword-only `*` (#7) — the 3 LOWER-ERROR files.
6. Modern-grammar features (#8-#10: `/`, `:=`, `type`), then class keyword bases (#11),
   then the FALLBACK lowering gaps (#12).

The fixes concentrate in two files: `racklr/gend-parser/lexer.rkt` (indent/bracket/line
handling) and the vendored `grammars-v4/python/python3/` lexer+parser grammar, plus
`racklr/lower-python/stmt.rkt` for the parameter-lowering gaps.
