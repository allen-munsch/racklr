// Koka grammar for racklr (authored, pass 1).
//
// Koka is layout-sensitive: indentation delimits blocks. racklr's generated
// lexer inserts synthetic NEWLINE/INDENT/DEDENT tokens (the Python3 model),
// so this grammar names its tokens to match that machinery exactly:
//   NEWLINE, INDENT, DEDENT, OPEN_PAREN, CLOSE_PAREN,
//   OPEN_BRACK, CLOSE_BRACK, OPEN_BRACE, CLOSE_BRACE
//
// Pass-1 scope and deliberate approximations (see docs/plans/koka-pipeline.md):
//   - Blank lines inside blocks are tolerated (extra NEWLINE tokens).
//   - `<`/`>` are reserved for effect annotations; comparison `<`/`>` are
//     not in the operator set yet.
//   - Effect annotations use the `<>` form (`: <exn, div> int`) or `total`;
//     bare effect names (`console`) are pass 2.
//   - Types are `()` or a single identifier (no generics / function types).
//   - `with handler` uses `fun op(...) -> expr` operation clauses,
//     `return(x) -> expr`, then body statements in the same block.

grammar Koka;

// ── Entry point ─────────────────────────────────────────────────────

file : (NEWLINE | stmt)* EOF ;

// ── Statements / declarations ───────────────────────────────────────

stmt : simple_stmt | compound_stmt ;

simple_stmt : expr | valdecl ;

compound_stmt
  : funcdecl
  | effectdecl
  | typedecl
  | importdecl
  | moduledecl
  ;

moduledecl : MODULE qualname ;
importdecl : IMPORT qualname ;
valdecl    : VAL IDENT ASSIGN expr ;
funcdecl   : FUN IDENT OPEN_PAREN params? CLOSE_PAREN rettype? block ;
effectdecl : EFFECT IDENT NEWLINE INDENT (NEWLINE | opdecl)+ DEDENT ;
typedecl   : TYPE IDENT NEWLINE INDENT (NEWLINE | constr)+ DEDENT ;

// ── Blocks ──────────────────────────────────────────────────────────

block : NEWLINE INDENT (NEWLINE | stmt)+ DEDENT ;

// ── Declarations inside effect/type blocks ──────────────────────────

opdecl : FUN IDENT OPEN_PAREN params? CLOSE_PAREN COLON ktype ;
constr : TYPE_IDENT (OPEN_PAREN params? CLOSE_PAREN)? ;

// ── Types ───────────────────────────────────────────────────────────

rettype : COLON effects? ktype ;
effects : LT IDENT (COMMA IDENT)* GT | TOTAL ;
ktype   : OPEN_PAREN CLOSE_PAREN | IDENT ;

params : param (COMMA param)* ;
param  : IDENT (COLON ktype)? ;

qualname : IDENT (DOT IDENT)* ;

// ── Expressions ─────────────────────────────────────────────────────

expr
  : withexpr
  | ifexpr
  | lambda
  | binop_expr
  ;

ifexpr : IF expr THEN block (ELSE block)? ;

withexpr : WITH HANDLER NEWLINE INDENT (NEWLINE | handler_item)+ DEDENT ;

handler_item : op_clause | return_clause | stmt ;
op_clause    : FUN IDENT OPEN_PAREN params? CLOSE_PAREN (COLON ktype)? ARROW expr ;
return_clause : RETURN OPEN_PAREN IDENT CLOSE_PAREN ARROW expr ;

lambda : FN OPEN_PAREN params? CLOSE_PAREN block ;

binop_expr : binop_expr OP unary_expr | unary_expr ;

unary_expr : app_expr ;

app_expr : primary postfix* ;

postfix : DOT IDENT | OPEN_PAREN args? CLOSE_PAREN ;

primary : literal | IDENT | TYPE_IDENT | OPEN_PAREN expr CLOSE_PAREN ;

literal : INT | STRING ;

args : expr (COMMA expr)* ;

// ── Lexer ───────────────────────────────────────────────────────────

// Layout: newline plus the following line's leading indentation.
NEWLINE : ('\r'? '\n' | '\r') [ \t]* ;

// Keywords (ordered before IDENT; maximal munch + first-wins resolves ties).
MODULE  : 'module' ;
IMPORT  : 'import' ;
FUN     : 'fun' ;
FN      : 'fn' ;
VAL     : 'val' ;
TYPE    : 'type' ;
EFFECT  : 'effect' ;
WITH    : 'with' ;
HANDLER : 'handler' ;
RETURN  : 'return' ;
IF      : 'if' ;
THEN    : 'then' ;
ELSE    : 'else' ;
TOTAL   : 'total' ;

// Structural tokens (exact names required by the indent machinery).
OPEN_PAREN  : '(' ;
CLOSE_PAREN : ')' ;
OPEN_BRACK  : '[' ;
CLOSE_BRACK : ']' ;
OPEN_BRACE  : '{' ;
CLOSE_BRACE : '}' ;

// Punctuation.
COLON  : ':' ;
COMMA  : ',' ;
DOT    : '.' ;
ARROW  : '->' ;
ASSIGN : '=' ;
LT     : '<' ;
GT     : '>' ;

// Binary operators (no `<`/`>` — reserved for effect annotations, pass 1).
OP : '==' | '!=' | '+' | '-' | '*' | '/' ;

INT    : [0-9]+ ;
STRING : '"' (~'"' | '\\' '"')* '"' ;

// Identifiers: value/type/effect names are lowercase-start; constructors
// (Cons, Nil, True, False) are uppercase-start.
IDENT      : [a-z_][a-zA-Z0-9_']* ;
TYPE_IDENT : [A-Z][a-zA-Z0-9_']* ;

COMMENT       : '//' ~[\r\n]* -> skip ;
BLOCK_COMMENT : '/*' .*? '*/' -> skip ;
