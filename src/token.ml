open! Base

type comment = string * Location.t
type docstring = Docstrings.docstring
type location = Location.t

let sexp_of_comment = [%sexp_of: _]
let sexp_of_docstring = [%sexp_of: _]
let sexp_of_location = [%sexp_of: _]

type t = Parser.token =
  | WITH
  | WHILE
  | WHEN
  | VIRTUAL
  | VAL
  | UNDERSCORE
  | UIDENT of string
  | TYPE
  | TRY
  | TRUE
  | TO
  | TILDE
  | THEN
  | STRUCT
  | STRING of (string * location * string option)
  | STAR
  | STACK
  | SIG
  | SEMISEMI
  | SEMI
  | RPAREN
  | REPR
  | REC
  | RBRACKETGREATER
  | RBRACKET
  | RBRACE
  | QUOTED_STRING_ITEM of (string * location * string * location * string option)
  | QUOTED_STRING_EXPR of (string * location * string * location * string option)
  | QUOTE
  | QUESTION
  | PRIVATE
  | PREFIXOP of string
  | POLY
  | PLUSEQ
  | PLUSDOT
  | PLUS
  | PERCENT
  | OVERWRITE
  | OR
  | OPTLABEL of string
  | OPEN
  | OF
  | OBJECT
  | NONREC
  | NEW
  | MUTABLE
  | MODULE
  | MOD
  | MINUSGREATER
  | MINUSDOT
  | MINUS
  | METHOD
  | METAOCAML_ESCAPE
  | METAOCAML_BRACKET_OPEN
  | METAOCAML_BRACKET_CLOSE
  | MATCH
  | LPAREN
  | LOCAL
  | LIDENT of string
  | LETOP of string
  | LET
  | LESSMINUS
  | LESSLBRACKET
  | LESS
  | LBRACKETPERCENTPERCENT
  | LBRACKETPERCENT
  | LBRACKETLESS
  | LBRACKETGREATER
  | LBRACKETCOLON
  | LBRACKETBAR
  | LBRACKETATATAT
  | LBRACKETATAT
  | LBRACKETAT
  | LBRACKET
  | LBRACELESS
  | LBRACE
  | LAZY
  | LAYOUT
  | LABEL of string
  | KIND_OF
  | KIND
  | INT of (string * char option)
  | INITIALIZER
  | INHERIT
  | INFIXOP4 of string
  | INFIXOP3 of string
  | INFIXOP2 of string
  | INFIXOP1 of string
  | INFIXOP0 of string
  | INCLUDE
  | IN
  | IF
  | HASH_SUFFIX
  | HASH_INT of (string * char option)
  | HASH_FLOAT of (string * char option)
  | HASH_CHAR of char
  | HASHTRUE
  | HASHOP of string
  | HASHLPAREN
  | HASHLBRACE
  | HASHFALSE
  | HASH
  | GREATERRBRACKET
  | GREATERRBRACE
  | GREATER
  | GLOBAL
  | FUNCTOR
  | FUNCTION
  | FUN
  | FOR
  | FLOAT of (string * char option)
  | FALSE
  | EXTERNAL
  | EXCLAVE
  | EXCEPTION
  | EQUAL
  | EOL
  | EOF
  | END
  | ELSE
  | EFFECT
  | DOWNTO
  | DOTOP of string
  | DOTHASH
  | DOTDOT
  | DOT
  | DONE
  | DOLLAR
  | DOCSTRING of docstring
  | DO
  | CONSTRAINT
  | COMMENT of comment
  | COMMA
  | COLONRBRACKET
  | COLONGREATER
  | COLONEQUAL
  | COLONCOLON
  | COLON
  | CLASS
  | CHAR of char
  | BORROW
  | BEGIN
  | BARRBRACKET
  | BARBAR
  | BAR
  | BANG
  | BACKQUOTE
  | ATAT
  | AT
  | ASSERT
  | AS
  | ANDOP of string
  | AND
  | AMPERSAND
  | AMPERAMPER
[@@deriving sexp_of]

[%%template
[@@@mode.default m = (local, global)]

let compare (x : t) (y : t) = (Poly.compare [@mode m]) x y
let equal (x : t) (y : t) = (Poly.equal [@mode m]) x y]
