
(* The type of tokens. *)

type token = 
  | WILD
  | VOID_TYPE
  | USE
  | UNKNOWN
  | UNIT
  | UNDEF
  | T_TYP
  | TYP_FUN
  | TYP_AP_SYMBOL
  | TYP
  | TURNSTILE
  | TUPLE_EXTENSION
  | TRUE
  | TP_TPAT
  | TIMES_FLOAT
  | TIMES
  | TILDE
  | THEN
  | TEST
  | STRING_TYPE
  | STRING_CONCAT
  | STRING of (string)
  | SLASH_TILDE
  | SINT_TYPE
  | SINGLE_EQUAL
  | SEMI_COLON
  | REC
  | QUOTED_LABEL of (string)
  | QUESTION
  | P_PAT
  | PROJECTOR_INVOKE of (string)
  | POWER_FLOAT
  | POWER
  | POLY
  | PLUS_FLOAT
  | PLUS
  | PIPELINE
  | PAUSE
  | OPEN_TRIPLE_CURLY
  | OPEN_SQUARE_BRACKET
  | OPEN_PAREN
  | OPEN_CURLY
  | NOT_EQUAL_FLOAT
  | NOT_EQUAL
  | NAT_TYPE
  | NAMED_FUN
  | MOD_SEMI
  | MODULE
  | MINUS_FLOAT
  | MINUS
  | L_OR
  | L_NOT
  | L_AND
  | LIVELIT_IDENT of (string)
  | LET
  | LESS_THAN_FLOAT
  | LESS_THAN_EQUAL_FLOAT
  | LESS_THAN_EQUAL
  | LESS_THAN
  | INT_TYPE
  | INTERNAL
  | INT of (Util.Bigint.t)
  | IN
  | IF
  | IDENT of (string)
  | HINT
  | HIDE
  | GREATER_THAN_FLOAT
  | GREATER_THAN_EQUAL_FLOAT
  | GREATER_THAN_EQUAL
  | GREATER_THAN
  | FUN
  | FLOAT_TYPE
  | FLOAT of (float)
  | FIX
  | FALSE
  | E_EXP
  | EVAL
  | EQUAL_ARROW
  | EOF
  | END
  | ELSE
  | DOUBLE_EQUAL_FLOAT
  | DOUBLE_EQUAL
  | DOT
  | DIVIDE_FLOAT
  | DIVIDE
  | DEBUG
  | DASH_ARROW
  | CONSTRUCTOR_IDENT of (string)
  | CONS
  | COMMA
  | COLON
  | CLOSE_TRIPLE_CURLY
  | CLOSE_SQUARE_BRACKET
  | CLOSE_PAREN
  | CLOSE_CURLY
  | CASE
  | BOOL_TYPE
  | AT_SYMBOL

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (AST.exp)
