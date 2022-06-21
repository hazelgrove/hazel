type seq = {
  start: int;
  ostart: int;
  length: int;
  olength: int;
} [@@deriving sexp]

type error =
  (** An invalid escape sequence. *)
  | InvalidSeq of {
    start: int;
    ostart: int;
    length: int;
  } [@@deriving sexp]

(** Parse a string literal. *)
val lex: Lexing.lexbuf -> string * seq list * error list 
