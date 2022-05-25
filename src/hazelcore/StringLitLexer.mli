type error =
  (** An invalid escape sequence. *)
  | InvalidEscape of {
    start: int;
    length: int;
  } [@@deriving sexp]

(** Parse a string literal. *)
val lex: Lexing.lexbuf -> string * error list
