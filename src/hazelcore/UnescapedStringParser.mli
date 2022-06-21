(** The type for a valid escape sequence. *)
type valid_seq = {
  start: int;
  ostart: int;
  length: int;
  olength: int;
} [@@deriving sexp]

(** The type for an invalid escape sequence. *)
type invalid_seq = {
  start: int;
  ostart: int;
  length: int;
} [@@deriving sexp]

type parsed = {
  unescaped: string;
  vseqs: valid_seq list;
  iseqs: invalid_seq list;
} [@@deriving sexp]

(** Parse a string literal. *)
val lex: Lexing.lexbuf -> string * valid_seq list * invalid_seq list 
