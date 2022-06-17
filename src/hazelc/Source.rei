[@deriving sexp]
type t =
  | Text(string)
  | Lexbuf(Lexing.lexbuf)
  | File(in_channel);

let to_lexbuf: t => Lexing.lexbuf;
