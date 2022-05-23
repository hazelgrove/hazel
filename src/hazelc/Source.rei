[@deriving sexp]
type t =
  | SourceString(string)
  | SourceLexbuf(Lexing.lexbuf)
  | SourceChannel(in_channel);

let to_lexbuf: t => Lexing.lexbuf;
