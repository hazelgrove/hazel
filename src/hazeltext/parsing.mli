(* Parses the given text lexbuf, returning a result of a UHExp.t,
   or a string with the given error message *)
val ast_of_lexbuf : Lexing.lexbuf -> (UHExp.t, string) result

(* Parses the given text string, returning a result of a UHExp.t,
   or a string with the given error message *)
val ast_of_string : string -> (UHExp.t, string) result
