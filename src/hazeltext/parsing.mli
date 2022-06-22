(* Parses the given text string, returning a result of a UHExp.t,
   or a string with the given error message *)
val ast_of_string : string -> (UHExp.t, string) result
