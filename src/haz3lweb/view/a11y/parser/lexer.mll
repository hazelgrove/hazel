{
  open Parser
}

rule token = parse
  | ['0'-'9']+ as d { DIGITS(int_of_string d) }
  | 'w'           { TEXT_OBJECT_KEY(Term) }
  | ['(' ')']    { TEXT_OBJECT_KEY(Parenthesis) }
  | 'm'           { ACTION_KEY(Move) }
  | 'd'           { QUERY_DECORATION(Definition) }
  | 't'           { QUERY_KEY(Type) }
  | 'r'           { QUERY_KEY(Read) }
  | 'i'           { TEXT_OBJECT_INNER }
  | 'q'           { TEXT_OBJECT_QUERY }
  | [' ' '\t' '\n' '\r']   { token lexbuf }  (* Skip whitespaces *)
  | eof           { EOF }
  | _             { failwith ("Unexpected character: " ^ Lexing.lexeme lexbuf) }
