(** Parser for string literals. *)
{
include UnescapedStringParserState
}

rule lex state = parse
  (* Special escapes. *)
  | '\\' (['\\' '\'' '\"' 'n' 't' 'r' ' '] as c)
    {
      add_escape_seq state c;
      lex state lexbuf
    }
  (* Decimal escapes (\___). *)
  | '\\' (['0'-'9'] as a) (['0'-'9'] as b) (['0'-'9'] as c)
    {
      let i = Conv.int_of_decimal_seq a b c in
      add_decimal_seq state lexbuf i;
      lex state lexbuf
    }
  (* Octal escapes (\o___). *)
  | '\\' 'o' (['0'-'7'] as a) (['0'-'7'] as b) (['0'-'7'] as c)
    {
      let i = Conv.int_of_octal_seq a b c in
      add_octal_seq state lexbuf i;
      lex state lexbuf
    }
  (* Hex escapes (\x__). *)
  | '\\' 'x' (['0'-'9' 'a'-'f' 'A'-'F'] as a) (['0'-'9' 'a'-'f' 'A'-'F'] as b)
    {
      let i = Conv.int_of_hex_seq a b in
      add_hex_seq state lexbuf i;
      lex state lexbuf
    }
  (* Lone backslash; error *)
  | '\\' eof
    {
      add_invalid_seq state lexbuf;
      lex state lexbuf
    }
  (* Unrecognized escape sequence. *)
  | '\\' _
    {
      add_invalid_seq state lexbuf;
      lex state lexbuf
    }
  (* Reached end of string. *)
  | eof
    {
      return state
    }
  (* All other characters. *)
  | (_ as c)
    {
      add_char state c;
      lex state lexbuf
    }

{
let parse lexbuf =
  let state = State.make () in
  let r = lex state lexbuf in
  let _ = State.clear state in
  r
}
