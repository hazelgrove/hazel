(** Parser for string literals. *)
{
include UnescapedStringParserState
}

rule lex = parse
  (* Special escapes. *)
  | '\\' (['\\' '\'' '\"' 'n' 't' 'r' ' '] as c)
    {
      add_escape_seq c;
      lex lexbuf
    }
  (* Decimal escapes (\___). *)
  | '\\' (['0'-'9'] as a) (['0'-'9'] as b) (['0'-'9'] as c)
    {
      let i = int_of_decimal_seq a b c in
      add_decimal_seq lexbuf i 3;
      lex lexbuf
    }
  (* Octal escapes (\o___). *)
  | '\\' 'o' (['0'-'7'] as a) (['0'-'7'] as b) (['0'-'7'] as c)
    {
      let i = int_of_octal_seq a b c in
      add_decimal_seq lexbuf i 4;
      lex lexbuf
    }
  (* Hex escapes (\x__). *)
  | '\\' 'x' (['0'-'9' 'a'-'f' 'A'-'F'] as a) (['0'-'9' 'a'-'f' 'A'-'F'] as b)
    {
      let i = int_of_hex_seq a b in
      add_decimal_seq lexbuf i 3;
      lex lexbuf
    }
  (* Lone backslash; error *)
  | '\\' eof
    {
      add_invalid_seq lexbuf;
      lex lexbuf
    }
  (* Unrecognized escape sequence. *)
  | '\\' _
    {
      add_invalid_seq lexbuf;
      lex lexbuf
    }
  (* Reached end of string. *)
  | eof
    {
      (* Return string and invalid_seqs list; reset buffer, invalid_seqs, index. *)
      let invalid_seqs_list = Stack.fold (fun a x -> x::a) [] invalid_seqs in
      let valid_seqs_list = Stack.fold (fun a x -> x::a) [] valid_seqs in
      let r = (Buffer.contents buffer, valid_seqs_list, invalid_seqs_list) in
      Buffer.clear buffer;
      Stack.clear valid_seqs;
      Stack.clear invalid_seqs;
      idx := 0;
      oidx := 0;
      r
    }
  (* All other characters. *)
  | (_ as c)
    {
      add_char c;
      lex lexbuf
    }
