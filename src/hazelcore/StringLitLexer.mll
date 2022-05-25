(** Parser for string literals. *)
{
open Sexplib.Std

type error =
  (** An invalid escape sequence. *)
  | InvalidEscape of {
    start: int;
    length: int;
  } [@@deriving sexp]

(** The buffer containing the parsed string literal's contents. *)
let buffer : Buffer.t = Buffer.create 256

(** The stack of errors encountered when parsing. *)
let errors: error Stack.t = Stack.create ()

(** The current character index. *)
let idx = ref 0

(** Add a character to the buffer. *)
let add_char c =
  Buffer.add_char buffer c;
  idx := !idx + 1

(** Add a string to the buffer. *)
let add_string s =
  Buffer.add_string buffer s;
  idx := !idx + String.length s

(** Add an error instance. *)
let invalid_escape lexbuf =
  let start = !idx in
  let length = Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf in

  add_string (Lexing.lexeme lexbuf);
  Stack.push (InvalidEscape {
    start = start;
    length = length
  }) errors;
  idx := start + length

let charcode_0 = Char.code '0'
let int_of_char c =
  Char.code c - charcode_0

let charcode_a = Char.code 'a'
let charcode_A = Char.code 'A'
let int_of_hex_char c =
  match c with
  | '0' .. '9' -> int_of_char c
  | 'a' .. 'f' -> 10 + Char.code c - charcode_a
  | 'A' .. 'F' -> 10 + Char.code c - charcode_A
  | _ -> assert false

let int_of_decimal a b c =
  a * 100 + b * 10 + c

let int_of_decimal_escape a b c =
  int_of_decimal (int_of_char a) (int_of_char b) (int_of_char c)

let int_of_octal a b c =
  a * 64 + b * 8 + c

let int_of_octal_escape a b c =
  int_of_octal (int_of_char a) (int_of_char b) (int_of_char c)

let int_of_hex a b =
  a * 16 + b

let int_of_hex_escape a b =
  int_of_hex (int_of_hex_char a) (int_of_hex_char b)

(** Add a decimal escape to the buffer. *)
let add_decimal_code lexbuf i =
  (* Check for valid escape value; if invalid, add error. *)
  if (i > 0 && i < 256) then
    add_char (Char.chr i)
  else
    begin
      invalid_escape lexbuf
    end

(** Convert a special escape into the actual character. *)
let escapechar c =
  match c with
  | '\\' | '\'' | '\"' | ' ' -> c
  | 'n' -> '\n'
  | 't' -> '\t'
  | 'b' -> '\b'
  | 'r' -> '\r'
  | _ -> assert false
}

rule lex = parse
  (* Special escapes. *)
  | '\\' (['\\' '\'' '\"' 'n' 't' 'r' ' '] as c)
    {
      add_char (escapechar c);
      lex lexbuf
    }
  (* Decimal escapes (\___). *)
  | '\\' (['0'-'9'] as a) (['0'-'9'] as b) (['0'-'9'] as c)
    {
      let i = int_of_decimal_escape a b c in
      add_decimal_code lexbuf i;
      lex lexbuf
    }
  (* Octal escapes (\o___). *)
  | '\\' 'o' (['0'-'7'] as a) (['0'-'7'] as b) (['0'-'7'] as c)
    {
      let i = int_of_octal_escape a b c in
      add_decimal_code lexbuf i;
      lex lexbuf
    }
  (* Hex escapes (\x__). *)
  | '\\' 'x' (['0'-'9' 'a'-'f' 'A'-'F'] as a) (['0'-'9' 'a'-'f' 'A'-'F'] as b)
    {
      let i = int_of_hex_escape a b in
      add_decimal_code lexbuf i;
      lex lexbuf
    }
  (* Lone backslash; error *)
  | '\\' eof
    {
      invalid_escape lexbuf;
      lex lexbuf
    }
  (* Unrecognized escape sequence. *)
  | '\\' _
    {
      invalid_escape lexbuf;
      lex lexbuf
    }
  (* Reached end of string. *)
  | eof
    {
      (* Return string and errors list; reset buffer, errors, index. *)
      let errors_list = Stack.fold (fun a x -> x::a) [] errors in
      let r = (Buffer.contents buffer, errors_list) in
      Buffer.clear buffer;
      Stack.clear errors;
      idx := 0;
      r
    }
  (* All other characters. *)
  | (_ as c)
    {
      add_char c;
      lex lexbuf
    }
