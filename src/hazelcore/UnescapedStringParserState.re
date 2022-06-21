open Sexplib.Std;

[@deriving sexp]
type valid_seq = {
  start: int,
  ostart: int,
  length: int,
  olength: int,
};

[@deriving sexp]
type invalid_seq = {
  start: int,
  ostart: int,
  length: int,
};

[@deriving sexp]
type parsed = {
  unescaped: string,
  vseqs: list(valid_seq),
  iseqs: list(invalid_seq),
};

/** The buffer containing the parsed string literal's contents. */
let buffer = Buffer.create(256);

/** The stack of valid sequences encountered when parsing. */
let valid_seqs = Stack.create();

/** The stack of invalid sequences encountered when parsing. */
let invalid_seqs = Stack.create();

/** The current character index. */
let idx = ref(0);
let oidx = ref(0);

/** Add a character to the buffer. */
let add_char = c => {
  Buffer.add_char(buffer, c);
  idx := idx^ + 1;
  oidx := oidx^ + 1;
};

/** Add a string to the buffer. */
let add_string = s => {
  Buffer.add_string(buffer, s);
  idx := idx^ + String.length(s);
  oidx := oidx^ + String.length(s);
};

/** Add a valid escape sequence. */
let add_valid_seq = (c, olen) => {
  let length = 1;
  let olength = olen;

  Buffer.add_char(buffer, c);
  Stack.push({start: idx^, ostart: oidx^, length, olength}, valid_seqs);

  idx := idx^ + length;
  oidx := oidx^ + olength;
};

/** Add an error instance. */
let add_invalid_seq = lexbuf => {
  let start = idx^;
  let ostart = oidx^;
  let length = Lexing.lexeme_end(lexbuf) - Lexing.lexeme_start(lexbuf);
  add_string(Lexing.lexeme(lexbuf));

  Stack.push({start, ostart, length}, invalid_seqs);
  idx := start + length;
  oidx := ostart + length;
};

let int_of_char = c => {
  let charcode_0 = Char.code('0');
  Char.code(c) - charcode_0;
};

let int_of_hex_char = c => {
  let charcode_a = Char.code('a');
  let charcode_A = Char.code('A');
  switch (c) {
  | '0' .. '9' => int_of_char(c)
  | 'a' .. 'f' => 10 + Char.code(c) - charcode_a
  | 'A' .. 'F' => 10 + Char.code(c) - charcode_A
  | _ => assert(false)
  };
};

let int_of_decimal = (a, b, c) => a * 100 + b * 10 + c;

let int_of_decimal_seq = (a, b, c) =>
  int_of_decimal(int_of_char(a), int_of_char(b), int_of_char(c));

let int_of_octal = (a, b, c) => a * 64 + b * 8 + c;

let int_of_octal_seq = (a, b, c) =>
  int_of_octal(int_of_char(a), int_of_char(b), int_of_char(c));

let int_of_hex = (a, b) => a * 16 + b;

let int_of_hex_seq = (a, b) =>
  int_of_hex(int_of_hex_char(a), int_of_hex_char(b));

/** Add a decimal escape to the buffer. */
let add_decimal_seq = (lexbuf, i, olen) =>
  /* Check for valid escape value; if invalid, add error. */
  if (i > 0 && i < 256) {
    add_valid_seq(Char.chr(i), olen);
  } else {
    add_invalid_seq(lexbuf);
  };

/** Convert a special escape into the actual character. */
let add_escape_seq = c => {
  let c =
    switch (c) {
    | '\\'
    | '\''
    | '"'
    | ' ' => c
    | 'n' => '\n'
    | 't' => '\t'
    | 'b' => '\b'
    | 'r' => '\r'
    | _ => assert(false)
    };

  add_valid_seq(c, 2);
};
