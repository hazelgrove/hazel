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

module Conv = {
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
};

module State = {
  type t = {
    /** The buffer containing the parsed string literal's contents. */
    buffer: Buffer.t,
    /** The stack of valid sequences encountered when parsing. */
    vseqs: Stack.t(valid_seq),
    /** The stack of invalid sequences encountered when parsing. */
    iseqs: Stack.t(invalid_seq),
    /** The current character indices. */
    idx: ref(int),
    oidx: ref(int),
  };

  let make = () => {
    {
      buffer: Buffer.create(256),
      vseqs: Stack.create(),
      iseqs: Stack.create(),
      idx: ref(0),
      oidx: ref(0),
    };
  };

  let clear = state => {
    Buffer.clear(state.buffer);
    Stack.clear(state.vseqs);
    Stack.clear(state.iseqs);
    state.idx := 0;
    state.oidx := 0;
  };

  let return = state => {
    let to_list: 'a. Stack.t('a) => list('a) =
      stack => Stack.fold((acc, x) => [x, ...acc], [], stack);

    let unescaped = Buffer.contents(state.buffer);
    let r = (unescaped, state.vseqs |> to_list, state.iseqs |> to_list);

    clear(state);
    r;
  };

  /** Add a character to the buffer. */
  let add_char = (state, c) => {
    Buffer.add_char(state.buffer, c);
    state.idx := state.idx^ + 1;
    state.oidx := state.oidx^ + 1;
  };

  /** Add a string to the buffer. */
  let add_string = (state, s) => {
    Buffer.add_string(state.buffer, s);
    state.idx := state.idx^ + String.length(s);
    state.oidx := state.oidx^ + String.length(s);
  };

  /** Add a valid escape sequence. */
  let add_valid_seq = (state, c, olen) => {
    let length = 1;
    let olength = olen;

    Buffer.add_char(state.buffer, c);
    Stack.push(
      {start: state.idx^, ostart: state.oidx^, length, olength},
      state.vseqs,
    );

    state.idx := state.idx^ + length;
    state.oidx := state.oidx^ + olength;
  };

  /** Add an error instance. */
  let add_invalid_seq = (state, lexbuf) => {
    let start = state.idx^;
    let ostart = state.oidx^;
    let length = Lexing.lexeme_end(lexbuf) - Lexing.lexeme_start(lexbuf);

    add_string(state, Lexing.lexeme(lexbuf));
    Stack.push({start, ostart, length}, state.iseqs);
    state.idx := start + length;
    state.oidx := ostart + length;
  };

  let add_decimal_seq' = (state, lexbuf, i, olen) =>
    /* Check for valid escape value; if invalid, add error. */
    if (i > 0 && i < 256) {
      add_valid_seq(state, Char.chr(i), olen);
    } else {
      add_invalid_seq(state, lexbuf);
    };

  /** Add a decimal escape to the buffer. */
  let add_decimal_seq = (state, lexbuf, i) =>
    add_decimal_seq'(state, lexbuf, i, 3);

  /** Add a hex escape to the buffer. */
  let add_hex_seq = (state, lexbuf, i) =>
    add_decimal_seq'(state, lexbuf, i, 3);

  /** Add an octal escape to the buffer. */
  let add_octal_seq = (state, lexbuf, i) =>
    add_decimal_seq'(state, lexbuf, i, 4);

  /** Convert a special escape into the actual character. */
  let add_escape_seq = (state, c) => {
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

    add_valid_seq(state, c, 2);
  };
};

include State;
