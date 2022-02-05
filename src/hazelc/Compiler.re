module Parsing = Hazeltext.Parsing;

exception Unreachable;

type compile_result = result(string, string);

let parse = (lexbuf: Lexing.lexbuf) => {
  let res = lexbuf |> Parsing.ast_of_lexbuf;

  switch (res) {
  | (Some(lines), _) => Ok(lines)
  | (None, Some(err)) => Error(err)
  | (None, None) => raise(Unreachable)
  };
};

// TODO: Update this when builtin-fns branch is merged.
let elaborate = Elaborator_Exp.syn_elab(Contexts.empty, Delta.empty);

let translate = Translator.translate;

let emit = Emit.emit;

let compile_dhexp = (d: DHExp.t) => {
  Ok(d |> translate |> emit);
};

let compile_uhexp = (e: UHExp.t) => {
  let res = e |> elaborate;
  switch (res) {
  | Elaborates(d, _, _) => compile_dhexp(d)
  | DoesNotElaborate => Error("Does not elaborate")
  };
};

let compile_buf = (lexbuf: Lexing.lexbuf) => {
  switch (parse(lexbuf)) {
  | Ok(e) => compile_uhexp(e)
  | Error(err) => Error(err)
  };
};

let compile_string = (s: string) => s |> Lexing.from_string |> compile_buf;

let compile_file = (f: in_channel) => f |> Lexing.from_channel |> compile_buf;
