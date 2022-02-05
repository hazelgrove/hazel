module Parsing = Hazeltext.Parsing;

exception Unreachable;

let parse = (lexbuf: Lexing.lexbuf) => {
  let res = lexbuf |> Parsing.ast_of_lexbuf;

  switch (res) {
  | (Some(lines), _) => Ok(lines)
  | (None, Some(err)) => Error(err)
  | (None, None) => raise(Unreachable)
  };
};

let elaborate = (e: UHExp.t) => {
  // TODO: Update this when builtin-fns branch is merged.
  e |> Elaborator_Exp.syn_elab(Contexts.empty, Delta.empty);
};

let translate = (d: DHExp.t) => {
  d |> Translator.translate;
};

let emit = (d: IHExp.t) => {
  d |> Emit.emit;
};

let compile = (lexbuf: Lexing.lexbuf) => {
  let e = lexbuf |> parse;
  switch (e) {
  | Ok(e) =>
    let res = e |> elaborate;
    switch (res) {
    | Elaborates(d, _, _) => Ok(d |> translate |> emit)
    | DoesNotElaborate => Error("Does not elaborate")
    };
  | Error(err) => Error(err)
  };
};

let compile_string = (s: string) => s |> Lexing.from_string |> compile;

let compile_file = (f: in_channel) => f |> Lexing.from_channel |> compile;
