open Sexplib.Std;
open SexpResult;

module Parsing = Hazeltext.Parsing;

[@deriving sexp]
type opts = {exp_only: bool};

[@deriving sexp]
type err =
  | Parse(string)
  | Elab;

[@deriving sexp]
type compile_result = result(string, err);

let default_opts = {exp_only: false};

let parse = lexbuf => {
  let res = lexbuf |> Parsing.ast_of_lexbuf;

  switch (res) {
  | Ok(lines) => Ok(lines)
  | Error(err) => Error(Parse(err))
  };
};

// TODO: Update this when builtin-fns branch is merged.
let elaborate = Elaborator_Exp.syn_elab(Contexts.empty, Delta.empty);

let transform = Transformer.transform;

let translate = (~opts=default_opts, d) =>
  if (opts.exp_only) {
    Translator.translate_exp(d);
  } else {
    Translator.translate(d);
  };

let compile_dhexp = (~opts=default_opts, d) => {
  Ok(d |> transform |> translate(~opts));
};

let compile_uhexp = (~opts=default_opts, e) => {
  let res = e |> elaborate;
  switch (res) {
  | Elaborates(d, _, _) => compile_dhexp(~opts, d)
  | DoesNotElaborate => Error(Elab)
  };
};

let compile_buf = (~opts=default_opts, lexbuf) => {
  switch (parse(lexbuf)) {
  | Ok(e) => compile_uhexp(~opts, e)
  | Error(err) => Error(err)
  };
};

let compile_string = (~opts=default_opts, s) =>
  s |> Lexing.from_string |> compile_buf(~opts);

let compile_file = (~opts=default_opts, f) =>
  f |> Lexing.from_channel |> compile_buf(~opts);
