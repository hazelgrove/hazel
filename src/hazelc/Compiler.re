open Sexplib.Std;
open SexpResult;

module Parsing = Hazeltext.Parsing;

[@deriving sexp]
type grain_opts = Grain.opts;

[@deriving sexp]
type opts = {
  exp_only: bool,
  grain: grain_opts,
};

[@deriving sexp]
type err =
  | Parse(string)
  | Elab
  | Grain;

[@deriving sexp]
type compile_result = result(unit, err);

[@deriving sexp]
type grain_result = result(string, err);

let default_opts = {
  exp_only: false,
  grain: {
    grain: None,
    // TODO: Fix this to include Hazel lib files.
    includes: None,
    optimize: None,
    debug: None,
    wat: None,
  },
};

let parse = lexbuf => {
  let res = lexbuf |> Parsing.ast_of_lexbuf;

  switch (res) {
  | Ok(lines) => Ok(lines)
  | Error(err) => Error(Parse(err))
  };
};

let elaborate = Elaborator_Exp.syn_elab(Contexts.initial, Delta.empty);

let transform = Transformer.transform;

let translate = (~opts=default_opts, d) =>
  if (opts.exp_only) {
    Translator.translate(
      d //TODO: there is no public translate_exp
    );
  } else {
    Translator.translate(d);
  };

let grain_compile_dhexp = (~opts=default_opts, d) => {
  Ok(d |> transform |> translate(~opts));
};

let grain_compile_uhexp = (~opts=default_opts, e) => {
  let res = e |> elaborate;
  switch (res) {
  | Elaborates(d, _, _) => grain_compile_dhexp(~opts, d)
  | DoesNotElaborate => Error(Elab)
  };
};

let grain_compile_buf = (~opts=default_opts, lexbuf) => {
  switch (parse(lexbuf)) {
  | Ok(e) => grain_compile_uhexp(~opts, e)
  | Error(err) => Error(err)
  };
};

let grain_compile_string = (~opts=default_opts, s) =>
  s |> Lexing.from_string |> grain_compile_buf(~opts);

let grain_compile_file = (~opts=default_opts, f) =>
  f |> Lexing.from_channel |> grain_compile_buf(~opts);

let write_temporary = contents => {
  let (outpath, f) = Filename.open_temp_file("hazel", "compile.gr");
  Printf.fprintf(f, "%s\n", contents);
  close_out(f);

  outpath;
};

let compile_grain = (~opts=default_opts, outpath, grain_res) => {
  switch (grain_res) {
  | Ok(grain_output) =>
    let grain_outpath = write_temporary(grain_output);
    switch (
      Grain.compile(
        ~opts=opts.grain,
        {file: grain_outpath, output: Some(outpath)},
      )
    ) {
    | Ok(_) => Ok()
    | Error(_) => Error(Grain)
    };
  | Error(err) => Error(err)
  };
};

let compile_dhexp = (~opts=default_opts, d, outpath) => {
  d |> grain_compile_dhexp(~opts) |> compile_grain(~opts, outpath);
};

let compile_uhexp = (~opts=default_opts, e, outpath) => {
  e |> grain_compile_uhexp(~opts) |> compile_grain(~opts, outpath);
};

let compile_buf = (~opts=default_opts, lexbuf, outpath) => {
  lexbuf |> grain_compile_buf(~opts) |> compile_grain(~opts, outpath);
};

let compile_string = (~opts=default_opts, s, outpath) => {
  s |> grain_compile_string(~opts) |> compile_grain(~opts, outpath);
};

let compile_file = (~opts=default_opts, f, outpath) => {
  f |> grain_compile_file(~opts) |> compile_grain(~opts, outpath);
};
