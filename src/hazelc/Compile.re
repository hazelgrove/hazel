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

type source =
  | SourceString(string)
  | SourceLexbuf(Lexing.lexbuf)
  | SourceChannel(in_channel);

let sexp_of_source = source => {
  Sexplib.Sexp.(
    switch (source) {
    | SourceString(s) => List([Atom("string"), sexp_of_string(s)])
    | SourceLexbuf(_) => List([Atom("lexbuf"), Atom("<Lexing.lexbuf>")])
    | SourceChannel(_) => List([Atom("channel"), Atom("<in_channel>")])
    }
  );
};

let source_of_sexp = sexp => {
  Sexplib0.(
    Sexplib.Sexp.(
      {
        let of_sexp_error = (what, sexp) =>
          raise(Sexp.Of_sexp_error(Failure(what), sexp));

        switch (sexp) {
        | List([Atom("string"), el]) => SourceString(string_of_sexp(el))
        | List([Atom("lexbuf"), _]) =>
          of_sexp_error("source_of_sexp: cannot deseralize for lexbuf", sexp)
        | List([Atom("channel"), _]) =>
          of_sexp_error(
            "source_of_sexp: cannot deseralize for in_channel",
            sexp,
          )
        | List(_) =>
          of_sexp_error("source_of_sexp: list must be (string el)", sexp)
        | Atom(_) => of_sexp_error("source_of_sexp: list needed", sexp)
        };
      }
    )
  );
};

[@deriving sexp]
type state =
  | Source(source)
  | Parsed(UHExp.t)
  | Elaborated(DHExp.t)
  | Transformed(IHExp.t)
  | Linearized(Anf.prog)
  | Grainized(GrainIR.prog)
  | Printed(string)
  | Wasmized(string);

[@deriving sexp]
type next_error =
  | ParseError(string)
  | ElaborateError
  | GrainError;

[@deriving sexp]
type next_result = result(state, next_error);

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

let parse = source => {
  let lexbuf =
    switch (source) {
    | SourceString(s) => s |> Lexing.from_string
    | SourceLexbuf(lexbuf) => lexbuf
    | SourceChannel(channel) => channel |> Lexing.from_channel
    };
  lexbuf |> Parsing.ast_of_lexbuf;
};

let elaborate = (~_opts=default_opts) =>
  Elaborator_Exp.elab(Contexts.initial, Delta.empty);

let transform = (~_opts=default_opts) => Transform.transform;

let linearize = (~_opts=default_opts) => Linearize.linearize;

let grainize = (~_opts=default_opts) => GrainCodegen.codegen;

let print = (~_opts=default_opts) => GrainPrint.print;

let wasmize = (~opts=default_opts, path, g) => {
  let write_temporary = contents => {
    let (outpath, f) = Filename.open_temp_file("hazel", "compile.gr");
    Printf.fprintf(f, "%s\n", contents);
    close_out(f);

    outpath;
  };

  let grain_outpath = write_temporary(g);
  switch (
    Grain.compile(
      ~opts=opts.grain,
      {file: grain_outpath, output: Some(path)},
    )
  ) {
  | Ok(_) => Ok()
  | Error(_) => Error()
  };
};

let next = (~opts=default_opts, path, state) => {
  switch (state) {
  | Source(source) =>
    parse(source)
    |> Result.map(e => Parsed(e))
    |> Result.map_error(err => ParseError(err))
  | Parsed(e) =>
    switch (elaborate(e)) {
    | Elaborates(d, _ty, _delta) => Ok(Elaborated(d))
    | DoesNotElaborate => Error(ElaborateError)
    }
  | Elaborated(d) => Ok(Transformed(transform(d)))
  | Transformed(u) => Ok(Linearized(linearize(u)))
  | Linearized(a) => Ok(Grainized(grainize(a)))
  | Grainized(g) => Ok(Printed(print(g)))
  | Printed(g) =>
    wasmize(~opts, path, g)
    |> Result.map(() => Wasmized(path))
    |> Result.map_error(() => GrainError)
  | Wasmized(path) => Ok(Wasmized(path))
  };
};

[@deriving sexp]
type resume_action =
  | Continue(state)
  | Stop;

let rec resume = (~opts=default_opts, ~hook=?, path, state) => {
  switch (next(~opts, path, state)) {
  | Ok(state) =>
    switch (hook) {
    | Some(hook) =>
      switch (hook(state)) {
      | Continue(state) => resume(~opts, ~hook, path, state)
      | Stop => Ok(state)
      }
    | None =>
      switch (state) {
      | Wasmized(_) => Ok(state)
      | _ => resume(~opts, ~hook?, path, state)
      }
    }
  | Error(err) => Error(err)
  };
};

let stop_after_parsed =
  fun
  | Parsed(_) => Stop
  | state => Continue(state);

let stop_after_elaborated =
  fun
  | Elaborated(_) => Stop
  | state => Continue(state);

let stop_after_transformed =
  fun
  | Transformed(_) => Stop
  | state => Continue(state);

let stop_after_linearized =
  fun
  | Linearized(_) => Stop
  | state => Continue(state);

let stop_after_grainized =
  fun
  | Grainized(_) => Stop
  | state => Continue(state);

let stop_after_printed =
  fun
  | Printed(_) => Stop
  | state => Continue(state);

let stop_after_wasmized =
  fun
  | Wasmized(_) => Stop
  | state => Continue(state);

exception BadState;

let compile_dhexp = (~opts=default_opts, path, source) => {
  switch (resume(~opts, ~hook=stop_after_elaborated, path, Source(source))) {
  | Ok(state) =>
    switch (state) {
    | Elaborated(d) => Ok(d)
    | _ => raise(BadState)
    }
  | Error(err) => Error(err)
  };
};

let compile_ihexp = (~opts=default_opts, path, source) => {
  switch (resume(~opts, ~hook=stop_after_transformed, path, Source(source))) {
  | Ok(state) =>
    switch (state) {
    | Transformed(d) => Ok(d)
    | _ => raise(BadState)
    }
  | Error(err) => Error(err)
  };
};

let compile_anf = (~opts=default_opts, path, source) => {
  switch (resume(~opts, ~hook=stop_after_linearized, path, Source(source))) {
  | Ok(state) =>
    switch (state) {
    | Linearized(d) => Ok(d)
    | _ => raise(BadState)
    }
  | Error(err) => Error(err)
  };
};

let compile_grain = (~opts=default_opts, path, source) => {
  switch (resume(~opts, ~hook=stop_after_printed, path, Source(source))) {
  | Ok(state) =>
    switch (state) {
    | Printed(g) => Ok(g)
    | _ => raise(BadState)
    }
  | Error(err) => Error(err)
  };
};

let compile_wasm = (~opts=default_opts, path, source) => {
  switch (resume(~opts, ~hook=stop_after_wasmized, path, Source(source))) {
  | Ok(state) =>
    switch (state) {
    | Wasmized(path) => Ok(path)
    | _ => raise(BadState)
    }
  | Error(err) => Error(err)
  };
};
