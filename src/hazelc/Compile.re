open Sexplib.Std;
open SexpResult;

module Parsing = Hazeltext.Parsing;

exception BadState;

[@deriving sexp]
type grain_opts = Grain.opts;

[@deriving sexp]
type opts = {grain: grain_opts};

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

let default_opts = {
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

let elaborate = (e: UHExp.t) => {
  let ctx = Contexts.initial;
  (ctx, Elaborator_Exp.elab(ctx, Delta.empty, e));
};

let transform = (ctx: Contexts.t) => Transform.transform(ctx);

let linearize = Linearize.linearize;

let grainize = GrainCodegen.codegen;

let print = GrainPrint.print;

let wasmize = (~opts=default_opts, src_path, out_path, g) => {
  let f = open_out(src_path);
  Printf.fprintf(f, "%s\n", g);
  close_out(f);

  switch (
    Grain.compile(
      ~opts=opts.grain,
      {file: src_path, output: Some(out_path)},
    )
  ) {
  | Ok(_) => Ok()
  | Error(_) => Error()
  };
};

let parse_next = (~opts=default_opts, source) => {
  let _ = opts;
  parse(source);
};

let elaborate_next = (~opts=default_opts, e) => {
  let _ = opts;
  switch (elaborate(e)) {
  | (ctx, Elaborates(d, _ty, _delta)) => Ok((ctx, d))
  | (_, DoesNotElaborate) => Error()
  };
};

let transform_next = (~opts=default_opts, ctx, d) => {
  let _ = opts;
  transform(ctx, d);
};

let linearize_next = (~opts=default_opts, d) => {
  let _ = opts;
  linearize(d);
};

let grainize_next = (~opts=default_opts, a) => {
  let _ = opts;
  grainize(a);
};

let print_next = (~opts=default_opts, g) => {
  let _ = opts;
  print(g);
};

let wasmize_next = (~opts=default_opts, src_path, out_path, g) =>
  wasmize(~opts, src_path, out_path, g);

[@deriving sexp]
type state =
  | Source(source)
  | Parsed(UHExp.t)
  | Elaborated(Contexts.t, DHExp.t)
  | Transformed(Hir.expr)
  | Linearized(Anf.prog)
  | Grainized(GrainIR.prog)
  | Printed(string);

[@deriving sexp]
type next_error =
  | ParseError(string)
  | ElaborateError;

[@deriving sexp]
type next_result = result(state, next_error);

let next = (~opts=default_opts, state) => {
  switch (state) {
  | Source(source) =>
    parse_next(~opts, source)
    |> Result.map(e => Parsed(e))
    |> Result.map_error(err => ParseError(err))

  | Parsed(e) =>
    elaborate_next(~opts, e)
    |> Result.map(((ctx, d)) => Elaborated(ctx, d))
    |> Result.map_error(() => ElaborateError)

  | Elaborated(ctx, d) => Ok(Transformed(transform(ctx, d)))

  | Transformed(u) => Ok(Linearized(linearize(u)))

  | Linearized(a) => Ok(Grainized(grainize(a)))

  | Grainized(g) => Ok(Printed(print(g)))

  | Printed(g) => Ok(Printed(g))
  };
};

[@deriving sexp]
type resume_action =
  | Continue(state)
  | Stop;

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

let rec resume = (~opts=default_opts, ~hook=stop_after_printed, state) => {
  switch (next(~opts, state)) {
  | Ok(state) =>
    switch (hook(state)) {
    | Continue(state) => resume(~opts, ~hook, state)
    | Stop => Ok(state)
    }
  | Error(err) => Error(err)
  };
};

let resume_until_dhexp = (~opts=default_opts, state) => {
  switch (resume(~opts, ~hook=stop_after_elaborated, state)) {
  | Ok(state) =>
    switch (state) {
    | Elaborated(_, d) => Ok(d)
    | _ => raise(BadState)
    }
  | Error(err) => Error(err)
  };
};

let resume_until_hir = (~opts=default_opts, state) => {
  switch (resume(~opts, ~hook=stop_after_transformed, state)) {
  | Ok(state) =>
    switch (state) {
    | Transformed(d) => Ok(d)
    | _ => raise(BadState)
    }
  | Error(err) => Error(err)
  };
};

let resume_until_anf = (~opts=default_opts, state) => {
  switch (resume(~opts, ~hook=stop_after_linearized, state)) {
  | Ok(state) =>
    switch (state) {
    | Linearized(d) => Ok(d)
    | _ => raise(BadState)
    }
  | Error(err) => Error(err)
  };
};

let resume_until_grain = (~opts=default_opts, state) => {
  switch (resume(~opts, ~hook=stop_after_grainized, state)) {
  | Ok(state) =>
    switch (state) {
    | Grainized(g) => Ok(g)
    | _ => raise(BadState)
    }
  | Error(err) => Error(err)
  };
};

let resume_until_grain_text = (~opts=default_opts, state) => {
  switch (resume(~opts, ~hook=stop_after_printed, state)) {
  | Ok(state) =>
    switch (state) {
    | Printed(g) => Ok(g)
    | _ => raise(BadState)
    }
  | Error(err) => Error(err)
  };
};
