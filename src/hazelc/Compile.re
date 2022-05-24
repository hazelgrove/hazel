open Sexplib.Std;
open SexpResult;

module Parsing = Hazeltext.Parsing;

exception BadState;

[@deriving sexp]
type grain_opts = Grain.opts;

[@deriving sexp]
type opts = {grain: grain_opts};

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

let _parse = (source: Source.t) =>
  source |> Source.to_lexbuf |> Parsing.ast_of_lexbuf;

let _elaborate = (e: UHExp.t) => {
  let ctx = Contexts.initial;
  (ctx, Elaborator_Exp.elab(ctx, Delta.empty, e));
};

let _transform = (ctx: Contexts.t) => Transform.transform(ctx);

let _linearize = Linearize.linearize;

let _grainize = GrainCodegen.codegen;

let _print = GrainPrint.print;

let _wasmize = (~opts=default_opts, src_path, out_path, g) => {
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

let parse = (~opts=default_opts, source) => {
  let _ = opts;
  _parse(source);
};

let elaborate = (~opts=default_opts, e) => {
  let _ = opts;
  switch (_elaborate(e)) {
  | (ctx, Elaborates(d, _ty, _delta)) => Ok((ctx, d))
  | (_, DoesNotElaborate) => Error()
  };
};

let transform = (~opts=default_opts, ctx, d) => {
  let _ = opts;
  _transform(ctx, d);
};

let linearize = (~opts=default_opts, d) => {
  let _ = opts;
  _linearize(d);
};

let grainize = (~opts=default_opts, a) => {
  let _ = opts;
  _grainize(a);
};

let print = (~opts=default_opts, g) => {
  let _ = opts;
  _print(g);
};

let wasmize = (~opts=default_opts, src_path, out_path, g) =>
  _wasmize(~opts, src_path, out_path, g);

[@deriving sexp]
type state =
  | Source(Source.t)
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
type next_result = result(option(state), next_error);

let next = (~opts=default_opts, state): next_result => {
  switch (state) {
  | Source(source) =>
    parse(~opts, source)
    |> Result.map(e => Some(Parsed(e)))
    |> Result.map_error(err => ParseError(err))

  | Parsed(e) =>
    elaborate(~opts, e)
    |> Result.map(((ctx, d)) => Some(Elaborated(ctx, d)))
    |> Result.map_error(() => ElaborateError)

  | Elaborated(ctx, d) => Ok(Some(Transformed(transform(~opts, ctx, d))))

  | Transformed(u) => Ok(Some(Linearized(linearize(~opts, u))))

  | Linearized(a) => Ok(Some(Grainized(grainize(~opts, a))))

  | Grainized(g) => Ok(Some(Printed(print(~opts, g))))

  | Printed(_) => Ok(None)
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
  | Ok(Some(state)) =>
    switch (hook(state)) {
    | Continue(state) => resume(~opts, ~hook, state)
    | Stop => Ok(Some(state))
    }
  | Ok(None) => Ok(None)
  | Error(err) => Error(err)
  };
};

let resume_until_dhexp = (~opts=default_opts, state) => {
  switch (resume(~opts, ~hook=stop_after_elaborated, state)) {
  | Ok(Some(Elaborated(_, d))) => Ok(d)
  | Ok(_) => raise(BadState)
  | Error(err) => Error(err)
  };
};

let resume_until_hir = (~opts=default_opts, state) => {
  switch (resume(~opts, ~hook=stop_after_transformed, state)) {
  | Ok(Some(Transformed(d))) => Ok(d)
  | Ok(_) => raise(BadState)
  | Error(err) => Error(err)
  };
};

let resume_until_anf = (~opts=default_opts, state) => {
  switch (resume(~opts, ~hook=stop_after_linearized, state)) {
  | Ok(Some(Linearized(d))) => Ok(d)
  | Ok(_) => raise(BadState)
  | Error(err) => Error(err)
  };
};

let resume_until_grain = (~opts=default_opts, state) => {
  switch (resume(~opts, ~hook=stop_after_grainized, state)) {
  | Ok(Some(Grainized(g))) => Ok(g)
  | Ok(_) => raise(BadState)
  | Error(err) => Error(err)
  };
};

let resume_until_grain_text = (~opts=default_opts, state) => {
  switch (resume(~opts, ~hook=stop_after_printed, state)) {
  | Ok(Some(Printed(g))) => Ok(g)
  | Ok(_) => raise(BadState)
  | Error(err) => Error(err)
  };
};
