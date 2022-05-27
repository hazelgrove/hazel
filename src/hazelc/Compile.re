open Sexplib.Std;
open SexpResult;

module Parsing = Hazeltext.Parsing;

exception BadState;

[@deriving sexp]
type opts = {
  optimize: Optimize.opts,
  codegen: GrainCodegen.opts,
};

[@deriving sexp]
type grain_opts = Grain.opts;

let _parse = (source: Source.t) =>
  source |> Source.to_lexbuf |> Parsing.ast_of_lexbuf;

let _elaborate = (e: UHExp.t) => {
  let ctx = Contexts.initial;
  (ctx, Elaborator_Exp.elab(ctx, Delta.empty, e));
};

let _transform = (ctx: Contexts.t) => Transform.transform(ctx);

let _linearize = Linearize.linearize;

let _optimize = opts => Optimize.optimize(~opts);

let _grainize = GrainCodegen.codegen;

let _print = GrainPrint.print;

let _wasmize = (~opts, src_path, out_path, g) => {
  let f = open_out(src_path);
  Printf.fprintf(f, "%s\n", g);
  close_out(f);

  switch (Grain.compile(~opts, {file: src_path, output: Some(out_path)})) {
  | Ok(_) => Ok()
  | Error(_) => Error()
  };
};

let parse = (~opts, source) => {
  let _ = opts;
  _parse(source);
};

let elaborate = (~opts, e) => {
  let _ = opts;
  switch (_elaborate(e)) {
  | (ctx, Elaborates(d, _ty, _delta)) => Ok((ctx, d))
  | (_, DoesNotElaborate) => Error()
  };
};

let transform = (~opts, ctx, d) => {
  let _ = opts;
  _transform(ctx, d);
};

let linearize = (~opts, d) => {
  let _ = opts;
  _linearize(d);
};

let optimize = (~opts, a) => _optimize(opts.optimize, a);

let grainize = (~opts, a) => {
  let _ = opts;
  _grainize(~opts=opts.codegen, a);
};

let print = (~opts, g) => {
  let _ = opts;
  _print(g);
};

let wasmize = (~opts, src_path, out_path, g) =>
  _wasmize(~opts, src_path, out_path, g);

[@deriving sexp]
type state =
  | Source(Source.t)
  | Parsed(UHExp.t)
  | Elaborated(Contexts.t, DHExp.t)
  | Transformed(Hir.expr)
  | Linearized(Anf.prog)
  | Optimized(Anf.prog)
  | Grainized(GrainIR.prog)
  | Printed(string);

[@deriving sexp]
type next_error =
  | ParseError(string)
  | ElaborateError;

[@deriving sexp]
type next_result = result(option(state), next_error);

let next = (~opts, state): next_result => {
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

  | Linearized(a) => Ok(Some(Optimized(optimize(~opts, a))))

  | Optimized(a) => Ok(Some(Grainized(grainize(~opts, a))))

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

let stop_after_optimized =
  fun
  | Optimized(_) => Stop
  | state => Continue(state);

let stop_after_grainized =
  fun
  | Grainized(_) => Stop
  | state => Continue(state);

let stop_after_printed =
  fun
  | Printed(_) => Stop
  | state => Continue(state);

let rec resume = (~opts, ~hook=stop_after_printed, state) => {
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

let resume_until_elaborated = (~opts, state) => {
  switch (resume(~opts, ~hook=stop_after_elaborated, state)) {
  | Ok(Some(Elaborated(_, d))) => Ok(d)
  | Ok(_) => raise(BadState)
  | Error(err) => Error(err)
  };
};

let resume_until_transformed = (~opts, state) => {
  switch (resume(~opts, ~hook=stop_after_transformed, state)) {
  | Ok(Some(Transformed(d))) => Ok(d)
  | Ok(_) => raise(BadState)
  | Error(err) => Error(err)
  };
};

let resume_until_linearized = (~opts, state) => {
  switch (resume(~opts, ~hook=stop_after_linearized, state)) {
  | Ok(Some(Linearized(a))) => Ok(a)
  | Ok(_) => raise(BadState)
  | Error(err) => Error(err)
  };
};

let resume_until_optimized = (~opts, state) => {
  switch (resume(~opts, ~hook=stop_after_optimized, state)) {
  | Ok(Some(Optimized(a))) => Ok(a)
  | Ok(_) => raise(BadState)
  | Error(err) => Error(err)
  };
};

let resume_until_grainized = (~opts, state) => {
  switch (resume(~opts, ~hook=stop_after_grainized, state)) {
  | Ok(Some(Grainized(g))) => Ok(g)
  | Ok(_) => raise(BadState)
  | Error(err) => Error(err)
  };
};

let resume_until_printed = (~opts, state) => {
  switch (resume(~opts, ~hook=stop_after_printed, state)) {
  | Ok(Some(Printed(g))) => Ok(g)
  | Ok(_) => raise(BadState)
  | Error(err) => Error(err)
  };
};
