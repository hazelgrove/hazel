open Sexplib.Std;

open ResultSexp;
open Hazeltext;

[@deriving sexp]
type opts = {
  optimize: Mir.Optimize.opts,
  codegen: Codegen.Grain.opts,
};

let parse' = (source: Source.t) =>
  source |> Source.to_lexbuf |> Parsing.ast_of_lexbuf;

let elaborate' = (e: UHExp.t) => {
  let ctx = Contexts.initial;
  (ctx, Elaborator_Exp.elab(ctx, Delta.empty, e));
};

let transform' = ctx => Hir.Transform.transform(ctx);

let linearize' = Mir.Linearize.linearize;

let optimize' = opts => Mir.Optimize.optimize(~opts);

let grainize' = opts => Codegen.Grain.codegen(~opts);

let print' = Grain.print;

[@deriving sexp]
type wasm_opts = {
  grain: string,
  includes: list(string),
  wat: bool,
  maximum_memory_pages: int,
  release: bool,
};

let wasmize' = (opts: wasm_opts, source, output, g) => {
  // Write Grain to source path.
  {
    open ChannelUtil.Syntax;
    let&o f = open_out(source);
    Printf.fprintf(f, "%s\n", g);
  };

  // TODO: Add necessary includes.
  // TODO: Add option for alternative stdlib path.
  let cmd =
    Grain.Cli.Compile.(
      Grain.Cli.make(~grain=opts.grain)
      |> make(~source)
      |> with_output(output)
      |> with_includes(opts.includes)
      |> with_wat(opts.wat)
      |> with_maximum_memory_pages(opts.maximum_memory_pages)
      |> with_release(opts.release)
      |> to_command
    );

  switch (cmd |> Grain.Cli.execute(~capture_stdout=false)) {
  | {stdout: _, status: Ok(_)} => Ok()
  | {stdout: _, status: Error(_)} => Error()
  };
};

let parse = (~opts, source) => {
  let _ = opts;
  parse'(source);
};

let elaborate = (~opts, e) => {
  let _ = opts;
  switch (elaborate'(e)) {
  | (ctx, Elaborates(d, _ty, delta)) => Ok((ctx, delta, d))
  | (_, DoesNotElaborate) => Error()
  };
};

let transform = (~opts, ctx, _delta, d) => {
  let _ = opts;
  /* TODO: Transform delta and perform check that transform' is correct. */
  transform'(ctx, d);
};

let linearize = (~opts, d) => {
  let _ = opts;
  linearize'(d);
};

let optimize = (~opts, a) => optimize'(opts.optimize, a);

let grainize = (~opts, a) => grainize'(opts.codegen, a);

let print = (~opts, g) => {
  let _ = opts;
  print'(g);
};

let wasmize = (~opts, ~source, ~output, g) =>
  wasmize'(opts, source, output, g);

[@deriving sexp]
type state =
  | Source(Source.t)
  | Parsed(UHExp.t)
  | Elaborated(Contexts.t, Delta.t, DHExp.t)
  | Transformed(Hir.Expr.expr)
  | Linearized(Mir.Anf.block)
  | Optimized(Mir.Anf.block)
  | Grainized(Grain.prog)
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
    |> Result.map(((ctx, delta, d)) => Some(Elaborated(ctx, delta, d)))
    |> Result.map_error(() => ElaborateError)

  | Elaborated(ctx, delta, d) =>
    Ok(Some(Transformed(transform(~opts, ctx, delta, d))))

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
  | Ok(Some(Elaborated(_, _, d))) => Ok(d)
  | Error(err) => Error(err)
  | Ok(_) =>
    failwith("resume_until_elaborated did not stop with Elaborated state")
  };
};

let resume_until_transformed = (~opts, state) => {
  switch (resume(~opts, ~hook=stop_after_transformed, state)) {
  | Ok(Some(Transformed(d))) => Ok(d)
  | Error(err) => Error(err)
  | Ok(_) =>
    failwith("resume_until_transformed did not stop with Transformed state")
  };
};

let resume_until_linearized = (~opts, state) => {
  switch (resume(~opts, ~hook=stop_after_linearized, state)) {
  | Ok(Some(Linearized(a))) => Ok(a)
  | Error(err) => Error(err)
  | Ok(_) =>
    failwith("resume_until_linearized did not stop with Linearized state")
  };
};

let resume_until_optimized = (~opts, state) => {
  switch (resume(~opts, ~hook=stop_after_optimized, state)) {
  | Ok(Some(Optimized(a))) => Ok(a)
  | Error(err) => Error(err)
  | Ok(_) =>
    failwith("resume_until_optimized did not stop with Optimized state")
  };
};

let resume_until_grainized = (~opts, state) => {
  switch (resume(~opts, ~hook=stop_after_grainized, state)) {
  | Ok(Some(Grainized(g))) => Ok(g)
  | Error(err) => Error(err)
  | Ok(_) =>
    failwith("resume_until_grainized did not stop with Grainized state")
  };
};

let resume_until_printed = (~opts, state) => {
  switch (resume(~opts, ~hook=stop_after_printed, state)) {
  | Ok(Some(Printed(g))) => Ok(g)
  | Error(err) => Error(err)
  | Ok(_) => failwith("resume_until_printed did not stop with Printed state")
  };
};
