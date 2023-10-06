open Util;
open StepperMonad;
open StepperResult;
open StepperMonad.Syntax;

module Variants = {
  let ap = (d1: DHExp.t, d2: DHExp.t) => DHExp.Ap(d1, d2);
  let bin_int_op = (op, d1, d2) => DHExp.BinIntOp(op, d1, d2);
};

type recursor = {
  require:
    (ClosureEnvironment.t, Instrument.t, DHExp.t) =>
    StepperMonad.t(StepperResult.t(DHExp.t)),
  compute:
    (ClosureEnvironment.t, Instrument.t, DHExp.t) =>
    StepperMonad.t(StepperResult.t(DHExp.t)),
  continue:
    (ClosureEnvironment.t, Instrument.t, DHExp.t) =>
    StepperMonad.t(StepperResult.t(DHExp.t)),
  recurse:
    (ClosureEnvironment.t, Instrument.t, DHExp.t) =>
    StepperMonad.t(StepperResult.t(DHExp.t)),
};

let combine = (r1, r2) => {
  switch (r1, r2) {
  | (Expr(d1), Expr(d2) | Indet(d2) | BoxedValue(d2)) => Expr((d1, d2))
  | (Indet(d1) | BoxedValue(d1), Expr(d2)) => Expr((d1, d2))
  | (Indet(d1), Indet(d2) | BoxedValue(d2)) => Indet((d1, d2))
  | (BoxedValue(d1), Indet(d2)) => Indet((d1, d2))
  | (BoxedValue(d1), BoxedValue(d2)) => BoxedValue((d1, d2))
  };
};

let combind = (r1, r2) => {
  let* r1 = r1;
  let* r2 = r2;
  combine(r1, r2) |> return;
};

type handled = {
  inj: DHExp.t,
  con: StepperMonad.t(StepperResult.t(DHExp.t)),
};

let bind = (r, c) => {
  let* r = r;
  switch (r) {
  | Expr((d, f)) => Expr(f(d)) |> return
  | Indet((d, f)) => Indet(f(d)) |> return
  | BoxedValue(d) => c(d)
  };
};

let ( and* ) = combind;
let ( let* ) = bind;

let rec step =
        (
          {require, compute, continue, recurse},
          env: ClosureEnvironment.t,
          ins: Instrument.t,
          d: DHExp.t,
        )
        : StepperMonad.t(StepperResult.t(DHExp.t)) => {
  switch (d) {
  | BoundVar(x) =>
    let d =
      x
      |> ClosureEnvironment.lookup(env)
      |> OptUtil.get(() => {
           print_endline("FreeInvalidVar: " ++ x);
           raise(EvaluatorError.Exception(FreeInvalidVar(x)));
         });
    recurse(env, ins, d);

  | Ap(d1, d2) =>
    let* d1 = require(env, ins, d1)
    and* d2 = require(env, ins, d2)
    and* _ =
      StepperResult.BoxedValue(((d1, d2)) => DHExp.Ap(d1, d2)) |> return;
    switch (d1) {
    | Constructor(_) => BoxedValue(DHExp.Ap(d1, d2)) |> return
    | Closure(closure_env, Fun(dp, _, d3, _)) =>
      switch (Evaluator.matches(dp, d2)) {
      | DoesNotMatch
      | IndetMatch => Indet(DHExp.Ap(d1, d2)) |> return
      | Matches(env') =>
        closure_env
        |> ClosureEnvironment.map_of
        |> Environment.union(env')
        |> ClosureEnvironment.of_environment
        |> EvaluatorMonad.with_eig
        |> with_eval
        >>= continue(_, ins, d3)
      }
    | _ => Expr(d) |> return
    };

  | BinIntOp(op, d1, d2) =>
    let* r1 = require(env, ins, d1);
    switch (r1) {
    | Expr(d1) =>
      let* r2 = require(env, ins, d2);
      switch (r2) {
      | Expr(d2) => Expr(BinIntOp(op, d1, d2)) |> return
      | Indet(d2) => Expr(BinIntOp(op, d1, d2)) |> return
      | BoxedValue(d2) => Expr(BinIntOp(op, d1, d2)) |> return
      };
    | Indet(d1) =>
      let* r2 = require(env, ins, d2);
      switch (r2) {
      | Expr(d2) => Expr(BinIntOp(op, d1, d2)) |> return
      | Indet(d2) => Indet(BinIntOp(op, d1, d2)) |> return
      | BoxedValue(d2) => Indet(BinIntOp(op, d1, d2)) |> return
      };
    | BoxedValue(IntLit(n1) as d1) =>
      let* r2 = require(env, ins, d2);
      switch (r2) {
      | Expr(d2) => Expr(BinIntOp(op, d1, d2)) |> return
      | Indet(d2) => Indet(BinIntOp(op, d1, d2)) |> return
      | BoxedValue(IntLit(n2) as d2) =>
        switch (op, n1, n2) {
        | (Divide, _, 0) =>
          Indet(InvalidOperation(BinIntOp(op, d1, d2), DivideByZero))
          |> return
        | (Power, _, _) when n2 < 0 =>
          Indet(InvalidOperation(BinIntOp(op, d1, d2), NegativeExponent))
          |> return
        | _ =>
          switch (ins) {
          | Eval =>
            BoxedValue(Evaluator.eval_bin_int_op(op, n1, n2)) |> return
          | Pause => Expr(BinIntOp(op, d1, d2)) |> return
          };
          Evaluator.eval_bin_int_op;
        }
      | _ => Indet(d) |> return
      };
    | _ => Indet(d) |> return
    };
  | _ => Expr(d) |> return
  };
};
