open Sexplib.Std;

/* Evaluator alias. */
[@deriving sexp]
type evaluate = (DHExp.t, EvaluatorResult.state) => EvaluatorResult.t;

[@deriving sexp]
type args = list(DHExp.t);

[@deriving sexp]
type eval = (args, evaluate, EvaluatorResult.state) => EvaluatorResult.t;

[@deriving sexp]
type elab = DHExp.t;

[@deriving sexp]
type t = {
  ident: Var.t,
  ty: HTyp.t,
  eval,
  elab,
};

/*
   Build the elaborated DHExp for a built-in function.

   For example:
   mk_elab("mod", Arrow(Int, Arrow(Int, Int)))
   =>
   Lam("x0", Arrow(Int, Arrow(Int, Int)),
   Lam("x1", Arrow(Int, Int),
   Apbuilt-in("mod", [BoundVar("x0"), BoundVar("x1")])
   )
   )
 */
let mk_elab = (ident: Var.t, ty: HTyp.t): DHExp.t => {
  let rec mk_elab_inner =
          (ty': HTyp.t, n: int, bindings: list(Var.t)): DHExp.t => {
    switch (ty') {
    | Arrow(_, ty'') =>
      let var = "x" ++ string_of_int(n);
      Fun(Var(var), ty', mk_elab_inner(ty'', n + 1, [var, ...bindings]));
    | _ =>
      let bindings = List.rev_map(x => DHExp.BoundVar(x), bindings);
      ApBuiltin(ident, bindings);
    };
  };

  mk_elab_inner(ty, 0, []);
};

let mk = (ident: Var.t, ty: HTyp.t, eval: eval): t => {
  let elab = mk_elab(ident, ty);
  {ident, ty, eval, elab};
};

let mk_zero = (ident: Var.t, ty: HTyp.t, v: DHExp.t): t => {
  let fn = (args, evaluate, state) => {
    switch (args) {
    | [] => evaluate(v, state)
    | _ => raise(EvaluatorError.Exception(BadBuiltinAp(ident, args)))
    };
  };

  mk(ident, ty, fn);
};

let mk_one =
    (
      ident: Var.t,
      ty: HTyp.t,
      fn: (Var.t, EvaluatorResult.t) => EvaluatorResult.t,
    )
    : t => {
  let fn = (args, evaluate, state) => {
    switch (args) {
    | [d1] =>
      let r1' = evaluate(d1, state);
      fn(ident, r1');
    | _ => raise(EvaluatorError.Exception(BadBuiltinAp(ident, args)))
    };
  };

  mk(ident, ty, fn);
};

let mk_two =
    (
      ident: Var.t,
      ty: HTyp.t,
      fn:
        (
          Var.t,
          (EvaluatorResult.exp, EvaluatorResult.exp, EvaluatorResult.state)
        ) =>
        EvaluatorResult.t,
    )
    : t => {
  let fn = (args, evaluate, state) => {
    switch (args) {
    | [d1, d2] =>
      let (r1, state) = evaluate(d1, state);
      let (r2, state) = evaluate(d2, state);
      fn(ident, (r1, r2, state));
    | _ => raise(EvaluatorError.Exception(BadBuiltinAp(ident, args)))
    };
  };

  mk(ident, ty, fn);
};
