open Sexplib.Std;

open EvaluatorMonad.Syntax;

/* Evaluator alias. */
[@deriving sexp]
type evaluate =
  (ClosureEnvironment.t, DHExp.t) => EvaluatorMonad.t(EvaluatorResult.t);

[@deriving sexp]
type args = list(DHExp.t);

[@deriving sexp]
type builtin_evaluate =
  (ClosureEnvironment.t, args, evaluate) =>
  EvaluatorMonad.t(EvaluatorResult.t);

[@deriving sexp]
type builtin_elaboration = DHExp.t;

[@deriving sexp]
type t = {
  ident: Var.t,
  ty: HTyp.t,
  eval: builtin_evaluate,
  elab: builtin_elaboration,
};

/*
   Build the elaborated DHExp for a built-in function.

   For example:
     mk_elab("mod", Arrow(Int, Arrow(Int, Int)))
       =>
     Lam("x0", Arrow(Int, Arrow(Int, Int)),
       Lam("x1", Arrow(Int, Int),
         ApBuiltin("mod", [BoundVar("x0"), BoundVar("x1")])
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

let mk = (ident: Var.t, ty: HTyp.t, eval: builtin_evaluate): t => {
  let elab = mk_elab(ident, ty);
  {ident, ty, eval, elab};
};

let mk_zero = (ident: Var.t, ty: HTyp.t, v: DHExp.t): t => {
  let fn = (env, args, evaluate) => {
    switch (args) {
    | [] => evaluate(env, v)
    | _ => raise(EvaluatorError.Exception(BadBuiltinAp(ident, args)))
    };
  };

  mk(ident, ty, fn);
};

let mk_one =
    (
      ident: Var.t,
      ty: HTyp.t,
      fn: (Var.t, EvaluatorResult.t) => EvaluatorMonad.t(EvaluatorResult.t),
    )
    : t => {
  let fn = (env, args, evaluate) => {
    switch (args) {
    | [d1] =>
      let* r1 = evaluate(env, d1);
      fn(ident, r1);
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
        (Var.t, EvaluatorResult.t, EvaluatorResult.t) =>
        EvaluatorMonad.t(EvaluatorResult.t),
    )
    : t => {
  let fn = (env, args, evaluate) => {
    switch (args) {
    | [d1, d2] =>
      let* r1 = evaluate(env, d1);
      let* r2 = evaluate(env, d2);
      fn(ident, r1, r2);
    | _ => raise(EvaluatorError.Exception(BadBuiltinAp(ident, args)))
    };
  };

  mk(ident, ty, fn);
};
