[@deriving sexp]
type evaluate =
  (EvalState.t, EvalEnv.t, DHExp.t) => (EvalState.t, EvaluatorResult.t);

[@deriving sexp]
type args = list(DHExp.t);

[@deriving sexp]
type eval =
  (EvalState.t, EvalEnv.t, args, evaluate) => (EvalState.t, EvaluatorResult.t);

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
   Create a built-in function.
 */
let mk: (Var.t, HTyp.t, eval) => t;

/*
   Create a built-in constant.
 */
let mk_zero: (Var.t, HTyp.t, DHExp.t) => t;

/*
   Create a built-in function that takes a single argument. The given type
   must be correct.
 */
let mk_one:
  (Var.t, HTyp.t, (Var.t, EvaluatorResult.t) => EvaluatorResult.t) => t;

/*
   Create a built-in function that takes two arguments. The given type must be
   correct.
 */
let mk_two:
  (
    Var.t,
    HTyp.t,
    (Var.t, EvaluatorResult.t, EvaluatorResult.t) => EvaluatorResult.t
  ) =>
  t;
