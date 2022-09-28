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
  ty: Typ.t,
  eval: builtin_evaluate,
  elab: builtin_elaboration,
};

/*
   Create a built-in function.
 */
let mk: (Var.t, Typ.t, builtin_evaluate) => t;

/*
   Create a built-in constant.
 */
let mk_zero: (Var.t, Typ.t, DHExp.t) => t;

/*
   Create a built-in function that takes a single argument. The given type
   must be correct.
 */
let mk_one:
  (
    Var.t,
    Typ.t,
    (Var.t, EvaluatorResult.t) => EvaluatorMonad.t(EvaluatorResult.t)
  ) =>
  t;

/*
   Create a built-in function that takes two arguments. The given type must be
   correct.
 */
let mk_two:
  (
    Var.t,
    Typ.t,
    (Var.t, EvaluatorResult.t, EvaluatorResult.t) =>
    EvaluatorMonad.t(EvaluatorResult.t)
  ) =>
  t;
