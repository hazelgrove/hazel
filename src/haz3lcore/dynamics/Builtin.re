open Sexplib.Std;

open EvaluatorMonad.Syntax;

/* Evaluator alias. */
[@deriving (show({with_path: false}), sexp, yojson)]
type evaluate =
  (ClosureEnvironment.t, DHExp.t) => EvaluatorMonad.t(EvaluatorResult.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type args = list(DHExp.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type builtin_evaluate =
  (ClosureEnvironment.t, args, evaluate) =>
  EvaluatorMonad.t(EvaluatorResult.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  typ: Typ.t,
  eval: builtin_evaluate,
  elab: DHExp.t,
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
let mk_elab = (name: Var.t, typ: Typ.t): DHExp.t => {
  let rec mk_elab_inner =
          (typ': Typ.t, n: int, bindings: list(Var.t)): DHExp.t => {
    ids: [],
    term:
      switch (typ') {
      | Arrow(_, typ'') =>
        let var = "x" ++ string_of_int(n);
        Fun(
          {ids: [], term: Var(var)},
          Some(typ'),
          mk_elab_inner(typ'', n + 1, [var, ...bindings]),
          Some(name),
        );
      | _ =>
        let bindings =
          List.rev_map(x => DHExp.{ids: [], term: DHExp.Var(x)}, bindings);
        ApBuiltin(name, bindings);
      /*
       I don't really get the definition of ListLit, so I am not sure if it works
       let bindings = List.rev(bindings);
       Ap(
         name, // What is Token.t? I cannot find its implementation through my IDE
         ListLit(
           bindings,
           Some(0, 0, StandardErrStatus(NotInHole), typ') // Again I am not sure what does this mean
         )
       );
       */
      },
  };

  mk_elab_inner(typ, 0, []);
};

let mk = (name: Var.t, typ: Typ.t, eval: builtin_evaluate): t => {
  let elab = mk_elab(name, typ);
  {typ, eval, elab};
};

let mk_zero = (name: Var.t, typ: Typ.t, v: DHExp.t): t => {
  let fn = (env, args, evaluate) => {
    switch (args) {
    | [] => evaluate(env, v)
    | _ => raise(EvaluatorError.Exception(BadBuiltinAp(name, args)))
    };
  };

  mk(name, typ, fn);
};

let mk_one =
    (
      name: Var.t,
      typ: Typ.t,
      fn: (Var.t, EvaluatorResult.t) => EvaluatorMonad.t(EvaluatorResult.t),
    )
    : t => {
  let fn = (env, args, evaluate) => {
    switch (args) {
    | [d1] =>
      let* r1 = evaluate(env, d1);
      fn(name, r1);
    | _ => raise(EvaluatorError.Exception(BadBuiltinAp(name, args)))
    };
  };

  mk(name, typ, fn);
};

let mk_two =
    (
      name: Var.t,
      ty: Typ.t,
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
      fn(name, r1, r2);
    | _ => raise(EvaluatorError.Exception(BadBuiltinAp(name, args)))
    };
  };

  mk(name, ty, fn);
};
