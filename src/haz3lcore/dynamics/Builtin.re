open Sexplib.Std;

// open EvaluatorMonad.Syntax;

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
let mk_elab = (name: Var.t, typ: Typ.t): ElaboratorMonad.t(DHExp.t) => {
  let rec mk_elab_inner =
          (typ': Typ.t, n: int, bindings: list(Var.t))
          : ElaboratorMonad.t(DHExp.t) => {
    open ElaboratorMonad.Syntax;
    let* term =
      switch (typ') {
      | Arrow(_, typ'') =>
        let var = "x" ++ string_of_int(n);
        let* r = mk_elab_inner(typ'', n + 1, [var, ...bindings]);
        let* var_id = ElaboratorMonad.with_id(IdGen.fresh);
        let var_ids = CH.Ids.mk([var_id]);
        DHExp.Fun(
          {ids: var_ids, term: Var(var)},
          Some(typ'),
          r,
          Some(name),
        )
        |> ElaboratorMonad.return;
      | _ =>
        let* var_id = ElaboratorMonad.with_id(IdGen.fresh);
        let var_ids = CH.Ids.mk([var_id]);
        let bindings =
          List.rev_map(
            x => DHExp.{ids: var_ids, term: DHExp.Var(x)},
            bindings,
          );
        DHExp.ApBuiltin(name, bindings) |> ElaboratorMonad.return;
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
      };
    let* id = ElaboratorMonad.with_id(IdGen.fresh);
    let ids = [(id, id)];
    DHExp.{ids, term} |> ElaboratorMonad.return;
  };

  mk_elab_inner(typ, 0, []);
};

let mk =
    (name: Var.t, typ: Typ.t, eval: builtin_evaluate): ElaboratorMonad.t(t) => {
  open ElaboratorMonad.Syntax;
  let* elab = mk_elab(name, typ);
  {typ, eval, elab} |> ElaboratorMonad.return;
};

let mk_zero =
    (name: Var.t, typ: Typ.t, v: ElaboratorMonad.t(DHExp.t))
    : ElaboratorMonad.t(t) => {
  open ElaboratorMonad.Syntax;
  let* v = v;
  let fn: builtin_evaluate =
    (env, args, evaluate) => {
      switch (args) {
      | [] => evaluate(env, v)
      | _ => raise(EvaluatorError.Exception(BadBuiltinAp(name, args)))
      };
    };

  mk(name, typ, fn);
};

let mk_one = (name, typ, fn) => {
  open ElaboratorMonad.Syntax;
  let* fn = fn;
  let fn: builtin_evaluate =
    (env, args, evaluate) => {
      EvaluatorMonad.Syntax.(
        switch (args) {
        | [d1] =>
          let* r1 = evaluate(env, d1);
          fn(name, r1);
        | _ => raise(EvaluatorError.Exception(BadBuiltinAp(name, args)))
        }
      );
    };

  mk(name, typ, fn);
};

let mk_two = (name, ty, fn) => {
  open ElaboratorMonad.Syntax;
  let* fn = fn;
  let fn = (env, args, evaluate) => {
    EvaluatorMonad.Syntax.(
      switch (args) {
      | [d1, d2] =>
        let* r1 = evaluate(env, d1);
        let* r2 = evaluate(env, d2);
        fn(name, r1, r2);
      | _ => raise(EvaluatorError.Exception(BadBuiltinAp(name, args)))
      }
    );
  };

  mk(name, ty, fn);
};
