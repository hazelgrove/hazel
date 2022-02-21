open Sexplib.Std;

[@deriving sexp]
type args = list(DHExp.t);

[@deriving sexp]
type form = (
  /* eval: */ (args, DHExp.t => EvaluatorResult.t) => EvaluatorResult.t,
  /* elab: */ DHExp.t,
);

[@deriving sexp]
type t = (Var.t, HTyp.t, form);

module Impl = {
  [@deriving sexp]
  type f =
    (
      /* args: */ args,
      /* evaluate: */ DHExp.t => EvaluatorResult.t,
      /* exp: */ DHExp.t
    ) =>
    EvaluatorResult.t;

  let mk_elab = (ident: Var.t, ty: HTyp.t): DHExp.t => {
    let rec mk_elab_inner =
            (ty': HTyp.t, n: int, bindings: list(Var.t)): DHExp.t => {
      switch (ty') {
      | Arrow(_, ty'') =>
        let var = "x" ++ string_of_int(n);
        Lam(Var(var), ty', mk_elab_inner(ty'', n + 1, [var, ...bindings]));
      | _ =>
        let bindings = List.rev_map(x => DHExp.BoundVar(x), bindings);
        ApBuiltin(ident, bindings);
      };
    };

    mk_elab_inner(ty, 0, []);
  };

  let mk = (ident: Var.t, ty: HTyp.t, fn: f): t => {
    let form = {
      let eval = (args, evaluate) =>
        fn(args, evaluate, DHExp.ApBuiltin(ident, args));
      let elab = mk_elab(ident, ty);
      (eval, elab);
    };

    (ident, ty, form);
  };

  let mk_zero = (ident: Var.t, ty: HTyp.t, v: DHExp.t): t => {
    let fn = (args, evaluate, _d) => {
      switch (args) {
      | [] => evaluate(v)
      | _ => raise(EvaluatorError.Exception(BadBuiltinAp(ident, args)))
      };
    };

    mk(ident, ty, fn);
  };

  let mk_one =
      (
        ident: Var.t,
        ty: HTyp.t,
        fn: (EvaluatorResult.t, DHExp.t) => EvaluatorResult.t,
      )
      : t => {
    let fn = (args, evaluate, d) => {
      switch (args) {
      | [d1] =>
        let d1' = evaluate(d1);
        fn(d1', d);
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
          (EvaluatorResult.t, EvaluatorResult.t, DHExp.t) => EvaluatorResult.t,
      )
      : t => {
    let fn = (args, evaluate, d) => {
      switch (args) {
      | [d1, d2] =>
        let d1' = evaluate(d1);
        let d2' = evaluate(d2);
        fn(d1', d2', d);
      | _ => raise(EvaluatorError.Exception(BadBuiltinAp(ident, args)))
      };
    };

    mk(ident, ty, fn);
  };
};

module Impls = {
  open EvaluatorResult;

  let int_of_float = (d1', d) =>
    switch (d1') {
    | BoxedValue(FloatLit(f)) =>
      let i = int_of_float(f);
      BoxedValue(IntLit(i));
    | _ => Indet(d)
    };

  let float_of_int = (d1', e) =>
    switch (d1') {
    | BoxedValue(IntLit(i)) =>
      let f = float_of_int(i);
      BoxedValue(FloatLit(f));
    | _ => Indet(e)
    };

  let int_mod = (d1', d2', e) =>
    switch (d1', d2') {
    | (BoxedValue(IntLit(n)), BoxedValue(IntLit(m))) =>
      BoxedValue(IntLit(n mod m))
    | _ => Indet(e)
    };

  let pi = DHExp.FloatLit(Float.pi);
};

let builtins = [
  Impl.mk_zero("PI", Float, Impls.pi),
  Impl.mk_one("int_of_float", Arrow(Float, Int), Impls.int_of_float),
  Impl.mk_one("float_of_int", Arrow(Int, Float), Impls.float_of_int),
  Impl.mk_two("mod", Arrow(Int, Arrow(Int, Int)), Impls.int_mod),
];

let ctx = List.map(((ident, ty, _)) => (ident, ty), builtins);
let forms = List.map(((ident, _, form)) => (ident, form), builtins);

let lookup_type = VarMap.lookup(ctx);
let lookup_form = VarMap.lookup(forms);
