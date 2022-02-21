open EvaluatorResult;

module Impl = {
  [@deriving sexp]
  type t =
    (
      /* args: */ list(DHExp.t),
      /* evaluate: */ DHExp.t => EvaluatorResult.t
    ) =>
    EvaluatorResult.t;

  let mk_one_arg = (f, name, ident, args, evaluate) => {
    let e = DHExp.ApBuiltin(ident, args);
    switch (args) {
    | [] => raise(EvaluatorError.Exception(BadBuiltinAp(name, args)))
    | [d1, ..._] =>
      let d1' = evaluate(d1);
      f(d1', e);
    };
  };
};

module Impls = {
  let int_of_float =
    (
      (d1', e) =>
        switch (d1') {
        | BoxedValue(FloatLit(f)) =>
          let i = int_of_float(f);
          BoxedValue(IntLit(i));
        | _ => Indet(e)
        }
    )
    |> Impl.mk_one_arg;

  let float_of_int =
    (
      (d1', e) =>
        switch (d1') {
        | BoxedValue(IntLit(i)) =>
          let f = float_of_int(i);
          BoxedValue(FloatLit(f));
        | _ => Indet(e)
        }
    )
    |> Impl.mk_one_arg;
};

let define = (name: string, ty: HTyp.t, impl) => (name, (ty, impl(name)));

let builtins = [
  define("int_of_float", Arrow(Float, Int), Impls.int_of_float),
  define("float_of_int", Arrow(Int, Float), Impls.float_of_int),
];

let ctx: VarCtx.t = List.map(((x, (ty, _))) => (x, ty), builtins);

let impls: VarMap.t_(Impl.t) =
  List.map(((x, (_, impl))) => (x, impl(x)), builtins);

let lookup_type = x => VarMap.lookup(ctx, x);

let lookup_impl = x =>
  VarMap.lookup(builtins, x) |> Option.map(((_, impl)) => impl(x));
