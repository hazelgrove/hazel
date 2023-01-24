/*
   Built-in functions for Hazel.

   To add a built-in function or constant, write the implementation in the
   `Pervasives.Impls` module below and add it to `Pervasives`.

   See the existing ones for reference.
 */

open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = VarMap.t_(Builtin.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type forms = VarMap.t_((DHExp.t, Builtin.builtin_evaluate));

let traverse: (('a, 's) => ('s, 'b), list('a), 's) => (list('b), 's) =
  (f, l, accu) => {
    let f: ('s, 'a) => ('s, 'b) =
      (a, b) => {
        f(b, a);
      };
    let (s, r) = List.fold_left_map(f, accu, l);
    (r, s);
  };

let ctx = (builtins: ElaboratorMonad.t(t)): Ctx.t => {
  let es: ElaboratorState.t = {id: Id.init(BuiltinElab)};
  let (_, builtins) = builtins(es);
  let f = ((name, Builtin.{typ, _})) => {
    open ElaboratorMonad.Syntax;
    let+ id = ElaboratorMonad.with_id(IdGen.fresh);
    Ctx.VarEntry({name, typ, id});
  };
  let (ctx, _) = traverse(f, builtins, es);
  ctx;
};

let elabs = builtins => {
  open ElaboratorMonad.Syntax;
  let* builtins = builtins;
  List.map(
    ((name, Builtin.{typ: _, eval, elab})) => (name, (elab, eval)),
    builtins,
  )
  |> ElaboratorMonad.return;
};

let forms = builtins => {
  let es: ElaboratorState.t = {id: Id.init(BuiltinEval)};
  let (_, builtins) = builtins(es);
  List.map(
    ((name, Builtin.{typ: _, eval, elab})) => (name, (elab, eval)),
    builtins,
  );
};

let using =
    (
      name: Var.t,
      impl: Var.t => ElaboratorMonad.t(Builtin.t),
      builtins: ElaboratorMonad.t(t),
    )
    : ElaboratorMonad.t(t) => {
  open ElaboratorMonad.Syntax;
  let* builtins = builtins;
  let+ r = impl(name);
  VarMap.extend(builtins, (name, r));
};

let ids_derive = (ids: CH.Ids.t): IdGen.t(CH.Ids.t) => {
  open IdGen.Syntax;
  let f = ((base: Id.t, _: Id.t)): IdGen.t((Id.t, Id.t)) => {
    let+ derived = IdGen.fresh;
    (base, derived);
  };
  ListUtil.traverse(f, ids);
};

module Pervasives = {
  module Impls = {
    open EvaluatorMonad;
    open EvaluatorResult;
    open EvaluatorMonad.Syntax;

    /* int_of_float implementation. */
    let int_of_float =
      ElaboratorMonad.return((name, r1) =>
        switch (r1) {
        | BoxedValue({ids, term: Float(f)}) =>
          let i = int_of_float(f);
          BoxedValue({ids, term: Int(i)}) |> return;
        | BoxedValue(d1) =>
          raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1)))
        | Indet(d1) =>
          let* ids = with_id(ids_derive(d1.ids));
          Indet({ids, term: ApBuiltin(name, [d1])}) |> return;
        }
      );

    /* float_of_int implementation. */
    let float_of_int =
      ElaboratorMonad.return((name, r1) =>
        switch (r1) {
        | BoxedValue({ids, term: Int(i)}) =>
          let f = float_of_int(i);
          BoxedValue({ids, term: Float(f)}) |> return;
        | BoxedValue(d1) =>
          raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1)))
        | Indet(d1) =>
          let* ids = with_id(ids_derive(d1.ids));
          Indet({ids, term: ApBuiltin(name, [d1])}) |> return;
        }
      );

    /* mod implementation */
    let int_mod =
      ElaboratorMonad.return((name, r1) =>
        switch (r1) {
        | BoxedValue(
            {ids, term: Tuple([{term: Int(n), _}, {term: Int(m), _}])} as d1,
          ) =>
          let* ids1 = with_id(ids_derive(ids));
          switch (m) {
          | 0 =>
            let* ids2 = with_id(ids_derive(d1.ids));
            Indet({
              ids: ids2,
              term:
                Hole(
                  None,
                  InvalidOperation(
                    DivideByZero,
                    {ids: ids1, term: ApBuiltin(name, [d1])},
                  ),
                ),
            })
            |> return;
          | _ => return(BoxedValue({ids: ids1, term: Int(n mod m)}))
          };
        | BoxedValue(d1) =>
          raise(EvaluatorError.Exception(InvalidBoxedTuple(d1)))
        | Indet(d1) =>
          let* ids = with_id(ids_derive(d1.ids));
          return(Indet({ids, term: ApBuiltin(name, [d1])}));
        }
      );

    /* PI implementation. */
    let pi: ElaboratorMonad.t(DHExp.t) = {
      open ElaboratorMonad.Syntax;
      let* id = ElaboratorMonad.with_id(IdGen.fresh);
      DHExp.{ids: CH.Ids.mk([id]), term: Float(Float.pi)}
      |> ElaboratorMonad.return;
    };
  };

  let pi = name => Builtin.mk_zero(name, Float, Impls.pi);
  let int_of_float = name =>
    Builtin.mk_one(name, Arrow(Float, Int), Impls.int_of_float);
  let float_of_int = name =>
    Builtin.mk_one(name, Arrow(Int, Float), Impls.float_of_int);
  let modulo = name =>
    Builtin.mk_one(name, Arrow(Prod([Int, Int]), Int), Impls.int_mod);

  let builtins = {
    VarMap.empty
    |> ElaboratorMonad.return
    |> using("pi", pi)
    |> using("int_of_float", int_of_float)
    |> using("float_of_int", float_of_int)
    |> using("mod", modulo);
  };
};
