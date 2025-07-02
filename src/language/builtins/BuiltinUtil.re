open Util;

type const = {
  name: Var.t,
  typ: Typ.term,
  imp: DHExp.t,
};

type fn = {
  name: Var.t,
  arg: Typ.term,
  ret: Typ.term,
  imp: DHExp.t => option(DHExp.t),
};

type hazel_fn = {
  name: string,
  arg: Typ.term,
  ret: Typ.term,
  str: string,
  imp: Exp.t,
};

[@deriving (show({with_path: false}), sexp)]
type builtin =
  | Const(Typ.t, DHExp.t)
  | Fn(Typ.t, Typ.t, DHExp.t => option(DHExp.t))
  | HazelFn(Typ.t, Typ.t, Exp.t);

[@deriving (show({with_path: false}), sexp)]
type t = VarMap.t_(builtin);

[@deriving (show({with_path: false}), sexp)]
type forms = VarMap.t_(DHExp.t => option(DHExp.t));

exception BuiltinAlreadyDefined(Var.t);

let (let-unbox) = ((request, v), f) =>
  switch (Unboxing.unbox(request, v)) {
  | IndetMatch
  | DoesNotMatch => None
  | Matches(n) => f(n)
  };

// Like VarMap.extend but it fails if the name is already bound
let extend = (builtins: t, (name: Var.t, v: builtin)): t =>
  if (VarMap.contains(builtins, name)) {
    raise(BuiltinAlreadyDefined(name));
  } else {
    VarMap.extend(builtins, (name, v));
  };

let of_list_const = (builtins: list(const)): t =>
  List.fold_left(
    (builtins, {name, typ, imp}) =>
      extend(builtins, (name, Const(typ |> Typ.fresh, imp))),
    VarMap.empty,
    builtins,
  );

let of_list_fn = (builtins: list(fn)): t =>
  List.fold_left(
    (builtins, {name, arg, ret, imp}: fn) =>
      extend(builtins, (name, Fn(arg |> Typ.fresh, ret |> Typ.fresh, imp))),
    VarMap.empty,
    builtins,
  );

// Like VarMap.concat but it fails if the name is already bound
let concat = (builtins: t, new_builtins: t): t => {
  List.iter(
    ((new_builtin, _)) =>
      if (VarMap.contains(builtins, new_builtin)) {
        raise(BuiltinAlreadyDefined(new_builtin));
      },
    new_builtins,
  );
  VarMap.concat(builtins, new_builtins);
};

module Fresh = IdTagged.FreshGrammar;

[@warning "-8"]
// let-unbox guarantees that the tuple will have length 2
let binary = (f: (DHExp.t, DHExp.t) => option(DHExp.t), d: DHExp.t) => {
  let-unbox [d1, d2] = (Tuple(2), d);
  f(d1, d2);
};

[@warning "-8"]
// let-unbox guarantees that the tuple will have length 3int
let ternary = (f: (DHExp.t, DHExp.t, DHExp.t) => option(DHExp.t), d: DHExp.t) => {
  let-unbox [d1, d2, d3] = (Tuple(3), d);
  f(d1, d2, d3);
};

let float_op = (fn, d) => {
  let-unbox f = (Atom(Float), d);
  Some(Fresh.Exp.float(fn(f)));
};

let of_atom_builtin = (b: Atom.builtin): builtin => {
  switch (b) {
  | OneFun(k1, k2, f) =>
    Fn(
      Atom(k1 |> Atom.cls_of_kind) |> Typ.fresh,
      Atom(k2 |> Atom.cls_of_kind) |> Typ.fresh,
      (d: DHExp.t) => {
        let-unbox x = (Atom(k1), d);
        switch (f(x)) {
        | L(x) => Some(Atom(Atom.repack(k2, x)) |> Exp.fresh)
        | R(_) => None
        };
      },
    )
  | TwoFun(k1, k2, k3, f) =>
    Fn(
      Prod([
        Atom(k1 |> Atom.cls_of_kind) |> Typ.fresh,
        Atom(k2 |> Atom.cls_of_kind) |> Typ.fresh,
      ])
      |> Typ.fresh,
      Atom(k3 |> Atom.cls_of_kind) |> Typ.fresh,
      [@warning "-8"] (d: DHExp.t) => {
        let-unbox [x, y] = (Tuple(2), d);
        let-unbox x = (Atom(k1), x);
        let-unbox y = (Atom(k2), y);
        switch (f(x, y)) {
        | L(x) => Some(Atom(Atom.repack(k3, x)) |> Exp.fresh)
        | R(_) => None
        };
      },
    )
  };
};
