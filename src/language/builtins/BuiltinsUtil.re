open Util;

[@deriving (show({with_path: false}), sexp)]
type const = {
  name: Var.t,
  typ: Typ.term,
  imp: DHExp.t,
};

[@deriving (show({with_path: false}), sexp)]
type fn = {
  name: Var.t,
  arg: Typ.term,
  ret: Typ.term,
  imp: DHExp.t => option(DHExp.t),
};

[@deriving (show({with_path: false}), sexp)]
type hazel_fn = {
  name: string,
  arg: Typ.term,
  ret: Typ.term,
  str: string,
  imp: Exp.t,
};

[@deriving (show({with_path: false}), sexp)]
type builtin =
  | Const(const)
  | Fn(fn)
  | HazelFn(hazel_fn);

[@deriving (show({with_path: false}), sexp)]
type forms = VarMap.t_(DHExp.t => option(DHExp.t));

module Fresh = IdTagged.FreshGrammar;

let fn_builtin = x => Fn(x);
let const_builtin = x => Const(x);
let hazel_fn_builtin = x => HazelFn(x);

let ctx_entry_of_builtin: builtin => Ctx.entry =
  fun
  | Const({name, typ, _}) =>
    Ctx.VarEntry({
      name,
      typ: typ |> Typ.fresh_empty,
      id: Id.invalid,
    })
  | Fn({name, arg, ret, _})
  | HazelFn({name, arg, ret, _}) =>
    Ctx.VarEntry({
      name,
      typ: Fresh.Typ.arrow(Typ.fresh_empty(arg), Typ.fresh_empty(ret)),
      id: Id.invalid,
    });

let form_of_builtin:
  builtin => option((string, TermBase.exp_t => option(TermBase.exp_t))) =
  fun
  | Const(_) => None
  | Fn({name, imp, _}) => Some((name, imp))
  | HazelFn({name, imp, _}) =>
    Some((name, (d => Some(Fresh.Exp.ap(Forward, imp, d)))));

let imp_of_builtin: builtin => (string, TermBase.exp_t) =
  fun
  | Const({name, imp, _}) => (name, imp)
  | Fn({name, _})
  | HazelFn({name, _}) => (name, Fresh.Exp.builtin_fun(name));

let name_of_builtin: builtin => string =
  fun
  | Const({name, _})
  | Fn({name, _})
  | HazelFn({name, _}) => name;

exception BuiltinAlreadyDefined(Var.t);

// Like VarMap.extend but it fails if the name is already bound
let extend = (map: VarMap.t_(builtin), builtin: builtin): VarMap.t_(builtin) => {
  let name = name_of_builtin(builtin);
  if (VarMap.contains(map, name)) {
    raise(BuiltinAlreadyDefined(name));
  } else {
    VarMap.extend(map, (name, builtin));
  };
};

let to_map: list(builtin) => VarMap.t_(builtin) =
  List.fold_left(extend, VarMap.empty);

let (let-unbox) = ((request, v), f) =>
  switch (Unboxing.unbox(request, v)) {
  | IndetMatch
  | DoesNotMatch => None
  | Matches(n) => f(n)
  };

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

let of_atom_builtin = ((name: string, b: Atom.builtin)): builtin => {
  switch (b) {
  | OneFun(k1, k2, f) =>
    Fn({
      name,
      arg: Atom(k1 |> Atom.cls_of_kind),
      ret: Atom(k2 |> Atom.cls_of_kind),
      imp: (d: DHExp.t) => {
        let-unbox x = (Atom(k1), d);
        switch (f(x)) {
        | L(x) => Some(Atom(Atom.repack(k2, x)) |> Exp.fresh)
        | R(_) => None
        };
      },
    })
  | TwoFun(k1, k2, k3, f) =>
    Fn({
      name,
      arg:
        Prod([
          Atom(k1 |> Atom.cls_of_kind) |> Typ.fresh_empty,
          Atom(k2 |> Atom.cls_of_kind) |> Typ.fresh_empty,
        ]),
      ret: Atom(k3 |> Atom.cls_of_kind),
      imp:
        [@warning "-8"]
        (
          (d: DHExp.t) => {
            let-unbox [x, y] = (Tuple(2), d);
            let-unbox x = (Atom(k1), x);
            let-unbox y = (Atom(k2), y);
            switch (f(x, y)) {
            | L(x) => Some(Atom(Atom.repack(k3, x)) |> Exp.fresh)
            | R(_) => None
            };
          }
        ),
    })
  };
};
