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
  custom_statics: option(Ctx.custom_statics),
};

[@deriving (show({with_path: false}), sexp)]
type hazel_fn = {
  name: string,
  arg: Typ.term,
  ret: Typ.term,
  str: string,
  imp: Exp.t,
};

/* A builtin module: exposed as a single variable bound to a labeled tuple
 * of members (the runtime representation of baby modules). Members are
 * (label, hazel_fn) pairs; the label is the name projected via dot access
 * (e.g. Jq.select), while hazel_fn.name is the internal let-bound name used
 * for member-to-member references, kept distinct from the labels so members
 * don't shadow global builtins within the module's let-chain. Members may
 * only reference members defined earlier in the list. */
[@deriving (show({with_path: false}), sexp)]
type hazel_module = {
  name: string,
  members: list((string, hazel_fn)),
};

[@deriving (show({with_path: false}), sexp)]
type builtin =
  | Const(const)
  | Fn(fn)
  | HazelFn(hazel_fn)
  | HazelModule(hazel_module);

[@deriving (show({with_path: false}), sexp)]
type forms = VarMap.t_(DHExp.t => option(DHExp.t));

module Fresh = IdTagged.FreshGrammar;

let fn_builtin = x => Fn(x);
let const_builtin = x => Const(x);
let hazel_fn_builtin = x => HazelFn(x);
let module_builtin = (x: hazel_module) => HazelModule(x);

/* The type of a builtin module: a labeled product of its members. */
let module_typ = ({members, _}: hazel_module): TermBase.typ_t =>
  Fresh.Typ.prod(
    List.map(
      ((label, member: hazel_fn)) =>
        Fresh.Typ.tup_label(
          Fresh.Typ.label(label),
          Fresh.Typ.arrow(Typ.fresh(member.arg), Typ.fresh(member.ret)),
        ),
      members,
    ),
  );

let ctx_entry_of_builtin: builtin => Ctx.entry =
  fun
  | Const({name, typ, _}) =>
    Ctx.VarEntry({
      name,
      typ: typ |> Typ.fresh,
      id: Id.invalid,
      custom_statics: None,
    })
  | Fn({name, arg, ret, custom_statics, _}) =>
    Ctx.VarEntry({
      name,
      typ: Fresh.Typ.arrow(Typ.fresh(arg), Typ.fresh(ret)),
      id: Id.invalid,
      custom_statics,
    })
  | HazelFn({name, arg, ret, _}) =>
    Ctx.VarEntry({
      name,
      typ: Fresh.Typ.arrow(Typ.fresh(arg), Typ.fresh(ret)),
      id: Id.invalid,
      custom_statics: None,
    })
  | HazelModule({name, _} as m) =>
    Ctx.VarEntry({
      name,
      typ: module_typ(m),
      id: Id.invalid,
      custom_statics: None,
    });

let form_of_builtin:
  builtin => option((string, TermBase.exp_t => option(TermBase.exp_t))) =
  fun
  | Const(_) => None
  | Fn({name, imp, _}) => Some((name, imp))
  | HazelFn(_) => None
  | HazelModule(_) => None;

let imp_of_builtin: builtin => (string, TermBase.exp_t) =
  fun
  | Const({name, imp, _}) => (name, imp)
  | HazelFn({name, imp, _}) => (name, imp)
  | Fn({name, _}) => (name, Fresh.Exp.builtin_fun(name))
  | HazelModule({name, members}) => {
      /* Mirror the elaborated form of a module literal: a let-chain of the
       * members ending in a labeled tuple of their values. */
      let tuple_exp =
        Fresh.Exp.tuple(
          List.map(
            ((label, member: hazel_fn)) =>
              Fresh.Exp.tup_label(
                Fresh.Exp.label(label),
                Fresh.Exp.var(member.name),
              ),
            members,
          ),
        );
      let chain =
        List.fold_right(
          ((_, member: hazel_fn), acc) =>
            Fresh.Exp.let_(Fresh.Pat.var(member.name), member.imp, acc),
          members,
          tuple_exp,
        );
      (name, chain);
    };

let name_of_builtin: builtin => string =
  fun
  | Const({name, _})
  | Fn({name, _})
  | HazelFn({name, _})
  | HazelModule({name, _}) => name;

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
      custom_statics: None,
    })
  | TwoFun(k1, k2, k3, f) =>
    Fn({
      name,
      arg:
        Prod([
          Atom(k1 |> Atom.cls_of_kind) |> Typ.fresh,
          Atom(k2 |> Atom.cls_of_kind) |> Typ.fresh,
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
      custom_statics: None,
    })
  };
};
