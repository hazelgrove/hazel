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

[@deriving (show({with_path: false}), sexp)]
type access =
  | Public
  | Private;

/* A builtin module: exposed as a single variable bound to a labeled tuple
 * of its public members (the runtime representation of baby modules).
 * Member names are both the let-bound names within the module's let-chain
 * and the labels projected via dot access (e.g. Jq.select), so they shadow
 * same-named globals within the chain. Private members participate in the
 * let-chain (typically to alias globals shadowed by other members) but are
 * not exported. Members may only reference members defined earlier in the
 * list. */
[@deriving (show({with_path: false}), sexp)]
type builtin =
  | Const(const)
  | Fn(fn)
  | HazelFn(hazel_fn)
  | HazelModule(hazel_module)
and hazel_module = {
  name: string,
  members: list((builtin, access)),
};

[@deriving (show({with_path: false}), sexp)]
type forms = VarMap.t_(DHExp.t => option(DHExp.t));

module Fresh = IdTagged.FreshGrammar;

let fn_builtin = x => Fn(x);
let const_builtin = x => Const(x);
let hazel_fn_builtin = x => HazelFn(x);
let module_builtin = (x: hazel_module) => HazelModule(x);

/* Module-member constructor: defaults access to Public. */
let public = (b: builtin): (builtin, access) => (b, Public);

let name_of_builtin: builtin => string =
  fun
  | Const({name, _})
  | Fn({name, _})
  | HazelFn({name, _})
  | HazelModule({name, _}) => name;

/* The type of a builtin: an arrow for fn-shaped builtins, the const's typ
 * for Const, and the labeled product of public members for HazelModule. */
let rec typ_of_builtin: builtin => TermBase.typ_t =
  fun
  | Const({typ, _}) => typ |> Typ.fresh
  | Fn({arg, ret, _})
  | HazelFn({arg, ret, _}) =>
    Fresh.Typ.arrow(Typ.fresh(arg), Typ.fresh(ret))
  | HazelModule(m) => module_typ(m)
and module_typ = ({members, _}: hazel_module): TermBase.typ_t =>
  Fresh.Typ.prod(
    members
    |> List.filter_map(((b, access)) =>
         switch (access) {
         | Public =>
           Some(
             Fresh.Typ.tup_label(
               Fresh.Typ.label(name_of_builtin(b)),
               typ_of_builtin(b),
             ),
           )
         | Private => None
         }
       ),
  );

let ctx_entry_of_builtin = (b: builtin): Ctx.entry =>
  Ctx.VarEntry({
    name: name_of_builtin(b),
    typ: typ_of_builtin(b),
    id: Id.invalid,
    custom_statics:
      switch (b) {
      | Fn({custom_statics, _}) => custom_statics
      | _ => None
      },
  });

let form_of_builtin:
  builtin => option((string, TermBase.exp_t => option(TermBase.exp_t))) =
  fun
  | Const(_) => None
  | Fn({name, imp, _}) => Some((name, imp))
  | HazelFn(_) => None
  | HazelModule(_) => None;

/* The imp of a builtin: for HazelModule, mirror the elaborated form of a
 * module literal — a let-chain of all members (in order) ending in a
 * labeled tuple of the public members' values. Recursive so nested
 * modules elaborate the same way. */
let rec imp_of_builtin: builtin => (string, TermBase.exp_t) =
  fun
  | Const({name, imp, _}) => (name, imp)
  | HazelFn({name, imp, _}) => (name, imp)
  | Fn({name, _}) => (name, Fresh.Exp.builtin_fun(name))
  | HazelModule({name, members}) => {
      let tuple_exp =
        Fresh.Exp.tuple(
          members
          |> List.filter_map(((b, access)) =>
               switch (access) {
               | Public =>
                 let n = name_of_builtin(b);
                 Some(
                   Fresh.Exp.tup_label(
                     Fresh.Exp.label(n),
                     Fresh.Exp.var(n),
                   ),
                 );
               | Private => None
               }
             ),
        );
      let chain =
        List.fold_right(
          ((b, _access), acc) => {
            let (n, imp) = imp_of_builtin(b);
            Fresh.Exp.let_(Fresh.Pat.var(n), imp, acc);
          },
          members,
          tuple_exp,
        );
      (name, chain);
    };

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
