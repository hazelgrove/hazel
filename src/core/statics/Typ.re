open Sexplib.Std;

/*******************************************************************************
 * Pos                                                                         *
 *******************************************************************************/

module Pos = {
  [@deriving (sexp, yojson)]
  [@sexp.opaque]
  [@yojson.opaqe]
  type absolute;

  [@deriving (sexp, yojson)]
  [@sexp.opaque]
  [@yojson.opaqe]
  type relative;
};

/*******************************************************************************
 * Idx                                                                         *
 *******************************************************************************/

module Idx = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type s('pos) = int;

  let pp = fmt => Format.fprintf(fmt, "%d");

  let show = Int.to_string;

  let equal = Int.equal;

  let shift = (~above: int=(-1), idx: s('pos), amount: int): s('pos) =>
    idx >= above ? idx + amount : idx;

  module Abs = {
    [@deriving (sexp, yojson)]
    type t = s(Pos.absolute);
    let pp = fmt => Format.fprintf(fmt, "%d");
    let show = show;
    let of_int = i => i;
    let to_int = i => i;
    let to_rel = (~offset=0, i) => i + offset;
  };

  module Rel = {
    [@deriving (sexp, yojson)]
    type t = s(Pos.relative);
    let pp = fmt => Format.fprintf(fmt, "%d");
    let show = show;
    let of_int = i => i;
    let to_int = i => i;
    let to_abs = (~offset=0, i) => i + offset;
  };
};

/*******************************************************************************
 * Ref                                                                         *
 *******************************************************************************/

module Ref = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type s('pos) = {
    index: Idx.s('pos),
    stamp: int,
    predecessors: list(string),
    successors: list(string),
  };

  [@deriving (sexp, yojson)]
  type t = s(Pos.absolute);

  let pp = (fmt, cref) =>
    Format.fprintf(
      fmt,
      "{index: %a, stamp: %d, predecessors: %a, successors: %a}",
      Idx.pp,
      cref.index,
      cref.stamp,
      Format.pp_print_list(Format.pp_print_string),
      cref.predecessors,
      Format.pp_print_list(Format.pp_print_string),
      cref.successors,
    );

  let show = cref => Format.asprintf("%a", pp, cref);

  /* let abs: (Idx.Abs.t, int, ~predecessors: peers=?, ~successors: peers=?) => t; */
  let abs =
      (
        index: Idx.Abs.t,
        ~predecessors: list(string)=[],
        ~successors: list(string)=[],
        stamp: int,
      )
      : t => {
    let i = Idx.Abs.to_int(index);
    if (i >= stamp) {
      failwith(__LOC__ ++ ": reference is in the past");
    } else if (i < 0) {
      failwith(__LOC__ ++ ": reference is in the future");
    };
    if (List.length(successors) != i) {
      failwith(__LOC__ ++ ": index does not agree with successors");
    };
    if (List.length(predecessors) != stamp - i - 1) {
      failwith(__LOC__ ++ ": stamp does not agree with predecessors");
    };
    {index, stamp, predecessors, successors};
  };

  let equivalent =
      ({index, stamp, _}: t, {index: index', stamp: stamp', _}: t): bool => {
    let delta = stamp - stamp';
    Idx.equal(
      delta >= 0 ? index : Idx.shift(index, - delta),
      delta <= 0 ? index' : Idx.shift(index', delta),
    );
  };
};

/*******************************************************************************
 * Typ_syntax                                                                  *
 *******************************************************************************/

module Typ_syntax = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type type_provenance =
    | SynSwitch
    | TypeHole
    | Internal;

  [@deriving (sexp, yojson)]
  type t('pos) =
    | Unknown(type_provenance)
    | Int
    | Float
    | Bool
    | List(t('pos))
    | Arrow(t('pos), t('pos))
    | Prod(list(t('pos)))
    | TyVar(Ref.s('pos), TyVar.t);

  let rec to_rel = (~offset: int=0, ty: t(Pos.absolute)): t(Pos.relative) =>
    switch (ty) {
    | Unknown(prov) => Unknown(prov)
    | Int => Int
    | Float => Float
    | Bool => Bool
    | List(ty) => List(to_rel(~offset, ty))
    | Arrow(ty1, ty2) => Arrow(to_rel(~offset, ty1), to_rel(~offset, ty2))
    | Prod(tys) => Prod(List.map(to_rel(~offset), tys))
    | TyVar(cref, t) =>
      let cref = {...cref, index: Idx.Abs.to_rel(~offset, cref.index)};
      TyVar(cref, t);
    };

  let rec to_abs = (~offset: int=0, ty: t(Pos.relative)): t(Pos.absolute) =>
    switch (ty) {
    | Unknown(prov) => Unknown(prov)
    | Int => Int
    | Float => Float
    | Bool => Bool
    | List(ty) => List(to_abs(~offset, ty))
    | Arrow(ty1, ty2) => Arrow(to_abs(~offset, ty1), to_abs(~offset, ty2))
    | Prod(tys) => Prod(List.map(to_abs(~offset), tys))
    | TyVar(cref, t) =>
      let index = Idx.Rel.to_abs(~offset, cref.index);
      let stamp = cref.stamp + offset;
      let cref = {...cref, index, stamp};
      TyVar(cref, t);
    };

  let join_type_provenance =
      (p1: type_provenance, p2: type_provenance): type_provenance =>
    switch (p1, p2) {
    | (TypeHole, TypeHole | Internal | SynSwitch)
    | (Internal | SynSwitch, TypeHole) => TypeHole
    | (Internal, Internal | SynSwitch)
    | (SynSwitch, Internal) => Internal
    | (SynSwitch, SynSwitch) => SynSwitch
    };
};

/*******************************************************************************
 * Kind_syntax                                                                 *
 *******************************************************************************/

module Kind_syntax = {
  [@deriving (sexp, yojson)]
  type s('pos) =
    | /** A type of unknown kind */
      Unknown
    | /** An abstract type */
      Abstract
    | /** An equivalence class of types */
      Singleton(Typ_syntax.t('pos));

  let to_rel = (~offset: int=0, k: s(Pos.absolute)): s(Pos.relative) =>
    switch (k) {
    | Unknown => Unknown
    | Abstract => Abstract
    | Singleton(ty) => Singleton(Typ_syntax.to_rel(~offset, ty))
    };

  let to_abs = (~offset: int=0, k: s(Pos.relative)): s(Pos.absolute) =>
    switch (k) {
    | Unknown => Unknown
    | Abstract => Abstract
    | Singleton(ty) => Singleton(Typ_syntax.to_abs(~offset, ty))
    };
};

/*******************************************************************************
 * Ctx                                                                         *
 *******************************************************************************/

module rec Ctx: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type binding;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(binding);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type var_entry = {
    id: Id.t,
    name: Var.t,
    typ: Typ.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type tyvar_entry = {
    id: Id.t,
    name: TyVar.t,
    kind: Kind.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type entry =
    | VarEntry(var_entry)
    | TyVarEntry(tyvar_entry);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type coentry = {
    id: Id.t,
    mode: Typ.mode,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type co = VarMap.t_(list(coentry));

  let to_context: t => Context.t;

  let subtract: (t, co) => co;
  let union: list(co) => co;

  let binding_name: binding => string;
  let empty: unit => t;
  let to_list:
    t => (list((Ref.t, var_entry)), list((Ref.t, tyvar_entry)));
  let of_entries: list(entry) => t;
  let entries: t => list(entry);
  let length: t => int;
  let rescope: (t, Ref.t) => Ref.t;
  let tyvars: t => list((Ref.t, tyvar_entry));
  let tyvar: (t, Ref.t) => option(tyvar_entry);
  let tyvar_named: (t, TyVar.t) => option((Ref.t, tyvar_entry));
  let add_tyvar: (t, tyvar_entry) => t;
  let reduce_tyvars: (t, t, Typ.t) => Typ.t;
  let vars: t => list((Ref.t, var_entry));
  let var: (t, Ref.t) => option(var_entry);
  let var_named: (t, Var.t) => option((Ref.t, var_entry));
  let add_var: (t, var_entry) => t;
} = {
  [@deriving (sexp, yojson)]
  type binding =
    | VarBinding({
        id: Id.t,
        name: Var.t,
        typ: Typ_syntax.t(Pos.relative),
      })
    | TyVarBinding({
        id: Id.t,
        name: TyVar.t,
        kind: Kind_syntax.s(Pos.relative),
      });

  [@deriving (show({with_path: false}), sexp, yojson)]
  type var_entry = {
    id: Id.t,
    name: Var.t,
    typ: Typ.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type tyvar_entry = {
    id: Id.t,
    name: TyVar.t,
    kind: Kind.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type entry =
    | VarEntry(var_entry)
    | TyVarEntry(tyvar_entry);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type coentry = {
    id: Id.t,
    mode: Typ.mode,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type co = VarMap.t_(list(coentry));

  let pp_binding = fmt =>
    fun
    | VarBinding({id, name, typ}) =>
      Format.fprintf(
        fmt,
        "VarBinding({id: %a, name: \"%s\", typ: %a})",
        Id.pp,
        id,
        String.escaped(name),
        Typ.pp,
        Typ.of_syntax(Typ_syntax.to_abs(typ)),
      )
    | TyVarBinding({id, name, kind}) =>
      Format.fprintf(
        fmt,
        "TyVarBinding({id: %a, name: \"%s\", kind: %a})",
        Id.pp,
        id,
        String.escaped(name),
        Kind.pp,
        Kind_syntax.to_abs(kind),
      );

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(binding);

  let subtract = (ctx: t, free: co): co =>
    VarMap.filter(
      ((k, _)) =>
        switch (Ctx.var_named(ctx, k)) {
        | None => true
        | Some(_) => false
        },
      free,
    );

  //TODO(andrew): is this correct in the case of duplicates?
  let union: list(co) => co =
    List.fold_left((free1, free2) => free1 @ free2, []);

  let show_binding: binding => string = Format.asprintf("%a", pp_binding);

  let binding_name =
    fun
    | VarBinding({name, _})
    | TyVarBinding({name, _}) => name;

  let empty: unit => t = () => [];

  let length = List.length;

  let ref_at_index = (ctx: t, i: int): Ref.t => {
    let index = Idx.Abs.of_int(i);
    let stamp = length(ctx);
    let (successors, _, predecessors) =
      ctx |> List.map(binding_name) |> ListUtil.pivot(i);
    Ref.abs(index, stamp, ~predecessors, ~successors);
  };

  let to_list =
      (ctx: t): (list((Ref.t, var_entry)), list((Ref.t, tyvar_entry))) => {
    List.fold_right(
      (binding, (i, (vars, tyvars))) =>
        switch (binding) {
        | VarBinding({id, name, typ}) =>
          let typ = Typ.of_syntax(Typ_syntax.to_abs(~offset=i, typ));
          (
            i + 1,
            ([(ref_at_index(ctx, i), {id, name, typ}), ...vars], tyvars),
          );
        | TyVarBinding({id, name, kind}) =>
          let kind = Kind_syntax.to_abs(~offset=i, kind);
          (
            i + 1,
            (vars, [(ref_at_index(ctx, i), {id, name, kind}), ...tyvars]),
          );
        },
      ctx,
      (0, ([], [])),
    )
    |> snd;
  };

  // The general idea behind "peer tracking", i.e., predecessors and successors,is:
  //
  // 1. Predecessors should never change.
  //
  //   different predecessors  ===>  different pasts
  //
  // 2. One list of successors should always be a prefix of the other.
  //
  //   deviating successors  ==>  deviating futures (since cref was constructed)
  //
  // new stamp = old stamp  ==>  new successors = pivot(old successors, index)[0]
  // new stamp > old stamp  ==>  new successors = old successors + new entries
  // new stamp < old stamp  ==>  new successors = new successors - old entries

  let rescope = (ctx: t, cref: Ref.t): Ref.t => {
    let stamp = length(ctx);
    if (stamp == 0) {
      failwith(
        __LOC__ ++ ": cannot rescope type variables in an empty context",
      );
    };
    let amount = stamp - cref.stamp;
    let i = Idx.Abs.to_int(cref.index) + amount;
    if (i >= stamp) {
      failwith(__LOC__ ++ ": rescoping context is in the past");
    } else if (i < 0) {
      failwith(__LOC__ ++ ": rescoping context is in the future");
    };
    let (successors, _, predecessors) =
      ctx |> List.map(binding_name) |> ListUtil.pivot(i);
    if (List.length(predecessors) != List.length(cref.predecessors)
        || !List.for_all2(String.equal, predecessors, cref.predecessors)) {
      failwith(
        __LOC__
        ++ ": cannot rescope index in an incompatbile context: different pasts",
      );
    };
    let n = List.length(successors);
    let m = List.length(cref.successors);
    let (succs1, succs2) =
      switch (n > m, n < m) {
      | (true, _) => (ListUtil.drop(n - m, successors), cref.successors)
      | (_, true) => (successors, ListUtil.drop(m - n, cref.successors))
      | _ => (successors, cref.successors)
      };
    if (!List.for_all2(String.equal, succs1, succs2)) {
      failwith(
        __LOC__
        ++ ": cannot rescope index in incompatible context: diverging futures",
      );
    };
    let index = Idx.Abs.of_int(i);
    let cref = Ref.abs(index, stamp, ~predecessors, ~successors);
    if (Idx.Abs.to_int(cref.index) >= stamp) {
      failwith(__LOC__ ++ ": rescoped type variable is in the future");
    };
    if (Idx.Abs.to_int(cref.index) < 0) {
      failwith(__LOC__ ++ ": rescoped type variable is in the past");
    };
    cref;
  };

  let nth_var_binding =
      (ctx: t, n: int): option((Id.t, Var.t, Typ_syntax.t(Pos.relative))) => {
    open OptUtil.Syntax;
    let* binding = List.nth_opt(ctx, n);
    switch (binding) {
    | VarBinding({id, name, typ}) => Some((id, name, typ))
    | TyVarBinding(_) => None
    };
  };

  let nth_tyvar_binding =
      (ctx: t, n: int): option((Id.t, Var.t, Kind_syntax.s(Pos.relative))) => {
    open OptUtil.Syntax;
    let* binding = List.nth_opt(ctx, n);
    switch (binding) {
    | TyVarBinding({id, name, kind}) => Some((id, name, kind))
    | VarBinding(_) => None
    };
  };

  let first_var_binding =
      (ctx: t, f: (Id.t, Var.t, Typ_syntax.t(Pos.relative)) => bool)
      : option((int, Id.t, Var.t, Typ_syntax.t(Pos.relative))) => {
    let rec go = (i, ctx) =>
      switch (ctx) {
      | [VarBinding({id, name, typ}), ...ctx'] =>
        f(id, name, typ) ? Some((i, id, name, typ)) : go(i + 1, ctx')
      | [TyVarBinding(_), ...ctx'] => go(i + 1, ctx')
      | [] => None
      };
    go(0, ctx);
  };

  let first_tyvar_binding =
      (ctx: t, f: (Id.t, TyVar.t, Kind_syntax.s(Pos.relative)) => bool)
      : option((int, Id.t, TyVar.t, Kind_syntax.s(Pos.relative))) => {
    let rec go = (i, ctx) =>
      switch (ctx) {
      | [TyVarBinding({id, name, kind}), ...ctx'] =>
        f(id, name, kind) ? Some((i, id, name, kind)) : go(i + 1, ctx')
      | [VarBinding(_), ...ctx'] => go(i + 1, ctx')
      | [] => None
      };
    go(0, ctx);
  };

  /* Type Variables */

  let tyvars = (ctx: t): list((Ref.t, tyvar_entry)) => {
    ctx
    |> List.mapi((i, binding) => (i, binding))
    |> List.fold_left(
         (tyvars, (i, binding)) => {
           switch (binding) {
           | TyVarBinding({id, name, kind}) =>
             let cref = ref_at_index(ctx, i);
             let kind = Kind_syntax.to_abs(~offset=i, kind);
             [(cref, {id, name, kind}), ...tyvars];
           | VarBinding(_) => tyvars
           }
         },
         [],
       )
    |> List.rev;
  };

  let tyvar = (ctx: t, cref: Ref.t): option(tyvar_entry) => {
    open OptUtil.Syntax;
    let cref = rescope(ctx, cref);
    let i = Idx.Abs.to_int(cref.index);
    let+ (id, name, kind) = nth_tyvar_binding(ctx, i);
    let kind = Kind_syntax.to_abs(~offset=i + 1, kind);
    {id, name, kind};
  };

  let tyvar_named = (ctx: t, name: TyVar.t): option((Ref.t, tyvar_entry)) => {
    open OptUtil.Syntax;
    let+ (i, id, name, kind) =
      first_tyvar_binding(ctx, (_, name', _) => TyVar.equal(name, name'));
    let cref = ref_at_index(ctx, i);
    let kind = Kind_syntax.to_abs(~offset=i + 1, kind);
    (cref, {id, name, kind});
  };

  let add_tyvar = (ctx: t, entry: tyvar_entry): t => [
    TyVarBinding({
      id: entry.id,
      name: entry.name,
      kind: Kind_syntax.to_rel(entry.kind),
    }),
    ...ctx,
  ];

  /* Assumes indices in ty are scoped to new_ctx. */
  let reduce_tyvars = (new_ctx: t, old_ctx: t, ty: Typ.t): Typ.t => {
    let new_tyvars = tyvars(new_ctx);
    let old_tyvars = tyvars(old_ctx);
    let n = List.length(new_tyvars) - List.length(old_tyvars);
    let tyvars =
      ListUtil.take(new_tyvars, n)
      |> List.map(((cref: Ref.t, entry: tyvar_entry)) => {
           let cref = rescope(new_ctx, cref);
           let ty = Typ.rescope(new_ctx, Kind.to_typ(entry.kind));
           (cref, ty);
         });
    Typ.subst_tyvars(new_ctx, ty, tyvars);
  };

  /* Expression Variables */

  let vars = (ctx: t): list((Ref.t, var_entry)) => {
    ctx
    |> List.mapi((i, binding) => (i, binding))
    |> List.fold_left(
         (vars, (i, binding)) => {
           switch (binding) {
           | VarBinding({id, name, typ}) =>
             let cref = ref_at_index(ctx, i);
             let typ = Typ.of_syntax(Typ_syntax.to_abs(~offset=i, typ));
             [(cref, {id, name, typ}), ...vars];
           | TyVarBinding(_) => vars
           }
         },
         [],
       )
    |> List.rev;
  };

  let var = (ctx: t, cref: Ref.t): option(var_entry) => {
    open OptUtil.Syntax;
    let cref = rescope(ctx, cref);
    let i = Idx.Abs.to_int(cref.index);
    let+ (id, name, typ) = nth_var_binding(ctx, i);
    /* TODO: (eric) is this rescope necessary? */
    let typ =
      Typ.rescope(ctx, Typ.of_syntax(Typ_syntax.to_abs(~offset=i, typ)));
    {id, name, typ};
  };

  let var_named = (ctx: t, name: Var.t): option((Ref.t, var_entry)) => {
    open OptUtil.Syntax;
    let+ (i, id, name, typ) =
      first_var_binding(ctx, (_, name', _) => Var.eq(name, name'));
    let cref = ref_at_index(ctx, i);
    let typ =
      Typ.rescope(ctx, Typ.of_syntax(Typ_syntax.to_abs(~offset=i, typ)));
    (cref, {id, name, typ});
  };

  let add_var = (ctx: t, entry: var_entry): t => [
    VarBinding({
      id: entry.id,
      name: entry.name,
      typ: Typ_syntax.to_rel(Typ.to_syntax(entry.typ)),
    }),
    ...ctx,
  ];

  let entries = (ctx: t): list(entry) =>
    List.mapi(
      (i, binding) =>
        switch (binding) {
        | VarBinding({id, name, typ}) =>
          VarEntry({
            id,
            name,
            typ: Typ.of_syntax(Typ_syntax.to_abs(~offset=i, typ)),
          })
        | TyVarBinding({id, name, kind}) =>
          TyVarEntry({id, name, kind: Kind_syntax.to_abs(~offset=i, kind)})
        },
      ctx,
    );

  let of_entries = (entries: list(entry)): t =>
    List.fold_right(
      (entry, ctx) =>
        switch (entry) {
        | VarEntry(var_entry) => add_var(ctx, var_entry)
        | TyVarEntry(tyvar_entry) => add_tyvar(ctx, tyvar_entry)
        },
      entries,
      [],
    );

  let to_context = (ctx: t): Context.t =>
    ctx
    |> entries
    |> List.map(
         fun
         | VarEntry(entry) =>
           Context.VarEntry(entry.name, Typ.to_htyp(ctx, entry.typ))
         | TyVarEntry(entry) =>
           TyVarEntry(entry.name, Kind.to_hkind(ctx, entry.kind)),
       )
    |> Context.of_entries;
}

/*******************************************************************************
 * Kind                                                                        *
 *******************************************************************************/

and Kind: {
  [@deriving (sexp, yojson)]
  type t = Kind_syntax.s(Pos.absolute);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type source = {
    id: int,
    ty: t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type self =
    | Just(t)
    /* | Joined(list(source)) */
    | Free;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type mode =
    | Syn
    | Ana(t);

  let to_hkind: (Ctx.t, t) => KindSystem.Kind.t;

  let pp: (Format.formatter, t) => unit;
  let show: t => string;

  /* let rescope: (Ctx.t, t) => t; */
  let to_typ: t => Typ.t;

  let unknown: unit => t;
  let abstract: unit => t;
  let singleton: Typ.t => t;

  let consistent_subkind: (Ctx.t, t, t) => bool;
  let equivalent: (Ctx.t, t, t) => bool;
} = {
  [@deriving (sexp, yojson)]
  type t = Kind_syntax.s(Pos.absolute);

  let pp = (fmt, kind: t) =>
    switch (kind) {
    | Unknown => Format.fprintf(fmt, "Unknown")
    | Abstract => Format.fprintf(fmt, "Abstract")
    | Singleton(ty) =>
      let ty = Typ.of_syntax(ty);
      Format.fprintf(fmt, "Singleton(%a)", Typ.pp, ty);
    };

  let show = Format.asprintf("%a", pp);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type source = {
    id: int,
    ty: t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type self =
    | Just(t)
    /* | Joined(list(source)) */
    | Free;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type mode =
    | Syn
    | Ana(t);

  /* let rescope = (ctx: Ctx.t, kind: t): t => */
  /*   switch (kind) { */
  /*   | Unknown */
  /*   | Abstract => kind */
  /*   | Singleton(ty) => */
  /*     Singleton(Typ.to_syntax(Typ.rescope(ctx, Typ.of_syntax(ty)))) */
  /*   }; */

  let to_hkind = (ctx: Ctx.t, kind: t): KindSystem.Kind.t =>
    switch (kind) {
    | Unknown => Hole
    | Abstract => Type
    | Singleton(ty) =>
      S(HTyp.to_syntax(Typ.to_htyp(ctx, Typ.of_syntax(ty))))
    };

  /* For converting type variables to equivalent [Typ]s while resolving local
     type aliases. */
  let to_typ: t => Typ.t =
    fun
    | Unknown
    | Abstract => Typ.unknown(Internal)
    | Singleton(ty) => Typ.of_syntax(ty);

  let unknown = () => Kind_syntax.Unknown;
  let abstract = () => Kind_syntax.Abstract;
  let singleton = ty => Kind_syntax.Singleton(ty);

  let consistent_subkind = (ctx: Ctx.t, k: t, k': t): bool =>
    switch (k, k') {
    | (Unknown, _)
    | (_, Unknown) => true
    | (Singleton(_), Abstract) => true
    | (Singleton(ty), Singleton(ty')) =>
      Typ.consistent(ctx, Typ.of_syntax(ty), Typ.of_syntax(ty'))
    | (Abstract, Singleton(_)) => false
    | (Abstract, Abstract) => true
    };

  let equivalent = (ctx: Ctx.t, k: t, k': t): bool =>
    switch (k, k') {
    | (Unknown | Abstract, Unknown | Abstract) => true
    | (Unknown | Abstract, _) => false
    | (Singleton(ty1), Singleton(ty1')) =>
      Typ.equivalent(ctx, Typ.of_syntax(ty1), Typ.of_syntax(ty1'))
    | (Singleton(_), _) => false
    };
}

/*******************************************************************************
 * Typ                                                                         *
 *******************************************************************************/

and Typ: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = pri Typ_syntax.t(Pos.absolute);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type source = {
    id: int,
    ty: t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type self =
    | Just(t)
    | Joined(list(source))
    | Multi
    | Free;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type mode =
    | Syn
    | Ana(t);

  [@deriving sexp]
  type normalized = Typ_syntax.t(Pos.absolute);

  let to_htyp: (Ctx.t, t) => HTyp.t;

  let to_syntax: t => Typ_syntax.t(Pos.absolute);
  let of_syntax: Typ_syntax.t(Pos.absolute) => t;

  let unknown: Typ_syntax.type_provenance => t;
  let int: unit => t;
  let float: unit => t;
  let bool: unit => t;
  let list: t => t;
  let arrow: (t, t) => t;
  let product: list(t) => t;

  let is_unknown: t => bool;
  let is_unknown_synswitch: t => bool;

  let is_int: t => bool;
  let is_float: t => bool;
  let is_bool: t => bool;
  let is_list: t => bool;
  let is_arrow: t => bool;
  let is_product: t => bool;
  let is_tyvar: t => bool;

  let consistent: (Ctx.t, t, t) => bool;
  let equivalent: (Ctx.t, t, t) => bool;
  let complete: (Ctx.t, t) => bool;

  let tyvar: (Ctx.t, Idx.Abs.t, TyVar.t) => t;
  let tyvar_ref: t => option(Ref.t);
  let tyvar_name: t => option(TyVar.t);

  let rescope: (Ctx.t, t) => t;
  let subst_tyvars: (Ctx.t, t, list((Ref.t, t))) => t;

  let of_normalized: normalized => t;
  let normalize: (Ctx.t, t) => normalized;
  let normalized_consistent: (normalized, normalized) => bool;
  let normalized_equivalent: (normalized, normalized) => bool;
} = {
  [@deriving (sexp, yojson)]
  type t = Typ_syntax.t(Pos.absolute);

  let rec pp = (fmt, ty: t) =>
    switch (ty) {
    | Unknown(type_provenance) =>
      Format.fprintf(
        fmt,
        "Unknown(%a)",
        Typ_syntax.pp_type_provenance,
        type_provenance,
      )
    | Int => Format.fprintf(fmt, "Int")
    | Float => Format.fprintf(fmt, "Float")
    | Bool => Format.fprintf(fmt, "Bool")
    | List(ty) => Format.fprintf(fmt, "List(%a)", pp, ty)
    | Arrow(ty1, ty2) =>
      Format.fprintf(fmt, "Arrow(%a, %a)", pp, ty1, pp, ty2)
    | Prod(tys) =>
      Format.fprintf(fmt, "Prod(%a)", Format.pp_print_list(pp), tys)
    | TyVar(cref, t) =>
      Format.fprintf(fmt, "TyVar(%a, %a)", Ref.pp, cref, TyVar.pp, t)
    };

  let rec show = (ty: t) =>
    switch (ty) {
    | Unknown(type_provenance) =>
      "Unknown(" ++ Typ_syntax.show_type_provenance(type_provenance) ++ ")"
    | Int => "Int"
    | Float => "Float"
    | Bool => "Bool"
    | List(ty) => "List(" ++ show(ty) ++ ")"
    | Arrow(ty1, ty2) => "Arrow(" ++ show(ty1) ++ ", " ++ show(ty2) ++ ")"
    | Prod(tys) => "Prod(" ++ String.concat(", ", List.map(show, tys)) ++ ")"
    | TyVar(cref, t) => "TyVar(" ++ Ref.show(cref) ++ TyVar.show(t) ++ ")"
    };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type source = {
    id: int,
    ty: t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type self =
    | Just(t)
    | Joined(list(source))
    | Multi
    | Free;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type mode =
    | Syn
    | Ana(t);

  [@deriving sexp]
  type normalized = Typ_syntax.t(Pos.absolute);

  let rec to_htyp = (ctx: Ctx.t, ty: t): HTyp.t =>
    switch (ty) {
    | Unknown(_) => HTyp.hole()
    | Int => HTyp.int()
    | Float => HTyp.float()
    | Bool => HTyp.bool()
    | List(ty) => HTyp.list(to_htyp(ctx, ty))
    | Arrow(ty1, ty2) => HTyp.arrow(to_htyp(ctx, ty1), to_htyp(ctx, ty2))
    | Prod(tys) => HTyp.product(List.map(to_htyp(ctx), tys))
    | TyVar(cref, t) =>
      HTyp.tyvar(Ctx.to_context(ctx), Index.Abs.of_int(cref.index), t)
    };

  let to_syntax: t => Typ_syntax.t(Pos.absolute) = ty => ty;

  let of_syntax: Typ_syntax.t(Pos.absolute) => t = ty => ty;

  let unknown: Typ_syntax.type_provenance => t =
    type_provenance => Unknown(type_provenance);
  let int: unit => t = () => Int;
  let float: unit => t = () => Float;
  let bool: unit => t = () => Bool;
  let list = (ty: t): t => List(ty);
  let arrow = (ty1: t, ty2: t): t => Arrow(ty1, ty2);
  let product = (tys: list(t)): t => Prod(tys);

  let is_unknown = (ty: t): bool =>
    switch (ty) {
    | Unknown(_) => true
    | _ => false
    };

  let is_unknown_synswitch = (ty: t): bool =>
    switch (ty) {
    | Unknown(SynSwitch) => true
    | _ => false
    };

  let is_int = (ty: t): bool => ty == Int;
  let is_float = (ty: t): bool => ty == Float;
  let is_bool = (ty: t): bool => ty == Bool;

  let is_list = (ty: t): bool =>
    switch (ty) {
    | List(_) => true
    | _ => false
    };

  let is_arrow = (ty: t): bool =>
    switch (ty) {
    | Arrow(_) => true
    | _ => false
    };

  let is_product = (ty: t): bool =>
    switch (ty) {
    | Prod(_) => true
    | _ => false
    };

  let is_tyvar = (ty: t): bool =>
    switch (ty) {
    | TyVar(_) => true
    | _ => false
    };

  let rec equivalent = (ctx: Ctx.t, ty: t, ty': t): bool =>
    switch (ty, ty') {
    | (TyVar(cref, _), TyVar(cref', _)) =>
      Ref.equivalent(cref, cref')
      || (
        switch (Ctx.tyvar(ctx, cref), Ctx.tyvar(ctx, cref')) {
        | (Some({kind, _}), Some({kind: kind', _})) =>
          Kind.equivalent(ctx, kind, kind')
        | (None, _)
        | (_, None) => false
        }
      )
    | (TyVar(_), _) => false
    /* | (TyVarHole(_, u, _), TyVarHole(_, u', _)) => MetaVar.eq(u, u') */
    /* | (TyVarHole(_, _, _), _) => false */
    /* | (InvalidText(u, _), InvalidText(u', _)) => MetaVar.eq(u, u') */
    /* | (InvalidText(_), _) => false */
    | (Unknown(_) | Int | Float | Bool, _) => ty == ty'
    | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
      equivalent(ctx, ty1, ty1') && equivalent(ctx, ty2, ty2')
    | (Arrow(_, _), _) => false
    | (Prod(tys1), Prod(tys2)) =>
      List.for_all2(equivalent(ctx), tys1, tys2)
    | (Prod(_), _) => false
    | (List(ty), List(ty')) => equivalent(ctx, ty, ty')
    | (List(_), _) => false
    };

  let rec consistent = (ctx: Ctx.t, ty: t, ty': t): bool =>
    switch (ty, ty') {
    | (TyVar(cref, _), TyVar(cref', _)) =>
      switch (
        Ctx.tyvar(ctx, Ctx.rescope(ctx, cref)),
        Ctx.tyvar(ctx, Ctx.rescope(ctx, cref')),
      ) {
      | (Some({kind: k, _}), Some({kind: k', _})) =>
        consistent(
          ctx,
          Typ.to_syntax(Kind.to_typ(k)),
          Typ.to_syntax(Kind.to_typ(k')),
        )
      | (None, _)
      | (_, None) => false
      }
    | (TyVar(cref, _), ty1)
    | (ty1, TyVar(cref, _)) =>
      switch (Ctx.tyvar(ctx, Ctx.rescope(ctx, cref))) {
      | Some({kind: k, _}) =>
        consistent(ctx, Typ.to_syntax(Kind.to_typ(k)), ty1)
      | None => false
      }
    | (Unknown(_), _)
    | (_, Unknown(_)) => true
    /* | (TyVarHole(_) | InvalidText(_) | Hole, _) */
    /* | (_, TyVarHole(_) | InvalidText(_) | Hole) => true */
    | (Int | Float | Bool, _) => ty == ty'
    | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
      consistent(ctx, ty1, ty1') && consistent(ctx, ty2, ty2')
    | (Arrow(_), _) => false
    | (Prod(tys), Prod(tys')) => List.for_all2(consistent(ctx), tys, tys')
    | (Prod(_), _) => false
    | (List(ty1), List(ty1')) => consistent(ctx, ty1, ty1')
    | (List(_), _) => false
    };

  /* let inconsistent = (ctx: Ctx.t, ty1: t, ty2: t): bool => */
  /*   !consistent(ctx, ty1, ty2); */

  /* let rec consistent_all = (ctx: Context.t, types: list(t)): bool => */
  /*   switch (types) { */
  /*   | [] => true */
  /*   | [hd, ...tl] => */
  /*     !List.exists(inconsistent(ctx, hd), tl) || consistent_all(ctx, tl) */
  /*   }; */

  /* complete (i.e. does not have any holes) */
  let rec complete = (ctx: Ctx.t, ty: t): bool =>
    switch (ty) {
    | Unknown(_) => false
    | Int
    | Float
    | Bool => true
    | TyVar(cref, _) =>
      switch (Ctx.tyvar(ctx, cref)) {
      | None
      | Some({kind: Unknown, _}) => false
      | Some({kind: Abstract, _}) => true
      | Some({kind: Singleton(ty), _}) => complete(ctx, ty)
      }
    | Arrow(ty1, ty2) => complete(ctx, ty1) && complete(ctx, ty2)
    | Prod(tys) => tys |> List.for_all(complete(ctx))
    | List(ty) => complete(ctx, ty)
    };

  /* Type Variables */

  let tyvar = (ctx: Ctx.t, index: Idx.Abs.t, t: TyVar.t): t => {
    let stamp = Ctx.length(ctx);
    let (successors, _, predecessors) =
      ctx
      |> List.map(Ctx.binding_name)
      |> ListUtil.pivot(Idx.Abs.to_int(index));
    TyVar({index, stamp, successors, predecessors}, t);
  };

  let tyvar_ref = (ty: t): option(Ref.t) =>
    switch (ty) {
    | TyVar(cref, _) => Some(cref)
    | Unknown(_)
    | Int
    | Float
    | Bool
    | Arrow(_)
    | Prod(_)
    | List(_) => None
    };

  let tyvar_name = (ty: t): option(TyVar.t) =>
    switch (ty) {
    | TyVar(_, t) => Some(t)
    | Unknown(_)
    | Int
    | Float
    | Bool
    | Arrow(_)
    | Prod(_)
    | List(_) => None
    };
  let rec rescope = (ctx: Ctx.t, ty: t): t =>
    switch (ty) {
    | Unknown(_)
    | Int
    | Float
    | Bool => ty
    | List(ty1) => List(rescope(ctx, ty1))
    | Arrow(ty1, ty2) => Arrow(rescope(ctx, ty1), rescope(ctx, ty2))
    | Prod(tys) => Prod(List.map(rescope(ctx), tys))
    | TyVar(cref, t) => TyVar(Ctx.rescope(ctx, cref), t)
    };

  let rec subst_tyvar = (ctx: Ctx.t, ty: t, cref: Ref.t, ty': t): t => {
    switch (ty) {
    | Unknown(_)
    | Int
    | Float
    | Bool => ty
    | List(ty1) => List(subst_tyvar(ctx, ty1, cref, ty'))
    | Arrow(ty1, ty2) =>
      Arrow(
        subst_tyvar(ctx, ty1, cref, ty'),
        subst_tyvar(ctx, ty2, cref, ty'),
      )
    | Prod(tys) =>
      Prod(List.map(ty1 => subst_tyvar(ctx, ty1, cref, ty'), tys))
    | TyVar(cref', _) => Ref.equivalent(cref, cref') ? ty' : ty
    };
  };

  let subst_tyvars = (ctx: Ctx.t, ty: t, tyvars: list((Ref.t, t))): t =>
    List.fold_left(
      (ty, (cref, ty')) => subst_tyvar(ctx, ty, cref, ty'),
      ty,
      tyvars,
    );

  /* Normalization */

  let of_normalized: normalized => t = t => t;

  /* Replaces every singleton-kinded type variable with a normalized type. */
  let rec normalize = (ctx: Ctx.t, ty: t): normalized =>
    switch (ty) {
    | TyVar(cref, _) =>
      switch (Ctx.tyvar(ctx, cref)) {
      | Some({kind: Unknown, _}) => ty
      | Some({kind: Singleton(ty1), _}) => normalize(ctx, ty1)
      | Some(_) => ty
      | None =>
        failwith(
          __LOC__
          ++ ": unknown type variable index "
          ++ Idx.show(cref.index)
          ++ " stamped "
          ++ Int.to_string(cref.stamp),
        )
      }
    | Unknown(_)
    | Int
    | Float
    | Bool => ty
    | Arrow(ty1, ty2) => Arrow(normalize(ctx, ty1), normalize(ctx, ty2))
    | Prod(tys) => Prod(List.map(normalize(ctx), tys))
    | List(ty1) => List(normalize(ctx, ty1))
    };

  /* Properties of Normalized Typ */

  let rec normalized_consistent = (ty: normalized, ty': normalized): bool =>
    switch (ty, ty') {
    | (TyVar(cref, _), TyVar(cref', _)) =>
      // normalization eliminates all type variables of singleton kind, so these
      // must be of kind Type or Hole
      Ref.equivalent(cref, cref')
    | (TyVar(_) | Unknown(_), _)
    | (_, TyVar(_) | Unknown(_)) => true
    | (Int | Float | Bool, _) => ty == ty'
    | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
      normalized_consistent(ty1, ty1') && normalized_consistent(ty2, ty2')
    | (Arrow(_), _) => false
    | (Prod(tys), Prod(tys')) =>
      List.for_all2(normalized_consistent, tys, tys')
    | (Prod(_), _) => false
    | (List(ty1), List(ty1')) => normalized_consistent(ty1, ty1')
    | (List(_), _) => false
    };

  let rec normalized_equivalent = (ty: normalized, ty': normalized): bool =>
    switch (ty, ty') {
    | (TyVar(cref, _), TyVar(cref', _)) => Ref.equivalent(cref, cref')
    | (TyVar(_), _) => false
    | (Unknown(_) | Int | Float | Bool, _) => ty == ty'
    | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
      normalized_equivalent(ty1, ty1') && normalized_equivalent(ty2, ty2')
    | (Arrow(_, _), _) => false
    | (Prod(tys1), Prod(tys2)) =>
      List.for_all2(normalized_equivalent, tys1, tys2)
    | (Prod(_), _) => false
    | (List(ty), List(ty')) => normalized_equivalent(ty, ty')
    | (List(_), _) => false
    };
};

/*******************************************************************************/

include (
          Typ:
             (module type of Typ) with
              type t = Typ.t and
              type source = Typ.source and
              type self = Typ.self and
              type mode = Typ.mode
        );

/* Strip location information from a list of sources */
let source_tys = List.map((source: source) => source.ty);

/* Lattice join on types. This is a LUB join in the hazel2
   sense in that any type dominates Unknown */
let rec join = (ctx: Ctx.t, ty1: t, ty2: t): option(t) =>
  switch (to_syntax(ty1), to_syntax(ty2)) {
  | (Unknown(p1), Unknown(p2)) =>
    Some(unknown(Typ_syntax.join_type_provenance(p1, p2)))
  | (Unknown(_), ty)
  | (ty, Unknown(_)) => Some(of_syntax(ty))
  | (Int, Int) => Some(ty1)
  | (Int, _) => None
  | (Float, Float) => Some(ty1)
  | (Float, _) => None
  | (Bool, Bool) => Some(ty1)
  | (Bool, _) => None
  | (Arrow(ty1_1, ty1_2), Arrow(ty2_1, ty2_2)) =>
    switch (
      join(ctx, of_syntax(ty1_1), of_syntax(ty2_1)),
      join(ctx, of_syntax(ty1_2), of_syntax(ty2_2)),
    ) {
    | (Some(ty1), Some(ty2)) => Some(arrow(ty1, ty2))
    | _ => None
    }
  | (Arrow(_), _) => None
  | (Prod(tys1), Prod(tys2)) =>
    if (List.length(tys1) != List.length(tys2)) {
      None;
    } else {
      switch (
        List.map2(
          join(ctx),
          List.map(of_syntax, tys1),
          List.map(of_syntax, tys2),
        )
        |> Util.OptUtil.sequence
      ) {
      | None => None
      | Some(tys) => Some(product(tys))
      };
    }
  | (Prod(_), _) => None
  | (List(ty_1), List(ty_2)) =>
    switch (join(ctx, of_syntax(ty_1), of_syntax(ty_2))) {
    | Some(ty) => Some(list(ty))
    | None => None
    }
  | (List(_), _) => None
  | (TyVar(cref, _), _) =>
    switch (Ctx.tyvar(ctx, cref)) {
    | Some({kind: Unknown, _}) => join(ctx, unknown(TypeHole), ty2)
    | Some({kind: Abstract, _}) => failwith(__LOC__ ++ ": not implemented")
    | Some({kind: Singleton(ty), _}) => join(ctx, of_syntax(ty), ty2)
    | None => None
    }
  };

let join_all = (ctx: Ctx.t): (list(t) => option(t)) =>
  List.fold_left(
    (acc, ty) => Util.OptUtil.and_then(join(ctx, ty), acc),
    Some(unknown(Internal)),
  );

let join_or_fst = (ctx: Ctx.t, ty: t, ty': t): t =>
  switch (join(ctx, ty, ty')) {
  | None => ty
  | Some(ty) => ty
  };

/* MATCHED JUDGEMENTS: Note that matched judgements work
   a bit different than hazel2 here since hole fixing is
   implicit. Somebody should check that what I'm doing
   here actually makes sense -Andrew */

let matched_arrow: t => (t, t) =
  ty =>
    switch (to_syntax(ty)) {
    | Arrow(ty_in, ty_out) => (of_syntax(ty_in), of_syntax(ty_out))
    | Unknown(prov) => (unknown(prov), unknown(prov))
    | _ => (unknown(Internal), unknown(Internal))
    };

let matched_arrow_mode: mode => (mode, mode) =
  fun
  | Syn => (Syn, Syn)
  | Ana(ty) => {
      let (ty_in, ty_out) = matched_arrow(ty);
      (Ana(ty_in), Ana(ty_out));
    };

let matched_prod_mode = (mode: mode, length): list(mode) =>
  switch (mode) {
  | Ana(ty) when is_product(ty) =>
    switch (Typ.to_syntax(ty)) {
    | Prod(ana_tys) when List.length(ana_tys) == length =>
      List.map(ty => Ana(Typ.of_syntax(ty)), ana_tys)
    | _ => List.init(length, _ => Syn)
    }
  | _ => List.init(length, _ => Syn)
  };

let matched_list: t => t =
  ty =>
    switch (Typ.to_syntax(ty)) {
    | List(ty) => Typ.of_syntax(ty)
    | Unknown(prov) => Typ.unknown(prov)
    | _ => Typ.unknown(Internal)
    };

let matched_list_mode: mode => mode =
  fun
  | Syn => Syn
  | Ana(ty) => Ana(matched_list(ty));

let matched_list_lit_mode = (mode: mode, length): list(mode) =>
  switch (mode) {
  | Syn => List.init(length, _ => Syn)
  | Ana(ty) => List.init(length, _ => Ana(matched_list(ty)))
  };

let ap_mode: mode = Ana(arrow(unknown(Internal), unknown(Internal)));
