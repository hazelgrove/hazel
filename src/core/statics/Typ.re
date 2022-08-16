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

  let equiv =
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
  type entry = {
    id: Id.t,
    typ: Typ.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = VarMap.t_(entry);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type co_item = {
    id: Id.t,
    mode: Typ.mode,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type co_entry = list(co_item);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type co = VarMap.t_(co_entry);

  let empty: t;

  let subtract: (t, co) => co;

  let union: list(co) => co;

  let tyvar_kind: (t, Ref.t) => option(Kind.t);
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type entry = {
    id: Id.t,
    typ: Typ.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = VarMap.t_(entry);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type co_item = {
    id: Id.t,
    mode: Typ.mode,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type co_entry = list(co_item);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type co = VarMap.t_(co_entry);

  let empty = VarMap.empty;

  let subtract = (ctx: t, free: co): co =>
    VarMap.filter(
      ((k, _)) =>
        switch (VarMap.lookup(ctx, k)) {
        | None => true
        | Some(_) => false
        },
      free,
    );

  //TODO(andrew): is this correct in the case of duplicates?
  let union: list(co) => co =
    List.fold_left((free1, free2) => free1 @ free2, []);

  let tyvar_kind = (_ctx: t, _cref: Ref.t): option(Kind.t) =>
    /* XXX */
    None;
}

/*******************************************************************************
 * Kind                                                                        *
 *******************************************************************************/

and Kind: {
  [@deriving (sexp, yojson)]
  type t = Kind_syntax.s(Pos.absolute);

  let pp: (Format.formatter, t) => unit;

  let show: t => string;
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
  let is_int: t => bool;
  let is_float: t => bool;
  let is_bool: t => bool;
  let is_list: t => bool;
  let is_arrow: t => bool;
  let is_product: t => bool;
  let is_tyvar: t => bool;
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
    switch (Ctx.tyvar_kind(ctx, cref)) {
    | Some(Unknown) => join(ctx, unknown(TypeHole), ty2)
    | Some(Abstract) => failwith(__LOC__ ++ ": not implemented")
    | Some(Singleton(ty)) => join(ctx, of_syntax(ty), ty2)
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
