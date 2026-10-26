[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Invalid
  | EmptyHole
  | MultiHole
  | SigLet
  | SigType
  | SigModule;

include TermBase.Sig;

let fresh: term => t = IdTagged.fresh;

let rep_id: t => Id.t = IdTagged.rep_id;

let hole = (tms: list(TermBase.Any.t)): TermBase.Sig.term =>
  switch (tms) {
  | [] => EmptyHole
  | [_, ..._] => MultiHole(tms)
  };

let cls_of_term: Grammar.sig_term('a) => cls =
  fun
  | Invalid(_) => Invalid
  | EmptyHole => EmptyHole
  | MultiHole(_) => MultiHole
  | SigLet(_) => SigLet
  | SigType(_, _) => SigType
  | SigModule(_) => SigModule;

let show_cls: cls => string =
  fun
  | Invalid => "Invalid signature"
  | MultiHole => "Broken signature"
  | EmptyHole => "Signature hole"
  | SigLet => "Let declaration"
  | SigType => "Type declaration"
  | SigModule => "Module declaration";

let temp: term => t =
  term => {
    term,
    annotation: IdTagged.IdTag.temp(),
  };

/* ==================== Member view ====================
   A signature type is a dependent record: later items may mention earlier
   type members by their bare name. A member is a well-formed item (holes,
   invalid items and non-variable patterns are skipped). Canonical items are
   `SigLet(Asc(Var x, τ))`, `SigModule(Asc(Var m, τ))` and
   `SigType(Var T, τ)`. A `module m : S` member is a value member. */

[@deriving (show({with_path: false}), sexp, yojson)]
type member =
  | Val(Var.t, TermBase.Typ.t)
  | TypeManifest(Var.t, TermBase.Typ.t);

let rec var_of_pat = (p: TermBase.Pat.t): option(Var.t) =>
  switch (p.term) {
  | Var(x) => Some(x)
  | Parens(p) => var_of_pat(p)
  | _ => None
  };

let rec val_of_pat = (p: TermBase.Pat.t): option(member) =>
  switch (p.term) {
  | Parens(p) => val_of_pat(p)
  | Var(x) => Some(Val(x, TempGrammar.Typ.unknown(Internal)))
  | Asc(p', ty) =>
    switch (var_of_pat(p')) {
    | Some(x) => Some(Val(x, ty))
    | None => None
    }
  | _ => None
  };

let val_of_mpat = (mp: TermBase.MPat.t): option(member) =>
  switch (mp.term) {
  | Var(x) => Some(Val(x, TempGrammar.Typ.unknown(Internal)))
  | Asc(inner, ty) => MPat.name(inner) |> Option.map(x => Val(x, ty))
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => None
  };

let member_of_item = (item: t): option(member) =>
  switch (item.term) {
  | SigLet(p) => val_of_pat(p)
  | SigModule(mp) => val_of_mpat(mp)
  | SigType({term: Var(name), _}, ty) => Some(TypeManifest(name, ty))
  | SigType(_, _)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => None
  };

let members = (items: list(t)): list(member) =>
  List.filter_map(member_of_item, items);

let item_of_member = (m: member): t =>
  switch (m) {
  | Val(x, ty) => TempGrammar.(Sig.sig_let(Pat.asc(Pat.var(x), ty)))
  | TypeManifest(name, ty) => TempGrammar.(Sig.sig_type(TPat.var(name), ty))
  };

/* `module m : ty` — a sub-module member. */
let module_item = (name: Var.t, ty: TermBase.Typ.t): t =>
  TempGrammar.(Sig.sig_module(MPat.asc(MPat.var(name), ty)));

/* Apply [f] to the type carried by a well-formed item, keeping the item's
   form (`let`, `type` or `module`). An item written without a type, `let x`,
   is read as `let x : ?`: [f] is applied to `?`, and the item is ascribed
   the result unless that is still `?`. */
let map_typ = (f: TermBase.Typ.t => TermBase.Typ.t, item: t): t => {
  let refined = (): option(TermBase.Typ.t) =>
    switch (f(TempGrammar.Typ.unknown(Internal))) {
    | {term: Unknown(_), _} => None
    | ty => Some(ty)
    };
  let rec pat_map = (p: TermBase.Pat.t): TermBase.Pat.t =>
    switch (p.term) {
    | Asc(p', ty) => {
        ...p,
        term: (Asc(p', f(ty)): TermBase.Pat.term),
      }
    | Parens(p') => {
        ...p,
        term: (Parens(pat_map(p')): TermBase.Pat.term),
      }
    | Var(x) =>
      switch (refined()) {
      | Some(ty) => {
          ...p,
          term: (Asc(TempGrammar.Pat.var(x), ty): TermBase.Pat.term),
        }
      | None => p
      }
    | _ => p
    };
  let mpat_map = (mp: TermBase.MPat.t): TermBase.MPat.t =>
    switch (mp.term) {
    | Asc(inner, ty) => {
        ...mp,
        term: (Asc(inner, f(ty)): TermBase.MPat.term),
      }
    | Var(x) =>
      switch (refined()) {
      | Some(ty) => {
          ...mp,
          term: (Asc(TempGrammar.MPat.var(x), ty): TermBase.MPat.term),
        }
      | None => mp
      }
    | Invalid(_)
    | EmptyHole
    | MultiHole(_) => mp
    };
  switch (item.term) {
  | SigLet(p) => {
      ...item,
      term: SigLet(pat_map(p)),
    }
  | SigType(tp, ty) => {
      ...item,
      term: SigType(tp, f(ty)),
    }
  | SigModule(mp) => {
      ...item,
      term: SigModule(mpat_map(mp)),
    }
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => item
  };
};

/* The type an item is written with, the one [map_typ] maps. */
let typ_of_item = (item: t): option(TermBase.Typ.t) => {
  let rec of_pat = (p: TermBase.Pat.t) =>
    switch (p.term) {
    | Asc(_, ty) => Some(ty)
    | Parens(p) => of_pat(p)
    | _ => None
    };
  switch (item.term) {
  | SigLet(p) => of_pat(p)
  | SigType(_, ty) => Some(ty)
  | SigModule({term: Asc(_, ty), _}) => Some(ty)
  | SigModule(_)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => None
  };
};

let value_names = (ms: list(member)): list(Var.t) =>
  ms
  |> List.filter_map(
       fun
       | Val(x, _) => Some(x)
       | TypeManifest(_) => None,
     )
  |> Util.ListUtil.dedup_f(String.equal);

let type_names = (ms: list(member)): list(Var.t) =>
  ms
  |> List.filter_map(
       fun
       | TypeManifest(x, _) => Some(x)
       | Val(_) => None,
     )
  |> Util.ListUtil.dedup_f(String.equal);

let same_kind = (a: member, b: member) =>
  switch (a, b) {
  | (Val(x, _), Val(y, _))
  | (TypeManifest(x, _), TypeManifest(y, _)) => String.equal(x, y)
  | (Val(_), TypeManifest(_))
  | (TypeManifest(_), Val(_)) => false
  };

let dedup_last_by = (member: 'a => member, xs: list('a)): list('a) =>
  List.rev(
    Util.ListUtil.dedup_f(
      (a, b) => same_kind(member(a), member(b)),
      List.rev(xs),
    ),
  );

/* Keep only the last member of each name and kind (a later binding
   shadows an earlier one), preserving the order of the survivors. */
let dedup_last: list(member) => list(member) = dedup_last_by(Fun.id);

/* Lookups take the last member with the name: a later binding shadows an
   earlier one. */
let find_value = (ms: list(member), name: Var.t): option(TermBase.Typ.t) =>
  List.fold_left(
    (acc, m) =>
      switch (m) {
      | Val(x, ty) when String.equal(x, name) => Some(ty)
      | _ => acc
      },
    None,
    ms,
  );

let find_type_def = (ms: list(member), name: Var.t): option(TermBase.Typ.t) =>
  List.fold_left(
    (acc, m) =>
      switch (m) {
      | TypeManifest(x, ty) when String.equal(x, name) => Some(ty)
      | _ => acc
      },
    None,
    ms,
  );
