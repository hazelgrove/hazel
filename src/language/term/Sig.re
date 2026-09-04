[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Invalid
  | EmptyHole
  | MultiHole
  | SigLet
  | SigType;

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
  | SigType(_, _) => SigType;

let show_cls: cls => string =
  fun
  | Invalid => "Invalid signature"
  | MultiHole => "Broken signature"
  | EmptyHole => "Signature hole"
  | SigLet => "Let declaration"
  | SigType => "Type declaration";

let temp: term => t =
  term => {
    term,
    annotation: IdTagged.IdTag.temp(),
  };

/* ==================== Member view ====================
   A signature type is a dependent record: later items may mention earlier
   type members by their bare name. This view exposes the well-formed items
   (holes, invalid items and non-variable patterns are skipped) so that the
   type checker can treat a `Sig` uniformly. Canonical items are
   `SigLet(Asc(Var x, τ))` and `SigType(Var T, τ)`. */

[@deriving (show({with_path: false}), sexp, yojson)]
type member =
  | Val(Var.t, TermBase.Typ.t)
  | TypeManifest(Var.t, TermBase.Typ.t);

let typ_temp = (term: TermBase.Typ.term): TermBase.Typ.t => {
  term,
  annotation: IdTagged.IdTag.temp(),
};

let rec var_of_pat = (p: TermBase.Pat.t): option(Var.t) =>
  switch (p.term) {
  | Var(x) => Some(x)
  | Parens(p) => var_of_pat(p)
  | _ => None
  };

let rec val_of_pat = (p: TermBase.Pat.t): option(member) =>
  switch (p.term) {
  | Parens(p) => val_of_pat(p)
  | Var(x) => Some(Val(x, typ_temp(Unknown(Internal))))
  | Asc(p', ty) =>
    switch (var_of_pat(p')) {
    | Some(x) => Some(Val(x, ty))
    | None => None
    }
  | _ => None
  };

let member_of_item = (item: t): option(member) =>
  switch (item.term) {
  | SigLet(p) => val_of_pat(p)
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
  | Val(x, ty) =>
    let var: TermBase.Pat.t = {
      term: Var(x),
      annotation: IdTagged.IdTag.temp(),
    };
    let asc: TermBase.Pat.t = {
      term: Asc(var, ty),
      annotation: IdTagged.IdTag.temp(),
    };
    temp(SigLet(asc));
  | TypeManifest(name, ty) =>
    let tpat: TermBase.TPat.t = {
      term: Var(name),
      annotation: IdTagged.IdTag.temp(),
    };
    temp(SigType(tpat, ty));
  };

let of_members = (ms: list(member)): list(t) =>
  List.map(item_of_member, ms);

/* Apply [f] to the type carried by a well-formed item; other items unchanged. */
let map_typ = (f: TermBase.Typ.t => TermBase.Typ.t, item: t): t =>
  switch (member_of_item(item)) {
  | Some(Val(x, ty)) => {
      ...item,
      term: item_of_member(Val(x, f(ty))).term,
    }
  | Some(TypeManifest(name, ty)) => {
      ...item,
      term: item_of_member(TypeManifest(name, f(ty))).term,
    }
  | None => item
  };

let dedup_names = (names: list(Var.t)): list(Var.t) =>
  List.fold_left(
    (acc, n) => List.mem(n, acc) ? acc : acc @ [n],
    [],
    names,
  );

let value_names = (ms: list(member)): list(Var.t) =>
  ms
  |> List.filter_map(
       fun
       | Val(x, _) => Some(x)
       | TypeManifest(_) => None,
     )
  |> dedup_names;

let type_names = (ms: list(member)): list(Var.t) =>
  ms
  |> List.filter_map(
       fun
       | TypeManifest(x, _) => Some(x)
       | Val(_) => None,
     )
  |> dedup_names;

/* Keep only the last member of each name and kind (a later binding
   shadows an earlier one), preserving the order of the survivors. */
let dedup_last = (ms: list(member)): list(member) => {
  let same_kind = (a: member, b: member) =>
    switch (a, b) {
    | (Val(x, _), Val(y, _))
    | (TypeManifest(x, _), TypeManifest(y, _)) => x == y
    | (Val(_), TypeManifest(_))
    | (TypeManifest(_), Val(_)) => false
    };
  let rec go = (ms: list(member)) =>
    switch (ms) {
    | [] => []
    | [m, ...rest] =>
      List.exists(same_kind(m), rest) ? go(rest) : [m, ...go(rest)]
    };
  go(ms);
};

/* Lookups take the LAST member with the name: a later binding shadows an
   earlier one, which is also how module bodies export their bindings. */
let find_value = (ms: list(member), name: Var.t): option(TermBase.Typ.t) =>
  List.fold_left(
    (acc, m) =>
      switch (m) {
      | Val(x, ty) when x == name => Some(ty)
      | _ => acc
      },
    None,
    ms,
  );

let find_type_def = (ms: list(member), name: Var.t): option(TermBase.Typ.t) =>
  List.fold_left(
    (acc, m) =>
      switch (m) {
      | TypeManifest(x, ty) when x == name => Some(ty)
      | _ => acc
      },
    None,
    ms,
  );
