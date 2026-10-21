[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Invalid
  | EmptyHole
  | MultiHole
  | SigLet
  | SigType
  | SigTypeAbstract
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
  | SigTypeAbstract(_) => SigTypeAbstract
  | SigModule(_) => SigModule;

let show_cls: cls => string =
  fun
  | Invalid => "Invalid signature"
  | MultiHole => "Broken signature"
  | EmptyHole => "Signature hole"
  | SigLet => "Let declaration"
  | SigType => "Type declaration"
  | SigTypeAbstract => "Abstract type declaration"
  | SigModule => "Module declaration";

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
   `SigLet(Asc(Var x, τ))`, `SigModule(Asc(Var m, τ))` and
   `SigType(Var T, τ)`. A `module m : S` member is a value member; `type T`
   with no definition is an abstract type member. */

[@deriving (show({with_path: false}), sexp, yojson)]
type member =
  | Val(Var.t, TermBase.Typ.t)
  | TypeManifest(Var.t, TermBase.Typ.t)
  | TypeAbstract(Var.t);

let typ_temp = (term: TermBase.Typ.term): TermBase.Typ.t => {
  term,
  annotation: IdTagged.IdTag.temp(),
};

let unknown_typ = () => typ_temp(Unknown(Internal));

let rec var_of_pat = (p: TermBase.Pat.t): option(Var.t) =>
  switch (p.term) {
  | Var(x) => Some(x)
  | Parens(p) => var_of_pat(p)
  | _ => None
  };

let rec val_of_pat = (p: TermBase.Pat.t): option(member) =>
  switch (p.term) {
  | Parens(p) => val_of_pat(p)
  | Var(x) => Some(Val(x, unknown_typ()))
  | Asc(p', ty) =>
    switch (var_of_pat(p')) {
    | Some(x) => Some(Val(x, ty))
    | None => None
    }
  | _ => None
  };

let rec name_of_mpat = (mp: TermBase.MPat.t): option(Var.t) =>
  switch (mp.term) {
  | Var(x) => Some(x)
  | Asc(inner, _) => name_of_mpat(inner)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => None
  };

let val_of_mpat = (mp: TermBase.MPat.t): option(member) =>
  switch (mp.term) {
  | Var(x) => Some(Val(x, unknown_typ()))
  | Asc(inner, ty) => name_of_mpat(inner) |> Option.map(x => Val(x, ty))
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => None
  };

let member_of_item = (item: t): option(member) =>
  switch (item.term) {
  | SigLet(p) => val_of_pat(p)
  | SigModule(mp) => val_of_mpat(mp)
  | SigType({term: Var(name), _}, ty) => Some(TypeManifest(name, ty))
  | SigTypeAbstract({term: Var(name), _}) => Some(TypeAbstract(name))
  | SigType(_, _)
  | SigTypeAbstract(_)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => None
  };

let members = (items: list(t)): list(member) =>
  List.filter_map(member_of_item, items);

let pat_var = (x: Var.t): TermBase.Pat.t => {
  term: Var(x),
  annotation: IdTagged.IdTag.temp(),
};

let mpat_var = (x: Var.t): TermBase.MPat.t => {
  term: Var(x),
  annotation: IdTagged.IdTag.temp(),
};

let item_of_member = (m: member): t =>
  switch (m) {
  | Val(x, ty) =>
    let asc: TermBase.Pat.t = {
      term: Asc(pat_var(x), ty),
      annotation: IdTagged.IdTag.temp(),
    };
    temp(SigLet(asc));
  | TypeManifest(name, ty) =>
    let tpat: TermBase.TPat.t = {
      term: Var(name),
      annotation: IdTagged.IdTag.temp(),
    };
    temp(SigType(tpat, ty));
  | TypeAbstract(name) =>
    let tpat: TermBase.TPat.t = {
      term: Var(name),
      annotation: IdTagged.IdTag.temp(),
    };
    temp(SigTypeAbstract(tpat));
  };

/* `module m : ty` — a sub-module member. */
let module_item = (name: Var.t, ty: TermBase.Typ.t): t => {
  let asc: TermBase.MPat.t = {
    term: Asc(mpat_var(name), ty),
    annotation: IdTagged.IdTag.temp(),
  };
  temp(SigModule(asc));
};

let of_members = (ms: list(member)): list(t) =>
  List.map(item_of_member, ms);

/* Apply [f] to the type carried by a well-formed item, keeping the item's
   form (`let`, `type` or `module`). A member written without a type carries
   `?`; it gains an ascription only if [f] refines that. */
let map_typ = (f: TermBase.Typ.t => TermBase.Typ.t, item: t): t => {
  let refined = (): option(TermBase.Typ.t) =>
    switch (f(unknown_typ())) {
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
          term: (Asc(pat_var(x), ty): TermBase.Pat.term),
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
          term: (Asc(mpat_var(x), ty): TermBase.MPat.term),
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
  | SigTypeAbstract(_)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => item
  };
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
       | TypeManifest(_)
       | TypeAbstract(_) => None,
     )
  |> dedup_names;

/* Names of type members, manifest or abstract. */
let type_names = (ms: list(member)): list(Var.t) =>
  ms
  |> List.filter_map(
       fun
       | TypeManifest(x, _)
       | TypeAbstract(x) => Some(x)
       | Val(_) => None,
     )
  |> dedup_names;

/* Value members and type members live in separate namespaces; manifest and
   abstract type members share one. */
let same_kind = (a: member, b: member) =>
  switch (a, b) {
  | (Val(x, _), Val(y, _))
  | (
      TypeManifest(x, _) | TypeAbstract(x),
      TypeManifest(y, _) | TypeAbstract(y),
    ) =>
    x == y
  | (Val(_), TypeManifest(_) | TypeAbstract(_))
  | (TypeManifest(_) | TypeAbstract(_), Val(_)) => false
  };

/* Keep only the last member of each name and kind (a later binding
   shadows an earlier one), preserving the order of the survivors. */
let dedup_last = (ms: list(member)): list(member) => {
  let rec go = (ms: list(member)) =>
    switch (ms) {
    | [] => []
    | [m, ...rest] =>
      List.exists(same_kind(m), rest) ? go(rest) : [m, ...go(rest)]
    };
  go(ms);
};

/* The well-formed items, keeping only the last of each name and kind. */
let dedup_last_items = (items: list(t)): list(t) => {
  let rec go = (items: list(t)) =>
    switch (items) {
    | [] => []
    | [item, ...rest] =>
      switch (member_of_item(item)) {
      | None => go(rest)
      | Some(m) =>
        let shadowed =
          List.exists(
            later =>
              switch (member_of_item(later)) {
              | Some(m') => same_kind(m, m')
              | None => false
              },
            rest,
          );
        shadowed ? go(rest) : [item, ...go(rest)];
      }
    };
  go(items);
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

/* The last type member named [name], manifest or abstract. */
let find_type = (ms: list(member), name: Var.t): option(member) =>
  List.fold_left(
    (acc, m) =>
      switch (m) {
      | TypeManifest(x, _)
      | TypeAbstract(x) when x == name => Some(m)
      | _ => acc
      },
    None,
    ms,
  );

/* The definition of the last MANIFEST type member named [name]. */
let find_type_def = (ms: list(member), name: Var.t): option(TermBase.Typ.t) =>
  switch (find_type(ms, name)) {
  | Some(TypeManifest(_, ty)) => Some(ty)
  | Some(TypeAbstract(_))
  | Some(Val(_))
  | None => None
  };
