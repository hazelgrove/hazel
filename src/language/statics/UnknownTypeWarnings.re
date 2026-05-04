open Util;

module Map = StaticsBase.Map;

let ty_of_info = (info: Info.t): option(Typ.t) =>
  switch (info) {
  | InfoExp({ty, _}) => Some(ty)
  | InfoPat({ty, _}) => Some(ty)
  | InfoTyp({user_term, _}) => Some(user_term)
  | _ => None
  };

let info_contains_unknown = (info: Info.t): bool =>
  switch (ty_of_info(info)) {
  | Some(ty) => Typ.contains_unknown(ty)
  | None => false
  };

let id_contains_unknown = (m: Map.t, id: Id.t): bool =>
  switch (Map.lookup(id, m)) {
  | Some(info) => info_contains_unknown(info)
  | None => false
  };

/* Collect direct depth-1 child term ids (across Exp/Pat/Typ/TPat) of a term.
   Strategy: run map_term with f's that, at depth 0, descend via `continue`,
   but at depth 1 record the id and stop. f_rul/f_any default to passthrough,
   flattening through Rul/Any wrappers so we see the Pat/Exp/Typ underneath. */
let depth1_children_exp = (e: Exp.t): list(Id.t) => {
  let depth = ref(0);
  let ids = ref([]);
  let f:
    'a.
    (IdTagged.t('a) => IdTagged.t('a), IdTagged.t('a)) => IdTagged.t('a)
   =
    (continue, x) =>
      if (depth^ == 0) {
        incr(depth);
        let r = continue(x);
        decr(depth);
        r;
      } else {
        ids := [IdTagged.rep_id(x), ...ids^];
        x;
      };
  let _ = Exp.map_term(~f_exp=f, ~f_pat=f, ~f_typ=f, ~f_tpat=f, e);
  ids^;
};

let depth1_children_pat = (p: Pat.t): list(Id.t) => {
  let depth = ref(0);
  let ids = ref([]);
  let f:
    'a.
    (IdTagged.t('a) => IdTagged.t('a), IdTagged.t('a)) => IdTagged.t('a)
   =
    (continue, x) =>
      if (depth^ == 0) {
        incr(depth);
        let r = continue(x);
        decr(depth);
        r;
      } else {
        ids := [IdTagged.rep_id(x), ...ids^];
        x;
      };
  let _ = TermBase.Pat.map_term(~f_exp=f, ~f_pat=f, ~f_typ=f, ~f_tpat=f, p);
  ids^;
};

let depth1_children_typ = (t: Typ.t): list(Id.t) => {
  let depth = ref(0);
  let ids = ref([]);
  let f:
    'a.
    (IdTagged.t('a) => IdTagged.t('a), IdTagged.t('a)) => IdTagged.t('a)
   =
    (continue, x) =>
      if (depth^ == 0) {
        incr(depth);
        let r = continue(x);
        decr(depth);
        r;
      } else {
        ids := [IdTagged.rep_id(x), ...ids^];
        x;
      };
  let _ = TermBase.Typ.map_term(~f_exp=f, ~f_pat=f, ~f_typ=f, ~f_tpat=f, t);
  ids^;
};

/* A node N is a "source" of unknown iff its own type contains an Unknown
   AND none of its direct children carry unknown in their own types — i.e.
   the unknown originates here, not propagated from a sub-term. */
let is_source = (m: Map.t, id: Id.t, children: list(Id.t)): bool =>
  id_contains_unknown(m, id)
  && !List.exists(id_contains_unknown(m), children);

/* For Typ infos, suppress the warning on user-written `Unknown(_)` terms
   themselves — the `?` is already visible on screen, and a warning would be
   redundant noise. We still warn on type subterms whose type contains an
   unknown but whose head is not an unknown literal (e.g. a Var that resolves
   through an alias to a partially-unknown type, which won't render `?`). */
let suppress_typ = (ty: Typ.t): bool =>
  switch (ty.term) {
  | Unknown(_) => true
  | _ => false
  };

let collect_source_ids = (m: Map.t, root: Exp.t): Id.Set.t => {
  let sources = ref(Id.Set.empty);
  let consider = (id: Id.t, children: list(Id.t)) =>
    if (is_source(m, id, children)) {
      sources := Id.Set.add(id, sources^);
    };
  let f_exp = (continue, x: Exp.t): Exp.t => {
    consider(Exp.rep_id(x), depth1_children_exp(x));
    continue(x);
  };
  let f_pat = (continue, x: Pat.t): Pat.t => {
    consider(Pat.rep_id(x), depth1_children_pat(x));
    continue(x);
  };
  let f_typ = (continue, x: Typ.t): Typ.t => {
    if (!suppress_typ(x)) {
      consider(Typ.rep_id(x), depth1_children_typ(x));
    };
    continue(x);
  };
  let _ = Exp.map_term(~f_exp, ~f_pat, ~f_typ, root);
  sources^;
};

/* Build a fresh Info.t with one extra warning appended. */
let add_warning = (info: Info.t): Info.t =>
  switch (info, ty_of_info(info)) {
  | (InfoExp(i), Some(ty)) =>
    InfoExp({
      ...i,
      warnings: i.warnings @ [Exp(ContainsUnknown(ty))],
    })
  | (InfoPat(i), Some(ty)) =>
    InfoPat({
      ...i,
      warnings: i.warnings @ [Pat(ContainsUnknown(ty))],
    })
  | (InfoTyp(i), Some(ty)) =>
    InfoTyp({
      ...i,
      warnings: i.warnings @ [Typ(ContainsUnknown(ty))],
    })
  | _ => info
  };

/* Public entry: append `ContainsUnknown` warnings to every "source" info in
   the map. Multiple ids may share an Info value; we update by representative
   id (Info.id_of), matching how `warning_ids`/`is_warning` filter consumers. */
let annotate = (m: Map.t, root: Exp.t): Map.t => {
  let sources = collect_source_ids(m, root);
  Id.Map.mapi(
    (id, info) =>
      Id.equal(id, Info.id_of(info)) && Id.Set.mem(id, sources)
        ? add_warning(info) : info,
    m,
  );
};
