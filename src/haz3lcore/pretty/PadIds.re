open Util;
open Language;

/* HACK[Matt]: Sometimes terms that should have multiple ids won't because
   evaluation only ever gives them one.

   Some upstream producers (e.g., evaluator collapse, certain absorption
   paths) can emit ids lists with duplicates — e.g., [case_id, case_id, ...]
   for a Match where the adoption machinery did not preserve distinct rule
   ids. If we pass duplicates through unchanged, the pretty-printer will
   emit multiple Tile pieces sharing the same id (e.g., the case `[case;end]`
   form and all `[|;=>]` rules all tagged with case_id), and
   Segment.reassemble will group them into a single Aba match and fail
   with an out-of-order combined_shards assertion.

   To prevent that, pad_ids also ensures the returned list has:
   1. no duplicates within itself;
   2. no id equal to any id in [~forbidden]. */
let pad_ids =
    (~forbidden: list(Id.t)=[], n: int, ids: list(Id.t)): list(Id.t) => {
  let len = List.length(ids);
  let forbidden_set = ref(Id.Set.of_list(forbidden));
  let replace = id =>
    if (Id.Set.mem(id, forbidden_set^)) {
      let fresh = Id.mk();
      forbidden_set := Id.Set.add(fresh, forbidden_set^);
      fresh;
    } else {
      forbidden_set := Id.Set.add(id, forbidden_set^);
      id;
    };
  let truncated =
    if (len < n) {
      ids @ List.init(n - len, _ => Id.mk());
    } else {
      ListUtil.split_n(n, ids) |> fst;
    };
  List.map(replace, truncated);
};

let necessary_ids: Typ.t => int =
  ty =>
    switch (ty.term) {
    /* "()", "Void" and "{}" render from rep_id */
    | Prod([]) => 1
    | Sum([]) => 1
    | Sig([]) => 1
    /* one id per separator */
    | Prod(tys) => List.length(tys) - 1
    /* one id per variant; the single-variant form renders from rep_id */
    | Sum(tys) => max(1, List.length(tys))
    /* rep_id for the braces, then one id per `;` between items */
    | Sig(items) => max(1, List.length(items))
    /* one grout id between entries */
    | Unknown(Hole(MultiHole(es))) => max(0, List.length(es) - 1)
    /* every other form renders from rep_id alone */
    | _ => 1
    };

/* Ids a rendered variant consumes. */
let necessary_variant_ann_ids: ConstructorMap.variant(Typ.t) => int =
  fun
  | Variant(_, _, Some(_)) => 2 /* parens ID + constructor name ID */
  | Variant(_, _, None) => 1 /* constructor name ID */
  | BadEntry(_) => 0;

let pad_variant_ann =
    (v: ConstructorMap.variant(Typ.t)): ConstructorMap.variant(Typ.t) =>
  switch (v) {
  | Variant(c, ann, payload) =>
    let needed = necessary_variant_ann_ids(v);
    let current = List.length(ann.ids);
    let ids = ann.ids @ List.init(max(0, needed - current), _ => Id.mk());
    Variant(
      c,
      {
        ...ann,
        ids,
      },
      payload,
    );
  | BadEntry(_) => v
  };

let rec pad_variant_anns = (ty: Typ.t): Typ.t => {
  let term: Typ.term =
    switch (ty.term) {
    | Sum(variants) =>
      Sum(
        List.map(
          fun
          | ConstructorMap.Variant(c, ann, payload) => {
              let v =
                ConstructorMap.Variant(
                  c,
                  ann,
                  Option.map(pad_variant_anns, payload),
                );
              pad_variant_ann(v);
            }
          | ConstructorMap.BadEntry(t) =>
            ConstructorMap.BadEntry(pad_variant_anns(t)),
          variants,
        ),
      )
    | Arrow(t1, t2) => Arrow(pad_variant_anns(t1), pad_variant_anns(t2))
    | Prod(ts) => Prod(List.map(pad_variant_anns, ts))
    | List(t) => List(pad_variant_anns(t))
    | TupLabel(t1, t2) =>
      TupLabel(pad_variant_anns(t1), pad_variant_anns(t2))
    | Parens(t) => Parens(pad_variant_anns(t))
    | Rec(tp, t) => Rec(tp, pad_variant_anns(t))
    | Poly(tp, t) => Poly(tp, pad_variant_anns(t))
    | Projector(d, t) => Projector(d, pad_variant_anns(t))
    | ProdProjection(t1, t2) =>
      ProdProjection(pad_variant_anns(t1), pad_variant_anns(t2))
    | ProdExtension(t1, t2) =>
      ProdExtension(pad_variant_anns(t1), pad_variant_anns(t2))
    | Unknown(_)
    | Atom(_)
    | DrvQuoteTy(_)
    | Label(_)
    | ExplicitNonlabel
    | Var(_)
    | ProofOf(_)
    | Sig(_) => ty.term
    };
  {
    ...ty,
    term,
  };
};

let pad_typ_ids = (ty: Typ.t): Typ.t => {
  let ty =
    Typ.map_term(
      ~f_typ=
        (cont, ty) => {
          let current_ids = ty.annotation.ids;
          let needed_ids = necessary_ids(ty);
          let ids =
            current_ids
            @ List.init(max(0, needed_ids - List.length(current_ids)), _ =>
                Id.mk()
              );
          cont({
            ...ty,
            annotation: {
              ids,
              secondary: ty.annotation.secondary,
            },
          });
        },
      ty,
    );
  pad_variant_anns(ty);
};
