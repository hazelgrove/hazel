open Language;

/* Number of IDs required for ExpToSegment compatibility per type constructor */
let necessary_ids: Typ.t => int =
  ty => {
    switch (ty.term) {
    | Prod([]) => 1 /* Empty product (unit-like) */
    | Prod(tys) => List.length(tys) - 1 /* One ID per separator */
    | Sum(tys) => List.length(tys) + 1 /* Constructors + prefix */
    | _ => 1 /* Default for other type constructors */
    };
  };

/* Number of IDs required for a variant_ann by ExpToSegment */
let necessary_variant_ann_ids: ConstructorMap.variant(Typ.t) => int =
  fun
  | Variant(_, _, Some(_)) => 2 /* parens ID + constructor name ID */
  | Variant(_, _, None) => 1 /* constructor name ID */
  | BadEntry(_) => 0;

/* Pad variant_ann.ids to the count ExpToSegment expects */
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

/* Recursively pad variant_ann.ids throughout a type */
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
    | Label(_)
    | ExplicitNonlabel
    | Var(_)
    | ProofOf(_) => ty.term
    };
  {
    ...ty,
    term,
  };
};

/**
 * Pads type IDs to ensure ExpToSegment uses them instead of creating new ones,
 * preserving ID correspondence for styling. Test_PadIds property test that checks
 *  ExpToSegment compatibility and padding equivalence.
 */
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

/* Compute the is_dynamic predicate for a type, given static and dynamic types.
   Handles fresh ID assignment, padding, and diffing in one place. */
let compute_dynamic_ids =
    (~static_typ: Typ.t, ~dynamic_typ: Typ.t): (Id.t => bool, Typ.t) => {
  let dynamic_typ =
    dynamic_typ
    |> Grammar.map_typ_annotation(_ => IdTagged.IdTag.fresh())
    |> pad_typ_ids;
  let dynamic_ids: list(Id.t) = Typ.diff(static_typ, dynamic_typ);
  (id => List.mem(id, dynamic_ids), dynamic_typ);
};
