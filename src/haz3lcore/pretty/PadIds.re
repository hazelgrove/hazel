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

/**
 * Pads type IDs to ensure ExpToSegment uses them instead of creating new ones,
 * preserving ID correspondence for styling. Test_PadIds property test that checks
 *  ExpToSegment compatibility and padding equivalence.
 */
let pad_typ_ids = (ty: Typ.t): Typ.t => {
  Typ.map_term(
    ~f_typ=
      (cont, ty) => {
        let current_ids = ty.annotation.ids;
        let needed_ids = necessary_ids(ty);
        let ids =
          current_ids
          @ List.init(needed_ids - List.length(current_ids), _ => Id.mk());
        cont({
          ...ty,
          annotation: {
            ids: ids,
          },
        });
      },
    ty,
  );
};
