open Language;

let necessary_ids: Typ.t => int =
  ty => {
    switch (ty.term) {
    | Parens(_) => 1
    | Prod([]) => 1
    | Prod(tys) => List.length(tys) - 1
    | Sum(tys) => List.length(tys) + 1
    | _ => 1
    };
  };

/*
  * This function pads the ids of a type to ensure that there are enough ids to be used so that ExpToSegment does not need to create new ids.
  * This is important for maintaining the correspondence between type ids and the resulting segment ids. Specifically if you're using the the ids from the type to
  * affect styling in the segment, you want to ensure that the ids in the type are the ones that get used.
  *
  * @param ty - The type to pad ids for.
  * @return A new type with padded ids.
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
