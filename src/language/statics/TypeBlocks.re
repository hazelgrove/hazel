open Util;

/* Representative typed terms whose type has a head, painted with it. */
let painted_terms =
    (m: StaticsBase.Map.t): Id.Map.t((Blocks.paint, Info.ancestors)) =>
  Id.Map.fold(
    (id, info, acc) =>
      switch (Info.typed_of(info)) {
      | Some((ty, ctx, ancestors)) when Id.equal(id, Info.id_of(info)) =>
        switch (Typ.head_of(ctx, ty)) {
        | Some(head) => Id.Map.add(id, (Blocks.Type(head), ancestors), acc)
        | None => acc
        }
      | _ => acc
      },
    m,
    Id.Map.empty,
  );

let of_map = (m: StaticsBase.Map.t): list(Blocks.t) =>
  Blocks.nest(painted_terms(m));
