/**
 * Given a `skel` in analytic position against type `ty`,
 * `tuple_zip(~get_tuple_elements, skel, ty)` attempts to
 * zip together the tuple components of `skel` and `ty`.
 * Zipping succeeds either when `skel` and `ty` have the
 * same number of tuple components, when `skel` consists of
 * a single tuple components, or when `ty` consists of a
 * single tuple component that is `Hole`.
 */
let tuple_zip =
    (
      ~get_tuple_elements: Skel.t('op) => list(Skel.t('op)),
      skel: Skel.t('op),
      ty: HTyp.head_normalized,
    )
    : option(list((Skel.t('op), HTyp.t))) => {
  let skels = skel |> get_tuple_elements;
  let tys = HTyp.get_prod_elements(ty);
  switch (ListUtil.opt_zip(skels, tys)) {
  | Some(_) as zipped => zipped
  | None =>
    switch (skels, tys) {
    | ([_], _) => Some([(skel, HTyp.of_head_normalized(ty))])
    | (_, [ty]) when HTyp.is_hole(ty) =>
      skels |> List.map(skel => (skel, HTyp.hole())) |> Option.some
    | _ => None
    }
  };
};
