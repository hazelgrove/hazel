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
      ty: HTyp.t,
    )
    : option(list((Skel.t('op), HTyp.t)), list(HTyp.inf_constraint)) => {
  let skels = skel |> get_tuple_elements;
  let tys = ty |> HTyp.get_prod_elements;
  switch (ListUtil.opt_zip(skels, tys)) {
  | Some(pairs) => Some(pairs, [])
  | None =>
    switch (skels, tys) {
    | ([_], _) => Some([(skel, ty)], [])
    | (_, [Hole(_) as progenitor]) =>
      //here: this is reached if a tuple is zipped against a hole; need to change this to match to fresh id holes
      //need to generate a set of pairs of skels with necessary hole type, generating ids as needed while extending a
      //prod type for a constraint involving the base hole
      let acc_skel_pairings = 
        ((acc_skel_pairs, acc_holes): (list((Skel.t('op), HTyp.t)), list(HTyp.t))) (elt: Skel.t('op)) => {
        let fresh_hole = InfVar.gen_new_type_var();
        ((elt, HTyp.Hole(fresh_hole))::acc_skel_pairs, HTyp.Hole(fresh_hole)::acc_holes)
      };
      let (skel_pairings, holes) = List.fold_left acc_skel_pairings_and_holes ([], []) skels;
      let prod_typ = HTyp.Prod(holes);
      let constraints = [(progenitor, prod_typ)];
      Some(skel_pairings, constraints)
    | _ => None
    }
  };
};
