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
    | (_, [Unkown(_) as progenitor]) =>
      //here: this is reached if a tuple is zipped against a hole; need to change this to match to fresh id holes
      //need to generate a set of pairs of skels with necessary hole type, generating ids as needed while extending a
      //prod type for a constraint involving the base hole
      // let acc_skel_pairings =
      //   ((acc_skel_pairs, acc_holes): (list((Skel.t('op), HTyp.t)), list(HTyp.t))) (elt: Skel.t('op)) => {
      //   let fresh_hole = InfVar.gen_new_type_var();
      //   ((elt, HTyp.Hole(fresh_hole))::acc_skel_pairs, HTyp.Hole(fresh_hole)::acc_holes)
      // };

    //   let acc_skel_pairings =
    //     ((acc_skel_pairs, acc_holes, index): ( list((Skel.t('op), HTyp.t)), list(HTyp.t), int ) ) (elt: Skel.t('op)) => {
    //       //let prod_hole_i = Hole(base, provenances @ Matched_prod(index));
    //       let prod_hole_i = Unknown(Matched_prod_R(index))
    //       ( (elt, prod_hole_i)::acc_skel_pairs, prod_hole_i::acc_holes, (index + 1) )
    //     };

    //   let (skel_pairings, holes) = List.fold_left acc_skel_pairings_and_holes ([], [], 0) skels;

    //   let prod_typ = HTyp.Prod(holes);
    //   let constraints = [(progenitor, prod_typ)];

      // o is provenance of (1, 2, 3)
      // Num x (Num x Num)
      // Matched_prod_L(o), Matched_prod_L(Matched_prod_R(o)), Matched_prod_R(Matched_prod_R(o))
      // Num x (Num x (Num x Num))
      // Matched_prod_L(o), Matched_prod_L(Matched_prod_R(o)), Matched_prod_L(Matched_prod_R(Matched_prod_R(o))), Matched_prod_R(Matched_prod_R(Matched_prod_R(o)))
      // MPL o x (MPL (MPR o) x )
      // Matched_prod original -> MPL o, MPR o
      // Matched prod MPR o -> MPL (MPR o), MPR (MPR o)
        // for every element of the list of skels, pair up each skel with a Matched product R hole
        // created from the original one
        // original = 1st x 2nd
        // 3rd = 2nd

        let prov_of_typ = (skel: Skel.t('op), ty: HTyp.t) {
          
        }

        // constructs a tree of matched product holes and associated constraints
        // Takes in a hole from which to spawn a stick tree and skels onto which to superimpose
        // each node to construct a list of pairs from each skel in skels to a node in the tree
        // from left to right
        let rec acc_skel_mpr_pairs = (base: HTyp.t, skels: list(Skel.t('op))) => {
            switch(skels) {
                | [] => [base]
                | [hd, ...tl] => {
                    let ((lt_hole, rt_hole), ctrs) = HTyp.matched_prod_inf(base);
                    let (tl_pairs, tl_ctrs) = acc_skel_mpr_pairs(rt_hole, tl);
                    ([(lt_hole, hd), ...tl_pairs], ctrs @ tl_ctrs)
                }
            }
        };

        Some(acc_skel_mpr_pairs(skels, progenitor), constraints)
    // | ([_], _) => Some([(skel, ty)])
    // | (_, [Unknown(l)]) =>
    //   skels |> List.map(skel => (skel, HTyp.Unknown(l))) |> Option.some
    | _ => None
    }
  };
};
