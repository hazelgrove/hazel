[@deriving sexp]
type edit_state = (ZExp.t, HTyp.t, MetaVarGen.t);

type type_mode =
  | Syn
  | Ana(HTyp.t);

let tuple_zip =
    (
      ~get_tuple_elements: Skel.t('op) => list(Skel.t('op)),
      skel: Skel.t('op),
      ty: HTyp.t,
    )
    : option(list((Skel.t('op), HTyp.t))) => {
  let skels = skel |> get_tuple_elements;
  let tys = ty |> HTyp.get_prod_elements;
  switch (ListUtil.opt_zip(skels, tys)) {
  | Some(_) as zipped => zipped
  | None =>
    switch (skels, tys) {
    | ([_], _) => Some([(skel, ty)])
    | (_, [Hole]) =>
      skels |> List.map(skel => (skel, HTyp.Hole)) |> Option.some
    | _ => None
    }
  };
};

let glb = (types: list(HTyp.t)): option(HTyp.t) => {
  switch (types) {
  | [] => None
  | [hd] => Some(hd)
  | [hd, ...tl] =>
    let rec exist_inconsistencies = l =>
      switch (l) {
      | [] => false
      | [hd, ...tl] =>
        if (List.exists(HTyp.inconsistent(hd), tl)) {
          true;
        } else {
          exist_inconsistencies(tl);
        }
      };
    if (exist_inconsistencies(types)) {
      None;
    } else {
      List.fold_left(
        (common_opt, ty) =>
          switch (common_opt) {
          | None => None
          | Some(common_ty) => HTyp.join(GLB, common_ty, ty)
          },
        Some(hd),
        tl,
      );
    };
  };
};
