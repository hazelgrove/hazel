let matched_arrow_inf:
  HTyp.t => (option((HTyp.t, HTyp.t)), list(InfTyp.inf_constraint)) =
  (htyp: HTyp.t) => {
    let typ = InfTyp.from_htyp(htyp);
    switch (typ) {
    | Unknown(prov) =>
      let unknown_lt = Unknown(Internal(Matched_arrow_L(prov)));
      let unknown_rt = Unknown(Internal(Matched_arrow_R(prov)));
      let pair = (unknown_lt |> InfTyp.to_htyp, unknown_rt |> InfTyp.to_htyp);
      let constraints = [(typ, Arrow(unknown_lt, unknown_rt))];
      (Some(pair), constraints);
    | Arrow(ty1, ty2) => (
        Some((ty1 |> InfTyp.to_htyp, ty2 |> InfTyp.to_htyp)),
        [],
      )
    | _ => (None, [])
    };
  };

let matched_prod_inf:
  HTyp.t => (option((HTyp.t, HTyp.t)), list(InfTyp.inf_constraint)) =
  (htyp: HTyp.t) => {
    let typ = InfTyp.from_htyp(htyp);
    switch (typ) {
    | Unknown(prov) =>
      let unknown_lt = Unknown(Internal(Matched_prod_L(prov)));
      let unknown_rt = Unknown(Internal(Matched_prod_R(prov)));
      let pair = (unknown_lt |> InfTyp.to_htyp, unknown_rt |> InfTyp.to_htyp);
      let constraints = [(typ, Prod(unknown_lt, unknown_rt))];
      (Some(pair), constraints);
    | Prod(ty1, ty2) => (
        Some((ty1 |> InfTyp.to_htyp, ty2 |> InfTyp.to_htyp)),
        [],
      )
    | _ => (None, [])
    };
  };

let matched_sum_inf:
  HTyp.t => (option((HTyp.t, HTyp.t)), list(InfTyp.inf_constraint)) =
  (htyp: HTyp.t) => {
    let typ = InfTyp.from_htyp(htyp);
    switch (typ) {
    | Unknown(prov) =>
      let unknown_lt = Unknown(Internal(Matched_sum_L(prov)));
      let unknown_rt = Unknown(Internal(Matched_sum_R(prov)));
      let pair = (unknown_lt |> InfTyp.to_htyp, unknown_rt |> InfTyp.to_htyp);
      let constraints = [(typ, Sum(unknown_lt, unknown_rt))];
      (Some(pair), constraints);
    | Sum(ty1, ty2) => (
        Some((ty1 |> InfTyp.to_htyp, ty2 |> InfTyp.to_htyp)),
        [],
      )
    | _ => (None, [])
    };
  };

let matched_list_inf:
  HTyp.t => (option(HTyp.t), list(InfTyp.inf_constraint)) =
  (htyp: HTyp.t) => {
    let typ = InfTyp.from_htyp(htyp);
    switch (typ) {
    | Unknown(prov) =>
      let unknown_list = Unknown(Internal(Matched_list(prov)));
      let constraints = [(typ, unknown_list)];
      (Some(unknown_list |> InfTyp.to_htyp), constraints);
    | List(ty_ls) => (Some(ty_ls |> InfTyp.to_htyp), [])
    | _ => (None, [])
    };
  };

let matched_arrow: HTyp.t => option((HTyp.t, HTyp.t)) =
  (typ: HTyp.t) => {
    let (pair, _) = matched_arrow_inf(typ);
    pair;
  };

let matched_sum: HTyp.t => option((HTyp.t, HTyp.t)) =
  (typ: HTyp.t) => {
    let (pair, _) = matched_sum_inf(typ);
    pair;
  };

let matched_list: HTyp.t => option(HTyp.t) =
  (typ: HTyp.t) => {
    let (ty, _) = matched_list_inf(typ);
    ty;
  };
