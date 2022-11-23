type status =
  | Solved(ITyp.t)
  | Unsolved(EqClass.t);

type t = (ITyp.t, status);

let status_to_string: status => string =
  fun
  | Solved(ityp) =>
    String.concat(
      "Solved: ",
      [ityp |> ITyp.sexp_of_t |> Sexplib.Sexp.to_string_hum],
    )
  | Unsolved(eqClass) =>
    String.concat(
      "Unsolved: ",
      [eqClass |> EqClass.sexp_of_t |> Sexplib.Sexp.to_string_hum],
    );

let t_to_string = ((ityp, status)) => {
  String.concat(
    "{For hole ",
    [
      ityp |> ITyp.sexp_of_t |> Sexplib.Sexp.to_string_hum,
      ", result is ",
      status_to_string(status),
      "}\n",
    ],
  );
};

let list_of_t_to_string = (statuses: list(t)): string => {
  let acc_str = (acc: string, elt: t) => {
    String.concat(acc, ["\n", t_to_string(elt)]);
  };
  List.fold_left(acc_str, "", statuses);
};

let condense = (eq_class: MutableEqClass.t): status => {
  let (eq_class, err) = MutableEqClass.snapshot_class(eq_class);
  let sorted_eq_class = EqClass.sort_eq_class(eq_class);
  let filtered_eq_class =
    EqClass.filter_unneeded_holes(EqClass.is_known, sorted_eq_class);

  switch (err) {
  | Some(_) => Unsolved(filtered_eq_class)
  | None =>
    let solved_opt = EqClass.filtered_eq_class_to_typ(filtered_eq_class);
    switch (solved_opt) {
    | Some(typ) => Solved(typ)
    | None => Unsolved(filtered_eq_class)
    };
  };
};

let rec prov_to_priority = (prov: Typ.type_provenance): int => {
  switch (prov) {
  | Anonymous => (-1)
  | SynSwitch(id)
  | TypeHole(id)
  | Internal(id) => id
  | Inference(_, prov) => prov_to_priority(prov)
  };
};

let rec convert_leftmost_to_priority = (typ: ITyp.t): int => {
  switch (typ) {
  | Int
  | Unit
  | Float
  | String
  | Bool => (-1)
  | Unknown(prov) => prov_to_priority(prov)
  | List(elt_typ) => convert_leftmost_to_priority(elt_typ)
  | Arrow(typ_lhs, typ_rhs)
  | Prod(typ_lhs, typ_rhs)
  | Sum(typ_lhs, typ_rhs) =>
    let lhs = convert_leftmost_to_priority(typ_lhs);
    let rhs = convert_leftmost_to_priority(typ_rhs);
    switch (lhs, rhs) {
    | ((-1), (-1)) => (-1)
    | ((-1), _) => rhs
    | _ => lhs
    };
  };
};

let comp_results = ((ty1, _): t, (ty2, _): t): int => {
  let priority1 = convert_leftmost_to_priority(ty1);
  let priority2 = convert_leftmost_to_priority(ty2);
  Stdlib.compare(priority1, priority2);
};
