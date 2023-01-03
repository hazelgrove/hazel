type status =
  | Solved(ITyp.t)
  | Unsolved(EqClass.t);

type t = (ITyp.t, status);

type annotation_map = list((Id.t, string));

let get_annotations = (inference_results: list(t)): annotation_map => {
  let status_to_string = (status: status): string => {
    switch (status) {
    | Solved(ityp) => ITyp.string_of_ityp(ityp)
    | Unsolved(eq_class) => EqClass.string_of_eq_class(eq_class)
    };
  };

  let id_and_annotation_if_type_hole = (result: t): option((Id.t, string)) => {
    switch (result) {
    | (Unknown(TypeHole(id)), status) =>
      Some((id, status_to_string(status)))
    | _ => None
    };
  };

  List.filter_map(id_and_annotation_if_type_hole, inference_results);
};

let get_annotation_of_id =
    (annotation_map: annotation_map, id: Id.t): option(string) => {
  let get_annotation_if_for_id = ((k, v)) => k == id ? Some(v) : None;

  let get_annotation_text =
      (possible_annotations: list(string)): option(string) => {
    switch (possible_annotations) {
    | [] => None
    | [hd, ..._tl] => Some(hd)
    };
  };

  annotation_map
  |> List.filter_map(get_annotation_if_for_id)
  |> get_annotation_text;
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
