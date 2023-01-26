open Util.OptUtil.Syntax;

type status =
  | Solved(ITyp.t)
  | Unsolved(EqClass.t);

type t = (ITyp.t, status);

type annotation_map = Hashtbl.t(Id.t, status);

let empty_annotations = (): annotation_map => Hashtbl.create(20);

let accumulated_annotations = empty_annotations();

let annotations_enabled = ref(true);

let update_annoation_mode = annot_mode => {
  annotations_enabled := annot_mode;
};

let get_annotations = (inference_results: list(t)): annotation_map => {
  let id_and_status_if_type_hole = (result: t): option((Id.t, status)) => {
    switch (result) {
    | (Unknown(TypeHole(id)), status) => Some((id, status))
    | _ => None
    };
  };

  let elts = List.filter_map(id_and_status_if_type_hole, inference_results);
  let new_map = Hashtbl.create(List.length(elts));

  List.iter(((id, annot)) => Hashtbl.add(new_map, id, annot), elts);

  new_map;
};

let get_solution_of_id = (id: Id.t): option(ITyp.t) =>
  if (annotations_enabled^) {
    let* status = Hashtbl.find_opt(accumulated_annotations, id);
    switch (status) {
    | Solved(Unknown(_)) => None
    | Solved(ityp) => Some(ityp)
    | Unsolved(_) => None
    };
  } else {
    None;
  };

let svg_display_settings = (id: Id.t): (bool, bool) => {
  switch (Hashtbl.find_opt(accumulated_annotations, id)) {
  | Some(status) =>
    switch (status) {
    | Solved(Unknown(_)) => (true, false)
    | Solved(_) => (false, false)
    | Unsolved(_) => (true, true)
    }
  | None => (true, false)
  };
};

let get_cursor_inspect_result = (id: Id.t): option((bool, string)) =>
  if (annotations_enabled^) {
    let* status = Hashtbl.find_opt(accumulated_annotations, id);
    switch (status) {
    | Unsolved(eq_class) =>
      Some((false, EqClass.string_of_eq_class(eq_class)))
    | Solved(ityp) =>
      Some((true, ityp |> ITyp.ityp_to_typ |> Typ.typ_to_string))
    };
  } else {
    None;
  };

let add_on_new_annotations = (new_map): unit => {
  let add_new_elt = (new_k, new_v) => {
    Hashtbl.replace(accumulated_annotations, new_k, new_v);
  };
  Hashtbl.iter(add_new_elt, new_map);
};

let clear_annotations = () => {
  Hashtbl.reset(accumulated_annotations);
};

let condense = (eq_class: MutableEqClass.t, key: ITyp.t): status => {
  let (eq_class, err) = MutableEqClass.snapshot_class(eq_class, key);
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
