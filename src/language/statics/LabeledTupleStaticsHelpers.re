/* Shared helpers for labeled tuple statics logic.
   These factor out the duplicated patterns in the Exp and Pat Tuple/TupLabel cases. */

module Map = StaticsBase.Map;
module Info = StaticsBase.Info;

/* Apply a type-only alignment plan to syntax, retaining inferred labels
   alongside the original nodes for the cursor inspector. */
let align_elements = (~constructor, product: MatchedTyp.product, elements) => {
  let elements = Array.of_list(elements);
  List.map(
    ((i, inferred)) => {
      let element = elements[i];
      (
        inferred,
        switch (inferred) {
        | None => element
        | Some(label) => constructor(label, element)
        },
      );
    },
    product.alignment,
  );
};

/* Validate a label name against expected/duplicate label lists.
   Returns (elab_syn_ty, marks, is_invalid) for the label itself. */
let validate_label_name =
    (
      ~name: string,
      ~expected_labels: option(list(string)),
      ~duplicate_labels: list(string),
    )
    : (Typ.t, list(Mark.t), bool) =>
  switch (expected_labels) {
  | Some(expected) when !List.mem(name, expected) => (
      SynTy.unknown_internal(),
      [Mark.InvalidLabel(name, expected)],
      true,
    )
  | _ =>
    List.mem(name, duplicate_labels)
      ? (
        Label(name) |> Typ.temp,
        [Mark.DuplicateLabel(name, Label(name) |> Typ.temp)],
        false,
      )
      : (Label(name) |> Typ.temp, [], false)
  };

/* Compute the synthesized type and marks for a TupLabel node
   given its label analysis and child value type. */
let tup_label_self_type =
    (
      ~lab_name: option(string),
      ~label_invalid: bool,
      ~duplicate_labels: list(string),
      ~value_ty: Typ.t,
      ~label_is_empty_hole: bool,
      ~malformed_source: Any.t,
    )
    : (Typ.t, list(Mark.t)) =>
  switch (lab_name) {
  | Some(name) =>
    let labeled_syn =
      TupLabel(Label(name) |> Typ.temp, value_ty) |> Typ.temp;
    let marks =
      label_invalid
        ? [
          Mark.TupleLabelError({
            malformed_labels: [],
            duplicate_labels: [],
            invalid_labels: [name],
            typ: labeled_syn,
          }),
        ]
        : List.mem(name, duplicate_labels)
            ? [
              Mark.TupleLabelError({
                malformed_labels: [],
                duplicate_labels: [name],
                invalid_labels: [],
                typ: labeled_syn,
              }),
            ]
            : [];
    (labeled_syn, marks);
  | None when label_is_empty_hole => (
      TupLabel(Unknown(SynSwitch) |> Typ.temp, value_ty) |> Typ.temp,
      [],
    )
  | None => (
      TupLabel(Unknown(Internal) |> Typ.temp, value_ty) |> Typ.temp,
      [
        Mark.TupleLabelError({
          malformed_labels: [malformed_source],
          duplicate_labels: [],
          invalid_labels: [],
          typ: TupLabel(Unknown(Internal) |> Typ.temp, value_ty) |> Typ.temp,
        }),
      ],
    )
  };

/* Standalone TupLabel variant (no expected/duplicate labels to check) */
let standalone_tup_label_self_type =
    (
      ~lab_name: option(string),
      ~value_ty: Typ.t,
      ~label_is_empty_hole: bool,
      ~malformed_source: Any.t,
    )
    : (Typ.t, list(Mark.t)) =>
  switch (lab_name) {
  | Some(name) => (
      TupLabel(Label(name) |> Typ.temp, value_ty) |> Typ.temp,
      [],
    )
  | None when label_is_empty_hole => (
      TupLabel(Unknown(SynSwitch) |> Typ.temp, value_ty) |> Typ.temp,
      [],
    )
  | None => (
      TupLabel(Unknown(Internal) |> Typ.temp, value_ty) |> Typ.temp,
      [
        Mark.TupleLabelError({
          malformed_labels: [malformed_source],
          duplicate_labels: [],
          invalid_labels: [],
          typ: TupLabel(Unknown(Internal) |> Typ.temp, value_ty) |> Typ.temp,
        }),
      ],
    )
  };

/* Compute expected labels from an analysis type. */
let expected_labels_of_ana = (ctx: Ctx.t, ana: Typ.t): option(list(string)) =>
  switch (Typ.weak_head_normalize(ctx, ana).term) {
  | Prod(ts) =>
    Some(
      List.filter_map(t => Typ.match_tup_label(t) |> Option.map(fst), ts),
    )
  | _ => None
  };

let labels_of_type = (ctx: Ctx.t, typ: Typ.t) =>
  switch (Typ.weak_head_normalize(ctx, typ).term) {
  | Prod(types) =>
    Some(List.map(t => Typ.match_tup_label(t) |> Option.map(fst), types))
  | _ => None
  };

/* A shared CLI/inspector explanation of tuple shape, independent of component
   types. Actual labels come from the matching request, before error recovery
   can discard them. Same-shape component failures keep the usual type message. */
let tuple_shape_mismatch_message =
    (ctx: Ctx.t, ~ana: Typ.t, ~actual_labels: list(option(string))) =>
  switch (labels_of_type(ctx, ana)) {
  | None => None
  | Some(expected_labels) =>
    let expected_names = List.filter_map(Fun.id, expected_labels);
    let actual_names = List.filter_map(Fun.id, actual_labels);
    let missing =
      List.filter(n => !List.mem(n, actual_names), expected_names);
    let unexpected =
      List.filter(n => !List.mem(n, expected_names), actual_names);
    if (List.length(expected_labels) == List.length(actual_labels)
        && missing == []
        && unexpected == []) {
      None;
    } else {
      let count = labels => {
        let n = List.length(labels);
        Printf.sprintf("%d element%s", n, n == 1 ? "" : "s");
      };
      let labels = names =>
        switch (names) {
        | [] => ""
        | names => " with labels " ++ String.concat(", ", names)
        };
      let extra = (what, names) =>
        switch (names) {
        | [] => ""
        | [name] => Printf.sprintf("; %s label %s", what, name)
        | names =>
          Printf.sprintf("; %s labels %s", what, String.concat(", ", names))
        };
      Some(
        Printf.sprintf(
          "Expecting a tuple of %s%s but got %s%s%s%s",
          count(expected_labels),
          labels(expected_names),
          count(actual_labels),
          labels(actual_names),
          extra("missing", missing),
          extra("unexpected", unexpected),
        ),
      );
    };
  };

let shape_mismatch_message = (ctx: Ctx.t, ~ana: Typ.t, ~syn: Typ.t) =>
  labels_of_type(ctx, syn)
  |> Option.bind(_, actual_labels =>
       tuple_shape_mismatch_message(ctx, ~ana, ~actual_labels)
     );

/* Expand per-occurrence duplicate labels from unique duplicates. */
let expand_duplicate_labels =
    (
      ~match_tup_label: 'a => option((string, 'b)),
      ~unique_duplicates: list(string),
      items: list('a),
    )
    : list(string) =>
  List.filter_map(
    item =>
      switch (match_tup_label(item)) {
      | Some((name, _)) when List.mem(name, unique_duplicates) => Some(name)
      | _ => None
      },
    items,
  );

/* Compute invalid labels (labels not in the expected set). */
let compute_invalid_labels =
    (
      ~match_tup_label: 'a => option((string, 'b)),
      ~expected_labels: option(list(string)),
      items: list('a),
    )
    : list(string) =>
  switch (expected_labels) {
  | None => []
  | Some(expected) =>
    List.filter_map(
      item =>
        switch (match_tup_label(item)) {
        | Some((name, _)) when !List.mem(name, expected) => Some(name)
        | _ => None
        },
      items,
    )
  };

/* Collect malformed label sources from children's TupleLabelError marks. */
let collect_malformed_labels =
    (
      ~has_tup_label: 'info => bool,
      ~get_marks: 'info => list(Mark.t),
      infos: list('info),
    )
    : list(Any.t) =>
  List.fold_left(
    (acc, info) =>
      switch (has_tup_label(info), Mark.highest(get_marks(info))) {
      | (true, Some(Mark.TupleLabelError({malformed_labels, _}))) =>
        acc @ malformed_labels
      | _ => acc
      },
    [],
    infos,
  );

/* Build the final Prod type and TupleLabelError mark for a Tuple node. */
let finalize_tuple_type =
    (
      ~duplicate_labels: list(string),
      ~invalid_labels: list(string),
      ~malformed_labels: list(Any.t),
      ty_list: list(Typ.t),
    )
    : (Typ.t, list(Mark.t)) => {
  let ty_list = Typ.remove_duplicate_labels(~duplicate_labels, ty_list);
  let prod_ty = Prod(ty_list) |> Typ.temp;
  let marks =
    List.is_empty(malformed_labels)
    && List.is_empty(duplicate_labels)
    && List.is_empty(invalid_labels)
      ? []
      : [
        Mark.TupleLabelError({
          malformed_labels,
          duplicate_labels,
          invalid_labels,
          typ: prod_ty,
        }),
      ];
  (prod_ty, marks);
};

/* Apply inferred_label to an info record and update the map if needed.
   Works for both exp and pat by taking updater functions. */
let apply_inferred_label_exp =
    (~inferred_label: option(string), info: Info.exp, m: Map.t)
    : (Info.exp, Map.t) =>
  switch (inferred_label) {
  | Some(_) =>
    let info = {
      ...info,
      inferred_label,
    };
    let m = Map.add_info(IdTagged.ids(info.user_term), InfoExp(info), m);
    (info, m);
  | None => (info, m)
  };

let apply_inferred_label_pat =
    (~inferred_label: option(string), info: Info.pat, m: Map.t)
    : (Info.pat, Map.t) =>
  switch (inferred_label) {
  | Some(_) =>
    let info = {
      ...info,
      inferred_label,
    };
    let m = Map.add_info(IdTagged.ids(info.user_term), InfoPat(info), m);
    (info, m);
  | None => (info, m)
  };
