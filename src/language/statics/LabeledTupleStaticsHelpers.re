module Map = StaticsBase.Map;
module Info = StaticsBase.Info;

let decompose_label_mode = (ctx: Ctx.t, ana: Typ.t): (Typ.t, Typ.t) =>
  switch (MatchedTyp.label(ctx, ana)) {
  | Some((labmode, val_mode)) => (labmode, val_mode)
  | _ => (Unknown(SynSwitch) |> Typ.temp, Unknown(Internal) |> Typ.temp)
  };

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

let expected_labels_of_ana = (ctx: Ctx.t, ana: Typ.t): option(list(string)) =>
  switch (Typ.weak_head_normalize(ctx, ana).term) {
  | Prod(ts) =>
    Some(
      List.filter_map(t => Typ.match_tup_label(t) |> Option.map(fst), ts),
    )
  | _ => None
  };

type shape_mismatch = {
  expected_labels: list(option(string)),
  actual_labels: list(option(string)),
  missing_labels: list(string),
  unexpected_labels: list(string),
};

let shape_mismatch =
    (ctx: Ctx.t, ~ana: Typ.t, ~syn: Typ.t): option(shape_mismatch) => {
  let labels_of = ty =>
    switch (Typ.weak_head_normalize(ctx, ty).term) {
    | Prod(ts) =>
      Some(List.map(t => Typ.match_tup_label(t) |> Option.map(fst), ts))
    | _ => None
    };
  switch (labels_of(ana), labels_of(syn)) {
  | (Some(expected_labels), Some(actual_labels)) =>
    let names = l => List.filter_map(Fun.id, l);
    let missing_labels =
      names(expected_labels)
      |> List.filter(n => !List.mem(n, names(actual_labels)));
    let unexpected_labels =
      names(actual_labels)
      |> List.filter(n => !List.mem(n, names(expected_labels)));
    List.length(expected_labels) == List.length(actual_labels)
    && missing_labels == []
    && unexpected_labels == []
      ? None
      : Some({
          expected_labels,
          actual_labels,
          missing_labels,
          unexpected_labels,
        });
  | _ => None
  };
};

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
