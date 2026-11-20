/* Shared helpers for labeled tuple statics logic.
   These factor out the duplicated patterns in the Exp and Pat Tuple/TupLabel cases. */

module Map = StaticsBase.Map;
module Info = StaticsBase.Info;

let decompose_label_mode = (ctx: Ctx.t, ana: Typ.t): (Typ.t, Typ.t) =>
  switch (MatchedTyp.label(ctx, ana)) {
  | Some((labmode, val_mode)) => (labmode, val_mode)
  | _ => (Unknown(SynSwitch) |> Typ.temp, Unknown(Internal) |> Typ.temp)
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
  | Some(expected) when !List.mem(expected, name, ~equal=String.equal) => (
      SynTy.unknown_internal(),
      [Mark.InvalidLabel(name, expected)],
      true,
    )
  | _ =>
    List.mem(duplicate_labels, name, ~equal=String.equal)
      ? (
        Label(name) |> Typ.temp,
        [Mark.DuplicateLabel(name, Label(name) |> Typ.temp)],
        false,
      )
      : (Label(name) |> Typ.temp, [], false)
  };

/* The type of a labeled entry whose value has type `value_ty`. */
let tup_label_type =
    (~lab_name: option(string), ~label_is_empty_hole: bool, value_ty: Typ.t)
    : Typ.t => {
  let label: Typ.t =
    switch (lab_name) {
    | Some(name) => Label(name) |> Typ.temp
    | None when label_is_empty_hole => Unknown(SynSwitch) |> Typ.temp
    | None => Unknown(Internal) |> Typ.temp
    };
  TupLabel(label, value_ty) |> Typ.temp;
};

/* The syn type of a TupLabel node, given its value's syn type, and the marks
   its label earns. */
let tup_label_syn_type =
    (
      ~lab_name: option(string),
      ~label_invalid: bool,
      ~duplicate_labels: list(string),
      ~value_ty: Typ.t,
      ~label_is_empty_hole: bool,
      ~malformed_source: Any.t,
    )
    : (Typ.t, list(Mark.t)) => {
  let typ = tup_label_type(~lab_name, ~label_is_empty_hole, value_ty);
  let error =
      (~malformed_labels=[], ~duplicate_labels=[], ~invalid_labels=[], ()) =>
    Mark.TupleLabelError({
      malformed_labels,
      duplicate_labels,
      invalid_labels,
      typ,
    });
  let marks =
    switch (lab_name) {
    | Some(name) when label_invalid => [error(~invalid_labels=[name], ())]
    | Some(name) when List.mem(duplicate_labels, name, ~equal=String.equal) => [
        error(~duplicate_labels=[name], ()),
      ]
    | Some(_) => []
    | None when label_is_empty_hole => []
    | None => [error(~malformed_labels=[malformed_source], ())]
    };
  (typ, marks);
};

/* Standalone TupLabel variant (no expected/duplicate labels to check) */
let standalone_tup_label_syn_type =
    (
      ~lab_name: option(string),
      ~value_ty: Typ.t,
      ~label_is_empty_hole: bool,
      ~malformed_source: Any.t,
    )
    : (Typ.t, list(Mark.t)) =>
  tup_label_syn_type(
    ~lab_name,
    ~label_invalid=false,
    ~duplicate_labels=[],
    ~value_ty,
    ~label_is_empty_hole,
    ~malformed_source,
  );

/* Compute expected labels from an analysis type. */
let expected_labels_of_ana = (ctx: Ctx.t, ana: Typ.t): option(list(string)) =>
  switch (MatchedTyp.head(ctx, ana).term) {
  | Prod(ts) =>
    Some(
      List.filter_map(
        ~f=t => Typ.match_tup_label(t) |> Option.map(~f=fst),
        ts,
      ),
    )
  | _ => None
  };

/* Why a tuple of type `syn` does not fit the tuple type `ana`, when their
   shapes differ: their sizes and, if the tuple has labels, the labels it is
   missing or should not have. None when either is not a tuple, or is the
   empty one, or their shapes agree: the types say more. */
let shape_mismatch_message =
    (ctx: Ctx.t, ~ana: Typ.t, ~syn: Typ.t): option(string) => {
  let labels = (ty: Typ.t) =>
    switch (MatchedTyp.head(ctx, ty).term) {
    | Prod([_, ..._] as tys) =>
      Some(
        List.map(tys, ~f=t => Typ.match_tup_label(t) |> Option.map(~f=fst)),
      )
    | _ => None
    };
  switch (labels(ana), labels(syn)) {
  | (Some(expected), Some(actual)) =>
    let expected_names = List.filter_opt(expected);
    let actual_names = List.filter_opt(actual);
    let not_in = (names, name) =>
      !List.mem(names, name, ~equal=String.equal);
    let missing =
      List.is_empty(actual_names)
        ? [] : List.filter(expected_names, ~f=not_in(actual_names));
    let unexpected = List.filter(actual_names, ~f=not_in(expected_names));
    if (List.length(expected) == List.length(actual)
        && List.is_empty(missing)
        && List.is_empty(unexpected)) {
      None;
    } else {
      let size = labels => {
        let n = List.length(labels);
        Printf.sprintf("%d element%s", n, n == 1 ? "" : "s");
      };
      let quoted = names =>
        String.concat(
          ~sep=", ",
          List.map(names, ~f=Token.quote_label_when_necessary),
        );
      let with_labels = names =>
        List.is_empty(names) ? "" : " with labels " ++ quoted(names);
      let extra = (what, names) =>
        switch (names) {
        | [] => ""
        | [_] => Printf.sprintf("; %s label %s", what, quoted(names))
        | _ => Printf.sprintf("; %s labels %s", what, quoted(names))
        };
      Some(
        Printf.sprintf(
          "Expecting a tuple of %s%s but got %s%s%s%s",
          size(expected),
          with_labels(expected_names),
          size(actual),
          with_labels(actual_names),
          extra("missing", missing),
          extra("unexpected", unexpected),
        ),
      );
    };
  | _ => None
  };
};

/* Expand per-occurrence duplicate labels from unique duplicates. */
let expand_duplicate_labels =
    (
      ~match_tup_label: 'a => option((string, 'b)),
      ~unique_duplicates: list(string),
      items: list('a),
    )
    : list(string) =>
  List.filter_map(
    ~f=
      item =>
        switch (match_tup_label(item)) {
        | Some((name, _))
            when List.mem(unique_duplicates, name, ~equal=String.equal) =>
          Some(name)
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
      ~f=
        item =>
          switch (match_tup_label(item)) {
          | Some((name, _))
              when !List.mem(expected, name, ~equal=String.equal) =>
            Some(name)
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
    ~f=
      (acc, info) =>
        switch (has_tup_label(info), Mark.highest(get_marks(info))) {
        | (true, Some(Mark.TupleLabelError({malformed_labels, _}))) =>
          acc @ malformed_labels
        | _ => acc
        },
    ~init=[],
    infos,
  );

/* The syn type of a Tuple node, given its entries' syn types, and the
   TupleLabelError mark its labels earn. */
let tuple_syn_type =
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
