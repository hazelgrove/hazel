/* Labeled tuple statics shared by the Exp and Pat tuple cases. */

module Map = StaticsBase.Map;
module Info = StaticsBase.Info;

/* A tuple entry's label: the errors on it, and the label its entry has
   in the tuple's type. None leaves the entry unlabeled there: an
   explicitly unlabeled entry, or one whose label the analyzed type lacks,
   so that the label alone is marked and not the tuple too. A malformed
   label is an unknown label in the type. */
let check_label =
    (
      ~expected_labels: option(list(string)),
      ~duplicate_labels: list(string),
      l: Label.t,
    )
    : (list(Mark.t), option(Label.t)) =>
  switch (l.term) {
  | Label(name) =>
    switch (expected_labels) {
    | Some(expected) when !List.mem(name, expected) => (
        [Mark.InvalidLabel(name, expected)],
        None,
      )
    | _ =>
      let marks =
        List.mem(name, duplicate_labels)
          ? [Mark.DuplicateLabel(name, Label(name) |> Typ.temp)] : [];
      (marks, Some(Label.temp(Label(name))));
    }
  | ExplicitNonlabel => ([], None)
  | EmptyHole
  | Invalid(_)
  | MultiHole(_) => ([], Some(Label.temp(EmptyHole)))
  };

/* Compute expected labels from an analysis type. */
let expected_labels_of_ana = (ctx: Ctx.t, ana: Typ.t): option(list(string)) =>
  switch (MatchedTyp.head(ctx, ana).term) {
  | Prod(ts) =>
    Some(
      List.filter_map(t => Typ.match_tup_label(t) |> Option.map(fst), ts),
    )
  | _ => None
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
    let m = Map.add_info(Exp.info_ids(info.user_term), InfoExp(info), m);
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
    let m = Map.add_info(Pat.info_ids(info.user_term), InfoPat(info), m);
    (info, m);
  | None => (info, m)
  };
