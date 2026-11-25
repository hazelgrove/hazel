open Language;

/* ERRORSITES.re — where the editor shows what statics marked.

   Statics marks the term where an error arises. The editor shows the mark
   there, with two exceptions: syntax a projector folds away has no position
   of its own, and the parentheses around a tuple are drawn as part of it. */

/* `id`, or failing that its nearest ancestor with a position: the projector
   that folds it away. None if neither it nor any ancestor has one, as for a
   term in another editor's part of a spliced statics map. */
let located =
    (~info_map: Statics.Map.t, ~measured: Measured.t, id: Id.t): option(Id.t) =>
  Stdlib.Seq.cons(id, () =>
    Stdlib.List.to_seq(Statics.Map.ancestors_of(id, info_map), ())
  )
  |> Stdlib.Seq.find(id =>
       Option.is_some(Measured.find_opt_by_id(id, measured))
     );

/* The parentheses directly around the tuple `id`, which are part of its
   syntax as the user sees it, so are drawn as part of its mark. */
let enclosing_parens = (~info_map: Statics.Map.t, id: Id.t): option(Id.t) =>
  switch (Statics.Map.ancestors_of(id, info_map)) {
  | [parent, ..._] =>
    switch (Statics.Map.lookup(parent, info_map)) {
    | Some(
        InfoExp({
          user_term: {term: Parens({term: Tuple(_), _} as tuple), _},
          _,
        }),
      )
        when Id.equal(Exp.rep_id(tuple), id) =>
      Some(parent)
    | Some(
        InfoPat({
          user_term: {term: Parens({term: Tuple(_), _} as tuple), _},
          _,
        }),
      )
        when Id.equal(Pat.rep_id(tuple), id) =>
      Some(parent)
    | _ => None
    }
  | [] => None
  };

/* The terms whose syntax draws the marks on `ids`, each once. */
let drawn =
    (~info_map: Statics.Map.t, ~measured: Measured.t, ids: list(Id.t))
    : list(Id.t) =>
  List.concat_map(ids, ~f=id =>
    switch (located(~info_map, ~measured, id)) {
    | Some(shown) when Id.equal(shown, id) =>
      [shown] @ Option.to_list(enclosing_parens(~info_map, id))
    | Some(shown) => [shown]
    | None => []
    }
  )
  |> List.dedup_and_sort(~compare=Id.compare);
