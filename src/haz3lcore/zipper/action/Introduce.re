/**
 * Introduces a new expression of the specified type.
 *
 * @param ty - The type of the expression to be introduced.
 * @return An optional tuple containing:
 *   - The newly introduced expression (`Exp.t`), which represents the expression to be introduced when the action is triggered on a hole of that type.
 *   - The `Id.t` which indicates the element the cursor should be on after the new expression is introduced.
 *   - A boolean indicating whether the cursor should move one to the left after the expression is generated.
       This is useful for cases where the cursor should be placed inside of an expression like inside of an empty list or string.
 *   Returns `None` if the introduction fails, meaning there is no form for that type.
 */
let introduce_expression = (ty: Typ.t): option((Exp.t, Id.t, bool)) => {
  IdTagged.FreshGrammar.(
    Exp.(
      switch (ty.term) {
      | Arrow(_, _) =>
        let cursor_pat = Pat.empty_hole();
        Some((
          fn(cursor_pat, empty_hole(), None, None),
          List.hd(cursor_pat.annotation.ids),
          false,
        ));
      | Prod([]) =>
        Some(
          tuple([]) |> (exp => (exp, List.hd(exp.annotation.ids), false)),
        )
      | Prod([t, ...ts]) =>
        let tuple_entry = (t: TermBase.Typ.t) => {
          let hole = empty_hole();
          (
            switch (t) {
            | {term: TupLabel({term: Label(l), _}, _), _} =>
              tup_label(label(l), hole)
            | _ => hole
            },
            List.hd(hole.annotation.ids),
          );
        };

        let (head_element, head_id) = tuple_entry(t);

        Some((
          tuple([
            head_element,
            ...List.map(t => t |> tuple_entry |> fst, ts),
          ]),
          head_id,
          false,
        ));
      | Sum([Variant(c, _, None)]) =>
        Some(
          constructor(c, None)
          |> (exp => (exp, List.hd(exp.annotation.ids), false)),
        )
      | Sum([Variant(c, _, Some(_))]) =>
        Some(
          empty_hole()
          |> (
            exp => (
              ap(Forward, constructor(c, None), exp),
              List.hd(exp.annotation.ids),
              false,
            )
          ),
        )
      | Forall(_, _) =>
        Some(
          TPat.empty_hole()
          |> (
            exp => (
              typ_fun(exp, empty_hole(), None),
              List.hd(exp.annotation.ids),
              false,
            )
          ),
        )
      | List(_) =>
        Some(
          list_lit([]) |> (exp => (exp, List.hd(exp.annotation.ids), true)),
        )
      | String =>
        Some(
          string("") |> (exp => (exp, List.hd(exp.annotation.ids), true)),
        )
      | _ => None
      }
    )
  );
};

/**
 * Introduces a new pattern of the specified type.
 *
 * @param ty - The type of the pattern to be introduced.
 * @return An optional tuple containing:
 *   - The newly introduced pattern (`Pat.t`), which represents the pattern to be introduced when the action is triggered on a pattern hole of that type.
 *   - The `Id.t` which indicates the element the cursor should be on after the new pattern is introduced.
 *   Returns `None` if the introduction fails, meaning there is no form for that type.
 */
let introduce_pattern = (ty: Typ.t): option((Pat.t, Id.t)) => {
  IdTagged.FreshGrammar.(
    Pat.(
      switch (ty.term) {
      | Prod([]) =>
        Some(tuple([]) |> (pat => (pat, List.hd(pat.annotation.ids))))
      | Prod([_, ...ts]) =>
        let (head_element, head_id) =
          empty_hole() |> (hole => (hole, List.hd(hole.annotation.ids)));

        Some((
          tuple([head_element, ...List.map(_ => empty_hole(), ts)]),
          head_id,
        ));
      | Sum([Variant(c, _, None)]) =>
        Some(
          constructor(c, None)
          |> (pat => (pat, List.hd(pat.annotation.ids))),
        )
      | Sum([Variant(c, _, Some(_))]) =>
        Some(
          empty_hole()
          |> (
            pat => (
              ap(constructor(c, None), pat),
              List.hd(pat.annotation.ids),
            )
          ),
        )

      | _ => None
      }
    )
  );
};

let already_parenthesized = (z: Zipper.t) => {
  let sibs = Siblings.trim_secondary(ZipperBase.sibs_with_sel(z));
  let parent = Ancestors.parent(z.relatives.ancestors);
  Option.map((p: Ancestor.t) => p.label, parent) == Some(["(", ")"])
  && sibs
  |> (((l, r)) => l @ r)
  |> List.length(_) == 1;
};

let rec move_right_until_id = (id: Id.t, z: Zipper.t): Zipper.t =>
  ZipperBase.MapPiece.left_sib_has_id(z, id)
    ? z
    : (
      switch (Zipper.move(Right, z)) {
      | None => z
      | Some(z) => move_right_until_id(id, z)
      }
    );

let introduce = (statics: Statics.Map.t, z: Zipper.t) => {
  let settings: ExpToSegment.Settings.t = {
    inline: true,
    fold_case_clauses: false,
    fold_fn_bodies: false,
    hide_fixpoints: false,
    fold_cast_types: false,
    show_filters: true,
    show_unknown_as_hole: true,
  };

  switch (Indicated.ci_of(z, statics)) {
  | None => None
  | Some(
      InfoExp({
        cls: Exp(EmptyHole),
        status: NotInHole(Common(Ana(Consistent({ana, _})))),
        ctx,
        _,
      }),
    ) =>
    open Util.OptUtil.Syntax;
    let selection = z.selection.content;
    let selected_expression = MakeTerm.go(selection);

    // This is to prevent replacing an expression that is not an empty hole
    let* _ =
      switch (selected_expression.term.term) {
      | EmptyHole => Some()
      | _ => None
      };

    let+ (expression, id, move_left) =
      introduce_expression(Typ.weak_head_normalize(ctx, ana));

    let seg =
      ExpToSegment.exp_to_segment(
        ~already_paren=already_parenthesized(z),
        ~settings,
        expression,
      );
    z
    |> Zipper.replace_selection(Left, seg, _)
    |> Zipper.directional_unselect(Left, _)
    |> move_right_until_id(id, _)
    |> (
      move_left ? Util.OptUtil.replace(Move.primary(ByChar, Left)) : Fun.id
    );
  | Some(
      InfoPat({
        cls: Pat(EmptyHole),
        status: NotInHole(Ana(Consistent({ana, _}))),
        ctx,
        _,
      }),
    ) =>
    open Util.OptUtil.Syntax;
    let selection = z.selection.content;
    let selected_pattern =
      MakeTerm.(pat(unsorted(Segment.skel(selection), selection)));

    // This is to prevent replacing an pattern that is not an empty hole
    let* _ =
      switch (selected_pattern.term) {
      | EmptyHole => Some()
      | _ => None
      };

    let+ (pattern, id) =
      introduce_pattern(Typ.weak_head_normalize(ctx, ana));

    let seg =
      ExpToSegment.any_to_segment(
        ~already_paren=already_parenthesized(z),
        ~settings,
        Pat(pattern),
      );
    z
    |> Zipper.replace_selection(Left, seg, _)
    |> Zipper.directional_unselect(Left, _)
    |> move_right_until_id(id, _);
  | _ => None
  };
};
