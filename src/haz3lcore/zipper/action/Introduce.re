module type Introducable = {
  type t;
  let parse: Segment.t => t;
  let is_hole: t => bool;

  /**
 * Introduces a new term of the specified type.
 *
 * @param ty - The type of the term to be introduced.
 * @return An optional tuple containing:
 *   - The newly introduced term, which represents the expression to be introduced when the action is triggered on a hole of that type.
 *   - The `Id.t` which indicates the element the cursor should be on after the new term is introduced.
 *   - A boolean indicating whether the cursor should move one to the left after the term is generated.
       This is useful for cases where the cursor should be placed inside of an term like inside of an empty list or string.
 *   Returns `None` if the introduction fails, meaning there is no form for that type.
 */
  let introduce:
    (~turbo_mode: bool, Ctx.t, Typ.t) => option((t, Id.t, bool));
  let to_segment: (~settings: ExpToSegment.Settings.t, t, bool) => Segment.t;
};
module IntroducePat: Introducable with type t = Pat.t = {
  type t = Pat.t;
  let parse = selection =>
    MakeTerm.(pat(unsorted(Segment.skel(selection), selection)));
  let is_hole = (pat: Pat.t) => {
    switch (pat.term) {
    | EmptyHole => true
    | _ => false
    };
  };

  let rec introduce = (~turbo_mode: bool, ctx: Ctx.t, ty: Typ.t) => {
    open IdTagged.FreshGrammar.Pat;
    let introduce_inner = (t: Typ.t): Pat.t =>
      if (turbo_mode) {
        introduce(~turbo_mode, ctx, t)
        |> Option.map(((exp, _, _)) => exp, _)
        |> Option.value(~default=empty_hole());
      } else {
        empty_hole();
      };
    (
      switch (Typ.weak_head_normalize(ctx, ty).term) {
      | Prod([]) =>
        Some(tuple([]) |> (pat => (pat, List.hd(pat.annotation.ids))))
      | Prod([t, ...ts]) =>
        let (head_element, head_id) =
          introduce_inner(t)
          |> (hole => (hole, List.hd(hole.annotation.ids)));

        Some((
          tuple([head_element, ...List.map(t => introduce_inner(t), ts)]),
          head_id,
        ));
      | Sum([Variant(c, _, None)]) =>
        Some(
          constructor(c, None)
          |> (pat => (pat, List.hd(pat.annotation.ids))),
        )
      | Sum([Variant(c, _, Some(t))]) =>
        Some(
          introduce_inner(t)
          |> (
            pat => (
              ap(constructor(c, None), pat),
              List.hd(pat.annotation.ids),
            )
          ),
        )
      | _ => None
      }:
        option((Pat.t, Id.t))
    )
    |> Option.map(((a, b)) => (a, b, false));
  };

  let to_segment = (~settings, pattern, already_parenthesized) =>
    ExpToSegment.any_to_segment(
      ~already_paren=already_parenthesized,
      ~settings,
      Pat(pattern),
    );
};

module IntroduceExp: Introducable with type t = Exp.t = {
  type t = Exp.t;
  let parse = selection =>
    MakeTerm.(exp(unsorted(Segment.skel(selection), selection)));
  let is_hole = (exp: Exp.t) => {
    switch (exp.term) {
    | EmptyHole => true
    | _ => false
    };
  };
  let rec introduce = (~turbo_mode: bool, ctx: Ctx.t, ty: Typ.t) => {
    open IdTagged.FreshGrammar.Exp;
    let introduce_inner = t =>
      if (turbo_mode) {
        introduce(~turbo_mode, ctx, t)
        |> Option.map(((exp, _, _)) => exp, _)
        |> Option.value(~default=empty_hole());
      } else {
        empty_hole();
      };
    (
      switch (Typ.weak_head_normalize(ctx, ty).term) {
      | Arrow(_, body_t) =>
        let cursor_pat = IdTagged.FreshGrammar.Pat.empty_hole();
        Some((
          fn(cursor_pat, introduce_inner(body_t), None, None),
          List.hd(cursor_pat.annotation.ids),
          false,
        ));
      | Prod([]) =>
        Some(
          tuple([]) |> (exp => (exp, List.hd(exp.annotation.ids), false)),
        )
      | Prod([t, ...ts]) =>
        let tuple_entry = (t: TermBase.Typ.t) => {
          let (tup_label, element) =
            switch (t) {
            | {term: TupLabel({term: Label(l), _}, t), _} =>
              introduce_inner(t)
              |> (elem => (tup_label(label(l), elem), elem))
            | _ => introduce_inner(t) |> (elem => (elem, elem))
            };
          (tup_label, List.hd(element.annotation.ids));
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
      | Sum([Variant(c, _, Some(t))]) =>
        let payload = introduce_inner(t);
        Some(
          payload
          |> (
            exp => (
              ap(Forward, constructor(c, None), exp),
              List.hd(exp.annotation.ids),
              false,
            )
          ),
        );
      | Forall(_, _) =>
        Some(
          IdTagged.FreshGrammar.TPat.empty_hole()
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
      | Atom(String) =>
        Some(
          string("") |> (exp => (exp, List.hd(exp.annotation.ids), true)),
        )
      | _ => None
      }
    )
    |> Option.map(((a, b, c)) => (a, b, c));
  };
  let to_segment = (~settings, expression, already_parenthesized) =>
    ExpToSegment.exp_to_segment(
      ~already_paren=already_parenthesized,
      ~settings,
      expression,
    );
};

module Make =
       (I: Introducable)
       : {
         let introduce:
           (~turbo_mode: bool=?, Zipper.t, Typ.t, Ctx.t) => option(Zipper.t);
       } => {
  let rec move_right_until_id = (id: Id.t, z: Zipper.t): Zipper.t =>
    ZipperBase.MapPiece.left_sib_has_id(z, id)
      ? z
      : (
        switch (Zipper.move(Right, z)) {
        | None => z
        | Some(z) => move_right_until_id(id, z)
        }
      );

  let already_parenthesized = (z: Zipper.t) => {
    let sibs = Siblings.trim_secondary(ZipperBase.sibs_with_sel(z));
    let parent = Ancestors.parent(z.relatives.ancestors);
    Option.map((p: Ancestor.t) => p.label, parent) == Some(["(", ")"])
    && sibs
    |> (((l, r)) => l @ r)
    |> List.length(_) == 1;
  };

  let add_segment_to_zipper = (move_left, id, seg, z) => {
    z
    |> Zipper.replace_selection(Left, seg, _)
    |> Zipper.directional_unselect(Left, _)
    |> move_right_until_id(id, _)
    |> (
      move_left ? Util.OptUtil.replace(Move.primary(ByChar, Left)) : Fun.id
    );
  };

  let introduce = (~turbo_mode=false, z: Zipper.t, ty: Typ.t, ctx: Ctx.t) => {
    open Util.OptUtil.Syntax;
    let selection = z.selection.content;
    let selected_term = I.parse(selection);

    // This is to prevent replacing an pattern that is not an empty hole
    let* _ = I.is_hole(selected_term) ? Some() : None;

    let+ (term, id, move_left) = I.introduce(~turbo_mode, ctx, ty);

    let seg =
      I.to_segment(
        ~settings={
          inline: true,
          fold_case_clauses: false,
          fold_fn_bodies: false,
          hide_fixpoints: false,
          fold_cast_types: false,
          show_filters: true,
          show_unknown_as_hole: true,
        },
        term,
        already_parenthesized(z),
      );

    add_segment_to_zipper(move_left, id, seg, z);
  };
};

let introduce = (~turbo_mode=false, statics: Statics.Map.t, z: Zipper.t) => {
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
    module IE = Make(IntroduceExp);
    IE.introduce(~turbo_mode, z, ana, ctx);

  | Some(
      InfoPat({
        cls: Pat(EmptyHole),
        status: NotInHole(Ana(Consistent({ana, _}))),
        ctx,
        _,
      }),
    ) =>
    module IP = Make(IntroducePat);
    IP.introduce(~turbo_mode, z, ana, ctx);
  | _ => None
  };
};
