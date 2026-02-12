open Language;

/* Predicates for checking if a type can be introduced.
   Used by ContextMenu to show/hide the Introduce action. */
let can_introduce_exp_type = (ty: Typ.t): bool =>
  switch (ty.term) {
  | Arrow(_, _)
  | Prod(_)
  | List(_)
  | Poly(_, _)
  | Atom(String) => true
  | Sum([_]) => true /* Single-variant sum only */
  | _ => false
  };

let can_introduce_pat_type = (ty: Typ.t): bool =>
  switch (ty.term) {
  | Prod(_) => true
  | Sum([_]) => true /* Single-variant sum only */
  | _ => false
  };

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
  let introduce: Typ.t => option((t, Id.t, bool));
  let to_segment: (~settings: ExpToSegment.Settings.t, t, bool) => Segment.t;
};

module IntroducePat: Introducable with type t = Pat.t = {
  type t = Pat.t;
  let parse = selection =>
    MakeTerm.(pat(unsorted(Pat, Segment.skel(selection), selection)));
  let is_hole = (pat: Pat.t) => {
    switch (pat.term) {
    | EmptyHole => true
    | _ => false
    };
  };

  let introduce = (ty: Typ.t) =>
    (
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
      ):
        option((Pat.t, Id.t))
    )
    |> Option.map(((a, b)) => (a, b, false));
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
    MakeTerm.(exp(unsorted(Exp, Segment.skel(selection), selection)));
  let is_hole = (exp: Exp.t) => {
    switch (exp.term) {
    | EmptyHole => true
    | _ => false
    };
  };
  let introduce = (ty: Typ.t) =>
    IdTagged.FreshGrammar.(
      Exp.(
        switch (ty.term) {
        | Arrow(_, _) =>
          let cursor_pat = Pat.empty_hole();
          Some((
            fn(cursor_pat, empty_hole()),
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
        | Poly(_, _) =>
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
            list_lit([])
            |> (exp => (exp, List.hd(exp.annotation.ids), true)),
          )
        | Atom(String) =>
          Some(
            string("") |> (exp => (exp, List.hd(exp.annotation.ids), true)),
          )
        | _ => None
        }
      )
    )
    |> Option.map(((a, b, c)) => (a, b, c));
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
         let introduce: (Zipper.t, Typ.t, Ctx.t) => option(Zipper.t);
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
    |> (move_left ? Util.OptUtil.replace(Move.local(ByChar, Left)) : Fun.id);
  };

  let introduce = (z: Zipper.t, ty: Typ.t, ctx: Ctx.t) => {
    open Util.OptUtil.Syntax;
    let selection = z.selection.content;
    let selected_term = I.parse(selection);

    // This is to prevent replacing an pattern that is not an empty hole
    let* _ = I.is_hole(selected_term) ? Some() : None;

    let+ (term, id, move_left) =
      I.introduce(Typ.weak_head_normalize(ctx, ty));

    let seg =
      I.to_segment(
        ~settings={
          secondary: AutoFormat,
          parenthesization: Defensive,
          label_format: QuoteWhenNecessary,
          inline: true,
          fold_case_clauses: false,
          project_tables: false,
          fold_fn_bodies: `NoFold,
          hide_fixpoints: false,
          show_filters: true,
          show_unknown_as_hole: true,
          raise_if_padding: false,
        },
        term,
        already_parenthesized(z),
      );

    add_segment_to_zipper(move_left, id, seg, z);
  };
};

let introduce = (ci: option(Info.t), z: Zipper.t) => {
  switch (ci) {
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
    IE.introduce(z, Typ.weak_head_normalize(ctx, ana), ctx);

  | Some(
      InfoPat({
        cls: Pat(EmptyHole),
        status: NotInHole(Ana(Consistent({ana, _}))),
        ctx,
        _,
      }),
    ) =>
    module IP = Make(IntroducePat);
    IP.introduce(z, Typ.weak_head_normalize(ctx, ana), ctx);
  | _ => None
  };
};
