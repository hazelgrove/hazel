let introduce_expression = (ty: Typ.t): option(Exp.t) => {
  switch (ty.term) {
  | Arrow(_, _) =>
    Some(
      Fun(EmptyHole |> Pat.fresh, EmptyHole |> Exp.fresh, None, None)
      |> Exp.fresh,
    )

  | Prod(ts) =>
    Some(
      Tuple(List.init(List.length(ts), _ => EmptyHole |> Exp.fresh))
      |> Exp.fresh,
    )
  | _ => None
  };
};

let already_parenthesized = (z: Zipper.t) => {
  let sibs = Siblings.trim_secondary(ZipperBase.sibs_with_sel(z));
  let parent = Ancestors.parent(z.relatives.ancestors);
  Option.map((p: Ancestor.t) => p.label, parent) == Some(["(", ")"])
  && sibs
  |> (((l, r)) => l @ r)
  |> List.length(_) == 1;
};

let introduce = (statics: Statics.Map.t, z: Zipper.t) => {
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
    let expression = introduce_expression(Typ.weak_head_normalize(ctx, ana));
    let seg =
      Option.map(
        ExpToSegment.exp_to_segment(
          ~already_paren=already_parenthesized(z),
          ~settings={
            inline: true,
            fold_case_clauses: false,
            fold_fn_bodies: false,
            hide_fixpoints: false,
            fold_cast_types: false,
            show_filters: true,
          },
        ),
        expression,
      );

    Option.map(
      seg =>
        Zipper.put_selection(Selection.mk(seg), z)
        |> Zipper.remold_regrout(Left, _),
      seg,
    );
  | _ => None
  };
};
