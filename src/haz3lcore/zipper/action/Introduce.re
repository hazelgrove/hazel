let introduce_expression = (ty: Typ.t): option(Exp.t) => {
  IdTagged.FreshGrammar.(
    Exp.(
      switch (ty.term) {
      | Arrow(_, _) => Some(fn(Pat.empty_hole(), empty_hole(), None, None))
      | Prod(ts) =>
        Some(tuple(List.init(List.length(ts), _ => empty_hole())))
      | Sum([Variant(c, _, None)]) => Some(constructor(c, None))
      | Sum([Variant(c, _, Some(_))]) =>
        Some(ap(Forward, constructor(c, None), empty_hole()))
      | Forall(_, _) => Some(typ_fun(TPat.empty_hole(), empty_hole(), None))
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
    open Util.OptUtil.Syntax;

    let+ expression =
      introduce_expression(Typ.weak_head_normalize(ctx, ana));
    let seg =
      ExpToSegment.exp_to_segment(
        ~already_paren=already_parenthesized(z),
        ~settings={
          inline: true,
          fold_case_clauses: false,
          fold_fn_bodies: false,
          hide_fixpoints: false,
          fold_cast_types: false,
          show_filters: true,
          show_unknown_as_hole: true,
        },
        expression,
      );

    z
    |> Zipper.put_selection(Selection.mk(seg), _)
    |> Zipper.remold_regrout(Left, _);
  | _ => None
  };
};
