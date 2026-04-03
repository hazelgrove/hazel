/* Labeled tuple alignment helpers for elaboration.
   These depend on Ctx and Typ.normalize so they live in the statics layer
   rather than in the generic LabeledTuple term utility. */

/* Align an elaborated expression with an expected labeled tuple type.
   Recursively rearranges and wraps tuple elements to match the expected labels. */
let rec align_exp =
        (ctx: Ctx.t, expected_ty: Typ.t, exp: Exp.t): Exp.t =>
  switch (Typ.term_of(Typ.normalize(ctx, expected_ty))) {
  | Prod(tys) =>
    let align_entry = (expected_entry, exp_entry) =>
      switch (Typ.match_tup_label(expected_entry)) {
      | Some((label, inner_ty)) =>
        switch (Exp.match_tup_label(exp_entry)) {
        | Some((label', inner_exp)) when label == label' =>
          TupLabel(
            Label(label) |> Exp.fresh,
            align_exp(ctx, inner_ty, inner_exp),
          )
          |> Exp.fresh
        | _ =>
          TupLabel(
            Label(label) |> Exp.fresh,
            align_exp(ctx, inner_ty, exp_entry),
          )
          |> Exp.fresh
        }
      | None => align_exp(ctx, expected_entry, exp_entry)
      };
    switch (exp.term) {
    | Parens(inner) => align_exp(ctx, expected_ty, inner)
    | Tuple(ds) =>
      let arranged =
        LabeledTuple.rearrange(
          Typ.match_tup_label,
          Exp.match_tup_label,
          tys,
          ds,
          (label, body) => {
          TupLabel(Label(label) |> Exp.fresh, body) |> Exp.fresh
        });
      let arranged =
        List.length(tys) == List.length(arranged)
          ? List.map2(align_entry, tys, arranged) : arranged;
      Tuple(arranged) |> Exp.fresh;
    | _ =>
      switch (tys) {
      | [ty] =>
        switch (Typ.match_tup_label(ty), Exp.match_tup_label(exp)) {
        | (Some((label, inner_ty)), Some((label', inner_exp)))
            when label == label' =>
          Tuple([
            TupLabel(
              Label(label) |> Exp.fresh,
              align_exp(ctx, inner_ty, inner_exp),
            )
            |> Exp.fresh,
          ])
          |> Exp.fresh
        | (Some((label, inner_ty)), _) =>
          Tuple([
            TupLabel(
              Label(label) |> Exp.fresh,
              align_exp(ctx, inner_ty, exp),
            )
            |> Exp.fresh,
          ])
          |> Exp.fresh
        | _ => exp
        }
      | _ => exp
      }
    };
  | _ => exp
  };

/* Check if an elaborated expression already matches the expected labeled tuple shape. */
let rec is_aligned_exp =
        (ctx: Ctx.t, expected_ty: Typ.t, exp: Exp.t): bool =>
  switch (Typ.term_of(Typ.normalize(ctx, expected_ty))) {
  | Prod(tys) =>
    switch (exp.term) {
    | Parens(_) => false
    | Tuple(ds) =>
      List.length(tys) == List.length(ds)
      && List.for_all2(
           (expected_entry, exp_entry) =>
             switch (Typ.match_tup_label(expected_entry)) {
             | Some((label, inner_ty)) =>
               switch (Exp.match_tup_label(exp_entry)) {
               | Some((label', inner_exp)) when label == label' =>
                 is_aligned_exp(ctx, inner_ty, inner_exp)
               | _ => false
               }
             | None =>
               is_aligned_exp(
                 ctx,
                 expected_entry,
                 exp_entry,
               )
             },
           tys,
           ds,
         )
    | _ => false
    }
  | _ => true
  };
