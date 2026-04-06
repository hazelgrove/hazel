/* Labeled tuple alignment helpers for elaboration.
   These depend on Ctx and Typ.normalize so they live in the statics layer
   rather than in the generic LabeledTuple term utility. */

/* Align an elaborated expression with an expected labeled tuple type.
   Recursively rearranges and wraps tuple elements to match the expected labels. */
let rec align_exp = (ctx: Ctx.t, expected_ty: Typ.t, exp: Exp.t): Exp.t =>
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
          Typ.match_tup_label, Exp.match_tup_label, tys, ds, (label, body) => {
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
let rec is_aligned_exp = (ctx: Ctx.t, expected_ty: Typ.t, exp: Exp.t): bool =>
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
             | None => is_aligned_exp(ctx, expected_entry, exp_entry)
             },
           tys,
           ds,
         )
    | _ => false
    }
  | _ => true
  };

/* Align only when the expression is not already in the expected shape. */
let align_exp_if_needed = (ctx: Ctx.t, expected_ty: Typ.t, exp: Exp.t): Exp.t =>
  is_aligned_exp(ctx, expected_ty, exp) ? exp : align_exp(ctx, expected_ty, exp);

/* Lift an expression into a singleton labeled tuple and preserve original status. */
let autolabel_singleton_exp =
    (
      ~analyze_original,
      ~analyze_elaborated,
      ~store_info,
      uexp: Exp.t,
      ~inner_ty,
      ~ana,
      ~label,
      m,
    ) => {
  let (term, rewrap) = Exp.unwrap(uexp);
  let original_expression = Exp.fresh(term);
  let (original_info: Info.exp, _, m) =
    analyze_original(~ana=inner_ty, original_expression, m);
  let elaborated_exp =
    rewrap(
      Tuple([
        TupLabel(Label(label) |> Exp.fresh, original_expression) |> Exp.fresh,
      ]),
    );
  let (info: Info.exp, _, m) = analyze_elaborated(~ana, elaborated_exp, m);
  let info = {
    ...info,
    status: original_info.status,
    label_inference:
      Some(
        SingletonLabelInference({
          label: label,
          pre_labeled_info: original_info,
        }),
      ),
  };
  (info, elaborated_exp, store_info(elaborated_exp, info, m));
};

/* Lift a pattern into a singleton labeled tuple and preserve original status. */
let autolabel_singleton_pat =
    (
      ~analyze_original,
      ~analyze_elaborated,
      ~store_info,
      upat: Pat.t,
      ~inner_ty,
      ~ana,
      ~label,
      m,
    ) => {
  let (term, rewrap) = Pat.unwrap(upat);
  let original_pattern = Pat.fresh(term);
  let (original_info: Info.pat, _, m) =
    analyze_original(~ana=inner_ty, original_pattern, m);
  let elaborated_pat =
    rewrap(
      Tuple([
        TupLabel(Label(label) |> Pat.fresh, original_pattern) |> Pat.fresh,
      ]),
    );
  let (info: Info.pat, _, m) = analyze_elaborated(~ana, elaborated_pat, m);
  let info = {
    ...info,
    status: original_info.status,
    label_inference:
      Some(
        SingletonLabelInference({
          label: label,
          pre_labeled_info: original_info,
        }),
      ),
  };
  (info, elaborated_pat, store_info(elaborated_pat, info, m));
};
