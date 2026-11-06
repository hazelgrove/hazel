/* Labeled tuple alignment helpers for elaboration.
   These depend on Ctx and Typ.normalize so they live in the statics layer
   rather than in the generic LabeledTuple term utility. */

/* Align an elaborated expression with an expected labeled tuple type.
   Recursively rearranges and wraps tuple elements to match the expected labels. */
let rec align_exp = (ctx: Ctx.t, expected_ty: Typ.t, exp: Exp.t): Exp.t =>
  switch (Typ.term_of(MatchedTyp.head(ctx, expected_ty))) {
  | Prod(tys) =>
    let align_entry = (expected_entry, entry: TupleEntry.t(Exp.t)) =>
      switch (Typ.match_tup_label(expected_entry)) {
      | Some((label, inner_ty)) =>
        switch (TupleEntry.label_name(entry)) {
        | Some(label') when label == label' =>
          TupleEntry.map(align_exp(ctx, inner_ty), entry)
        | _ =>
          TupleEntry.labeled(
            label,
            align_exp(ctx, inner_ty, TupleEntry.value(entry)),
          )
        }
      | None =>
        TupleEntry.map(
          align_exp(ctx, TupleEntry.value(expected_entry)),
          entry,
        )
      };
    switch (exp.term) {
    | Parens(inner) => align_exp(ctx, expected_ty, inner)
    | Tuple(ds) =>
      let arranged =
        LabeledTuple.rearrange(
          Typ.get_label, TupleEntry.get_label, tys, ds, (label, entry) =>
          TupleEntry.labeled(label, TupleEntry.value(entry))
        );
      let arranged =
        List.length(tys) == List.length(arranged)
          ? List.map2(align_entry, tys, arranged) : arranged;
      {
        /* Preserve the input Tuple's id on the rearranged outer. This puts
         * the rearranged result under a user-source id that's in info_map,
         * so the incremental evaluator can cache it and `newly_dirty_vars`
         * can detect value changes by comparing at this id directly (without
         * having to walk every descendant). */
        ...exp,
        term: Tuple(arranged),
      };
    | _ =>
      switch (tys) {
      | [ty] =>
        switch (Typ.match_tup_label(ty)) {
        | Some((label, inner_ty)) =>
          Tuple([TupleEntry.labeled(label, align_exp(ctx, inner_ty, exp))])
          |> Exp.fresh
        | None => exp
        }
      | _ => exp
      }
    };
  | _ => exp
  };

/* Check if an elaborated expression already matches the expected labeled tuple shape. */
let rec is_aligned_exp = (ctx: Ctx.t, expected_ty: Typ.t, exp: Exp.t): bool =>
  switch (Typ.term_of(MatchedTyp.head(ctx, expected_ty))) {
  | Prod(tys) =>
    switch (exp.term) {
    | Parens(_) => false
    | Tuple(ds) =>
      List.length(tys) == List.length(ds)
      && List.for_all2(
           (expected_entry, entry) =>
             switch (Typ.match_tup_label(expected_entry)) {
             | Some((label, inner_ty)) =>
               switch (TupleEntry.label_name(entry)) {
               | Some(label') when label == label' =>
                 is_aligned_exp(ctx, inner_ty, TupleEntry.value(entry))
               | _ => false
               }
             | None =>
               is_aligned_exp(
                 ctx,
                 TupleEntry.value(expected_entry),
                 TupleEntry.value(entry),
               )
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
  is_aligned_exp(ctx, expected_ty, exp)
    ? exp : align_exp(ctx, expected_ty, exp);

let derive_label_inference_info = (original_labels, new_labels) => {
  let introduced_labels =
    List.filter(
      l => !List.mem(l, List.filter_map(Fun.id, original_labels)),
      List.filter_map(Fun.id, new_labels),
    );
  let reordered =
    !
      List.equal(
        (a, b) => {
          switch (a, b) {
          | (Some(a), Some(b)) => a == b
          | (Some(a), None) => List.mem(a, introduced_labels)
          | (None, Some(_)) => false
          | (None, None) => true
          }
        },
        new_labels,
        original_labels,
      );
  Info.MultiLabelInference({
    reordered,
    introduced_labels,
  });
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
    rewrap(Tuple([TupleEntry.labeled(label, original_pattern)]));
  let (info: Info.pat, _, m) = analyze_elaborated(~ana, elaborated_pat, m);
  let info = {
    ...info,
    message: original_info.message,
    label_inference:
      Some(
        SingletonLabelInference({
          label,
          pre_labeled_info: original_info,
        }),
      ),
  };
  (info, elaborated_pat, store_info(elaborated_pat, info, m));
};
