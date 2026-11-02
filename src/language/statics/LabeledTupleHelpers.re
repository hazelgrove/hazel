/* Cursor record for labels a tuple case filled in or reordered. */

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
    rewrap(
      Tuple([
        TupLabel(Label(label) |> Pat.fresh, original_pattern) |> Pat.fresh,
      ]),
    );
  let (info: Info.pat, pat_elab, m) =
    analyze_elaborated(~ana, elaborated_pat, m);
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
  /* `pat_elab` is the wrapped pattern after its inner pattern has been
     elaborated. `elaborated_pat` still has the pre-elaboration inner term. */
  (info, pat_elab, store_info(elaborated_pat, info, m));
};
