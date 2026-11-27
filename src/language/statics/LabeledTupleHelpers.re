/* Cursor record for labels a tuple case filled in or reordered. */

let derive_label_inference_info = (original_labels, new_labels) => {
  let introduced_labels =
    List.filter(
      ~f=
        l =>
          !
            List.mem(
              List.filter_map(~f=Fn.id, original_labels),
              l,
              ~equal=String.equal,
            ),
      List.filter_map(~f=Fn.id, new_labels),
    );
  let reordered =
    !
      List.equal(
        (a, b) => {
          switch (a, b) {
          | (Some(a), Some(b)) => String.equal(a, b)
          | (Some(a), None) =>
            List.mem(introduced_labels, a, ~equal=String.equal)
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
