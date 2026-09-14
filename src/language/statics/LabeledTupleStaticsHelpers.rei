/* Shared helpers for labeled tuple statics, factoring out the patterns common
   to the Exp and Pat Tuple/TupLabel cases in Statics. */

/* Split an analysis type into the modes for a TupLabel's label and its value.
   Unknown modes when the type is not a labeled one. */
let decompose_label_mode: (Ctx.t, Typ.t) => (Typ.t, Typ.t);

/* Check a label name against the expected and duplicate label lists, giving
   the label's own synthesized type, its marks, and whether it is invalid. */
let validate_label_name:
  (
    ~name: string,
    ~expected_labels: option(list(string)),
    ~duplicate_labels: list(string)
  ) =>
  (Typ.t, list(Mark.t), bool);

/* The synthesized type and marks for a TupLabel node, given its label analysis
   and the type of its value. */
let tup_label_self_type:
  (
    ~lab_name: option(string),
    ~label_invalid: bool,
    ~duplicate_labels: list(string),
    ~value_ty: Typ.t,
    ~label_is_empty_hole: bool,
    ~malformed_source: Any.t
  ) =>
  (Typ.t, list(Mark.t));

/* As tup_label_self_type, for a TupLabel outside a tuple, where there are no
   expected or duplicate labels to check against. */
let standalone_tup_label_self_type:
  (
    ~lab_name: option(string),
    ~value_ty: Typ.t,
    ~label_is_empty_hole: bool,
    ~malformed_source: Any.t
  ) =>
  (Typ.t, list(Mark.t));

/* The labels a tuple type calls for, in order. None when it is not a tuple. */
let expected_labels_of_ana: (Ctx.t, Typ.t) => option(list(string));

/* Two tuple types differing in shape: how many elements each has, which labels
   each carries, and which labels one has that the other does not. Element
   labels are positional, None for an unlabeled element. */
type shape_mismatch =
  pri {
    expected_labels: list(option(string)),
    actual_labels: list(option(string)),
    missing_labels: list(string),
    unexpected_labels: list(string),
  };

/* How ana and syn differ in shape, or None when they agree in arity and labels
   or either is not a tuple type — in which case the difference lies in a
   component and is reported there instead. */
let shape_mismatch:
  (Ctx.t, ~ana: Typ.t, ~syn: Typ.t) => option(shape_mismatch);

/* The label of every item that carries one of the given duplicated labels, one
   entry per occurrence. */
let expand_duplicate_labels:
  (
    ~match_tup_label: 'a => option((string, 'b)),
    ~unique_duplicates: list(string),
    list('a)
  ) =>
  list(string);

/* The labels of the given items that the expected set does not allow. Empty
   when nothing is expected. */
let compute_invalid_labels:
  (
    ~match_tup_label: 'a => option((string, 'b)),
    ~expected_labels: option(list(string)),
    list('a)
  ) =>
  list(string);

/* The malformed label sources reported by the children's own
   TupleLabelError marks. */
let collect_malformed_labels:
  (
    ~has_tup_label: 'info => bool,
    ~get_marks: 'info => list(Mark.t),
    list('info)
  ) =>
  list(Any.t);

/* The Prod type for a tuple node and the single TupleLabelError mark covering
   its malformed, duplicate, and invalid labels. */
let finalize_tuple_type:
  (
    ~duplicate_labels: list(string),
    ~invalid_labels: list(string),
    ~malformed_labels: list(Any.t),
    list(Typ.t)
  ) =>
  (Typ.t, list(Mark.t));

/* Record an inferred label on an info, updating the map to match. */
let apply_inferred_label_exp:
  (
    ~inferred_label: option(string),
    StaticsBase.Info.exp,
    StaticsBase.Map.t
  ) =>
  (StaticsBase.Info.exp, StaticsBase.Map.t);

let apply_inferred_label_pat:
  (
    ~inferred_label: option(string),
    StaticsBase.Info.pat,
    StaticsBase.Map.t
  ) =>
  (StaticsBase.Info.pat, StaticsBase.Map.t);
