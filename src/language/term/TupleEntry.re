/* An entry of a tuple: a value, labeled or not (Grammar.tuple_entry). */

[@deriving (show({with_path: false}), sexp, yojson)]
type t('e) = Grammar.tuple_entry(IdTagged.IdTag.t, 'e);

let value: t('e) => 'e =
  fun
  | Labeled(_, _, v)
  | Unlabeled(v) => v;

/* The entry's label name, if it is labeled with a well-formed one */
let label_name: t('e) => option(LabeledTuple.label) =
  fun
  | Labeled(_, l, _) => Label.name(l)
  | Unlabeled(_) => None;

/* label_name in the shape LabeledTuple's helpers take */
let get_label = (entry: t('e)): option((LabeledTuple.label, t('e))) =>
  label_name(entry) |> Option.map(name => (name, entry));

let map = (f: 'e1 => 'e2, entry: t('e1)): t('e2) =>
  switch (entry) {
  | Labeled(eq, l, v) => Labeled(eq, l, f(v))
  | Unlabeled(v) => Unlabeled(f(v))
  };

/* An entry labeled by statics or the evaluator rather than written by
   the user: its ids are fresh, so nothing in the syntax points at them. */
let labeled = (name: LabeledTuple.label, v: 'e): t('e) =>
  Labeled(
    IdTagged.IdTag.fresh(),
    IdTagged.fresh(Grammar.Label(name): TermBase.label_term),
    v,
  );

let find = (name: LabeledTuple.label, entries: list(t('e))): option('e) =>
  LabeledTuple.find_label(get_label, entries, name) |> Option.map(value);

/* The ids of the entries' `=` tiles */
let eq_ids = (entries: list(t('e))): list(Id.t) =>
  List.concat_map(
    fun
    | Grammar.Labeled(eq: IdTagged.IdTag.t, _, _) => eq.ids
    | Unlabeled(_) => [],
    entries,
  );

/* The entries with their values replaced, in order */
let with_values = (entries: list(t('e1)), vs: list('e2)): list(t('e2)) =>
  List.map2((entry, v) => map(_ => v, entry), entries, vs);
