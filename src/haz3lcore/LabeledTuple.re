open Util;

exception Exception;

[@deriving (show({with_path: false}), sexp, yojson)]
type label = string;

let equal_label = String.equal;

let map_entry:
  type e e' ann ann'.
    (e => e', ann => ann', Grammar.labeled_entry_t(ann, e)) =>
    Grammar.labeled_entry_t(ann', e') =
  (f, g, entry) => {
    switch (entry) {
    | Labeled(e) =>
      let (l, e') = e.term;
      Labeled({
        annotation: g(e.annotation),
        term: (Grammar.map_label_annotation(g, l), f(e')),
      });
    | Unlabeled(e) => Unlabeled(f(e))
    };
  };

let map_elements:
  type a b ann.
    (a => b, list(Grammar.labeled_entry_t(ann, a))) =>
    list(Grammar.labeled_entry_t(ann, b)) =
  (f, xs) => {
    List.map(
      (entry: Grammar.labeled_entry_t(ann, a)) =>
        map_entry(f, Fun.id, entry),
      xs,
    );
  };

let get_label =
    (entry: Grammar.Annotated.t(Grammar.labeled_entry_term('a, 't), 'a))
    : Grammar.label_t('a) => {
  fst(entry.term);
};
let get_elem =
    (entry: Grammar.Annotated.t(Grammar.labeled_entry_term('a, 't), 'a)): 't => {
  snd(entry.term);
};

let has_same_labels: (option((label, 'a)), option((label, 'b))) => bool =
  (left, right) => {
    switch (left, right) {
    | (Some((s1, _)), Some((s2, _))) => equal_label(s1, s2)
    | (_, _) => false
    };
  };

let project:
  type a b.
    Grammar.labeled_entry_t(a, b) => (option(Grammar.label_t(a)), b) =
  le => {
    switch (le) {
    | Labeled(e) => (Some(fst(e.term)), snd(e.term))
    | Unlabeled(e) => (None, e)
    };
  };

let from_parts:
  type a b.
    (~ann: a, option(Grammar.label_t(a)), b) =>
    Grammar.labeled_entry_t(a, b) =
  (~ann, label, e) => {
    switch (label) {
    | Some(l) =>
      Labeled({
        term: (l, e),
        annotation: ann,
      })
    | None => Unlabeled(e)
    };
  };

let entry_rep_id: Grammar.labeled_entry_t(IdTagged.IdTag.t, 'a) => Id.t =
  entry => {
    switch (entry) {
    | Labeled(e) => IdTagged.rep_id(e)
    | Unlabeled(e) => IdTagged.rep_id(e)
    };
  };

// This function should only be used for type checking labels
let match_labels: (label, label) => bool =
  (label1, label2) => {
    switch (label1, label2) {
    // Empty label is a placeholder for checking any label
    | ("", _)
    | (_, "") => true
    | (_, _) => label1 == label2
    };
  };

// returns a pair containing a list of option(label) and a list of 'a
// if 'a is a tuplabel, extracts the label but keeps the tuplabel together
let separate_and_keep_labels:
  'a 'b.
  ('a => option((label, 'b)), list('a)) =>
  (list(option(label)), list('a))
 =
  (get_label, es) => {
    let results =
      List.fold_left(
        ((ls, ns), e) =>
          switch (get_label(e)) {
          | Some((s1, _)) => (ls @ [Some(s1)], ns @ [e])
          | None => (ls @ [None], ns @ [e])
          },
        ([], []),
        es,
      );
    results;
  };

// Keeps the labels in x that exist in y. Maintains the order from x.
let intersect = (xs: list(label), ys: list(label)) => {
  List.filter_map(x => List.find_opt(equal_label(x), ys), xs);
};

// Takes a list of strings and returns a list of duplicates
// Can also be modified to get unique labels.
let get_duplicate_labels_base: list(label) => list(label) =
  labels => {
    let (duplicates, _seen) =
      List.fold_left(
        ((dupes, seen), label) =>
          if (List.exists(l => label == l, seen)) {
            List.exists(l => label == l, dupes)
              ? (dupes, seen) : (dupes @ [label], seen);
          } else {
            (dupes, seen @ [label]);
          },
        ([], []),
        labels,
      );
    duplicates;
  };

let get_duplicate_labels:
  'a 'b.
  ('a => option((label, 'b)), list('a)) => list(label)
 =
  (get_label, es) => {
    separate_and_keep_labels(get_label, es)
    |> fst
    |> List.filter_map(Fun.id)
    |> get_duplicate_labels_base;
  };

/**
 * Assumes all labels are unique.
 * Rearranges all the labels in `l2` to match the order of the labels in `l1`.
 * Labels are optional and should be reordered for all present labels first and then unlabeled fields matched up pairwise.
 * So labeled fields can be reordered and unlabeled ones can't. Also, add labels to the unlabeled.
 */
let rec rearrange_base:
  'b.
  (
    ~show_b: 'b => string=?,
    list(option(label)),
    list((option(label), 'b))
  ) =>
  list((option(label), 'b))
 =
  (~show_b=?, l1: list(option(label)), l2: list((option(label), 'b))) => {
    let l1_labels = List.filter_map(Fun.id, l1);
    let l2_labels = List.filter_map(fst, l2);
    let common_labels = intersect(l1_labels, l2_labels);

    switch (l1, l2) {
    | ([], _) => l2
    | (_, []) => []
    | ([Some(expected_label), ...remaining_expectations], remaining) =>
      let maybe_found = List.assoc_opt(Some(expected_label), remaining);

      switch (maybe_found) {
      | Some(found) =>
        [(Some(expected_label), found)]
        @ rearrange_base(
            ~show_b?,
            remaining_expectations,
            List.remove_assoc(Some(expected_label), remaining),
          )
      | None =>
        let (
          pre: list((option(label), 'b)),
          current: option((option(label), 'b)),
          post: list((option(label), 'b)),
        ) =
          ListUtil.split(remaining, ((label: option(label), _)) => {
            switch (label) {
            | Some(label) => !List.mem(label, common_labels)
            | None => true
            }
          });

        switch (current) {
        | Some((_existing_label, b)) =>
          [(Some(expected_label), b)]
          @ rearrange_base(~show_b?, remaining_expectations, pre @ post)
        | None => remaining
        };
      };
    | ([None, ...remaining_expectations], remaining) =>
      // Pick the first one that's not in common labels and then keep the rest in remaining
      let (
        pre: list((option(label), 'b)),
        current: option((option(label), 'b)),
        post: list((option(label), 'b)),
      ) =
        ListUtil.split(remaining, ((label: option(label), _)) => {
          switch (label) {
          | Some(label) => !List.mem(label, common_labels)
          | None => true
          }
        });
      switch (current) {
      | Some((_existing_label, b)) =>
        [(None, b)]
        @ rearrange_base(~show_b?, remaining_expectations, pre @ post)
      | None => remaining
      };
    };
  };

/**
 * Rearranges the elements of the list to match up the labels and adds missing labels.
 * This function is essentially another way to call `rearrange_base` using raw lists.
 * It utilizes functions to extract labels from `TupLabels` and a constructor for new `TupLabels`.
 * The function maintains the same ids if possible, ensuring that the rearranged list
 * preserves the original structure as closely as possible while accommodating any new labels.
 */
let rearrange:
  'a 'b.
  (
    'a => option((label, 'a)),
    'b => option((label, 'b)),
    list('a),
    list('b),
    (label, 'b) => 'b
  ) =>
  list('b)
 =
  (get_label1, get_label2, l1, l2, constructor) =>
    if (List.length(l1) != List.length(l2)) {
      l2;
    } else {
      // TODO: Error handling in case of bad arguments
      let l1' = fst(separate_and_keep_labels(get_label1, l1));
      let (l2_labels, l2_vals) = separate_and_keep_labels(get_label2, l2);
      let l2' = List.combine(l2_labels, l2_vals);
      let l2_reordered = rearrange_base(l1', l2');
      List.map(
        ((optional_label, b)) =>
          switch (optional_label) {
          | Some(label) =>
            switch (get_label2(b)) {
            | Some(_) => b
            | None => constructor(label, b)
            }
          | None => b
          },
        l2_reordered,
      );
    };

let find_label: ('a => option((label, 'a)), list('a), label) => option('a) =
  (filt, es, label) => {
    List.find_opt(
      e => {
        switch (filt(e)) {
        | Some((s, _)) => s == label
        | None => false
        }
      },
      es,
    );
  };
