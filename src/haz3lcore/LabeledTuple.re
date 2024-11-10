open Util;

exception Exception;

[@deriving (show({with_path: false}), sexp, yojson)]
type label = string;

let equal: (option((label, 'a)), option((label, 'b))) => bool =
  (left, right) => {
    switch (left, right) {
    | (Some((s1, _)), Some((s2, _))) => String.equal(s1, s2)
    | (_, _) => false
    };
  };

let length = String.length;

let compare = String.compare;

let find_opt: ('a => bool, list('a)) => option('a) = List.find_opt;

// returns a pair containing a list of option(t) and a list of 'a
// if 'a is a tuplabel, separates the label from the element held by it.
let separate_labels:
  ('a => option((label, 'a)), list('a)) =>
  (list(option(label)), list('a)) =
  (get_label, es) => {
    let results =
      List.fold_left(
        ((ls, ns), e) =>
          switch (get_label(e)) {
          | Some((s1, e)) => (ls @ [Some(s1)], ns @ [e])
          | None => (ls @ [None], ns @ [e])
          },
        ([], []),
        es,
      );
    results;
  };

// returns a pair containing a list of option(t) and a list of 'a
// if 'a is a tuplabel, extracts the label but keeps the tuplabel together
let separate_and_keep_labels:
  ('a => option((label, 'a)), list('a)) =>
  (list(option(label)), list('a)) =
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

// TODO consider adding a t = (option(label), 'a)

let separate_labeled = (xs: list((option(label), 'a))) => {
  List.partition_map(
    ((l, a)) =>
      switch (l) {
      | None => Right(a)
      | Some(l) => Left((l, a))
      },
    xs,
  );
};

// TODO Performance
let intersect = (xs, ys) => {
  List.filter_map(x => List.find_opt((==)(x), ys), xs);
};

// TODO: can just use get_duplicate_labels and check if empty.
// Takes a list of strings and returns true if there are no duplicates.
let rec is_uniquely_labeled_base: list(label) => bool =
  labels => {
    let contains_duplicates =
      switch (labels) {
      | [] => false
      | [hd, ...tl] =>
        List.exists(l => hd == l, tl) || is_uniquely_labeled_base(tl)
      };
    !contains_duplicates;
  };

// TODO: Performance
// Takes a list of strings and returns a list of duplicates and list of uniques.
// Can also be modified to get unique labels.
let get_duplicate_and_unique_labels_base:
  list(label) => (list(label), list(label)) =
  labels => {
    let (duplicates, uniques) =
      List.fold_left(
        (acc, label) => {
          let (dupes, uniqs) = acc;
          List.exists(l => label == l, uniqs)
            ? List.exists(l => label == l, dupes)
                ? (dupes, uniqs) : (dupes @ [label], uniqs)
            : (dupes, uniqs @ [label]);
        },
        ([], []),
        labels,
      );
    (duplicates, uniques);
  };

// Takes in a get_label function and a list of elements and applys is_uniquely_labeled_base
let is_uniquely_labeled: 'a. ('a => option((label, 'a)), list('a)) => bool =
  (get_label, es) => {
    let labels = fst(separate_and_keep_labels(get_label, es));
    let labels =
      labels
      |> List.filter(x => Option.is_some(x))
      |> OptUtil.sequence
      |> OptUtil.get(() => []);
    is_uniquely_labeled_base(labels);
  };

let get_duplicate_and_unique_labels:
  'a.
  ('a => option((label, 'a)), list('a)) => (list(label), list(label))
 =
  (get_label, es) => {
    let labels = fst(separate_and_keep_labels(get_label, es));
    let labels =
      labels
      |> List.filter(x => Option.is_some(x))
      |> OptUtil.sequence
      |> OptUtil.get(() => []);
    get_duplicate_and_unique_labels_base(labels);
  };

// Assumes all labels are unique
// Rearranges all the labels in l2 to match the order of the labels in l1. Labels are optional and should me reordered for all present labels first and then unlabled fields matched up pairwise. So labeled fields can be reordered and unlabeled ones can't. Also add labels to the unlabeled.
// TODO Handle the unequal length case and extra labels case
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

// Basically another way to call rearrange_base using the raw lists, functions to extract labels from TupLabels, and constructor for new TupLabels.
// Maintains the same ids if possible
// TODO: clean up more
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
  (get_label1, get_label2, l1, l2, constructor) => {
    // TODO: Error handling in case of bad arguments
    let l1' = fst(separate_and_keep_labels(get_label1, l1));
    let (l2_labels, l2_vals) = separate_and_keep_labels(get_label2, l2);
    let l2' = List.combine(l2_labels, l2_vals);
    let l2_reordered = rearrange_base(l1', l2');
    List.map(
      ((optional_label, b)) =>
        switch (optional_label) {
        | Some(label) =>
          // TODO: probably can keep the same ids in a cleaner way
          switch (get_label2(b)) {
          | Some(_) => b
          | None => constructor(label, b)
          }
        | None => b
        },
      l2_reordered,
    );
  };

// rearrange two other lists to match the first list of labels.
// TODO: Ensure that the two lists match up with each other
// TODO: This function currently exists only to make the elaborator code cleaner. Probably can make more efficient
let rearrange2:
  'a 'b.
  (
    list(option(label)),
    'a => option((label, 'a)),
    'b => option((label, 'b)),
    list('a),
    list('b),
    (label, 'a) => 'a,
    (label, 'b) => 'b
  ) =>
  (list('a), list('b))
 =
  (labels, get_label1, get_label2, l1, l2, constructor1, constructor2) => {
    let (l1_labels, l1_vals) = separate_and_keep_labels(get_label1, l1);
    let l1' = List.combine(l1_labels, l1_vals);
    let l1_reordered = rearrange_base(labels, l1');
    let l1_rearranged =
      List.map(
        ((optional_label, b)) =>
          switch (optional_label) {
          | Some(label) =>
            // TODO: probably can keep the same ids in a cleaner way
            switch (get_label1(b)) {
            | Some(_) => b
            | None => constructor1(label, b)
            }
          | None => b
          },
        l1_reordered,
      );
    let (l2_labels, l2_vals) = separate_and_keep_labels(get_label2, l2);
    let l2' = List.combine(l2_labels, l2_vals);
    let l2_reordered = rearrange_base(labels, l2');
    let l2_rearranged =
      List.map(
        ((optional_label, b)) =>
          switch (optional_label) {
          | Some(label) =>
            // TODO: probably can keep the same ids in a cleaner way
            switch (get_label2(b)) {
            | Some(_) => b
            | None => constructor2(label, b)
            }
          | None => b
          },
        l2_reordered,
      );
    (l1_rearranged, l2_rearranged);
  };

let find_label: ('a => option((label, 'a)), list('a), label) => option('a) =
  (filt, es, label) => {
    find_opt(
      e => {
        switch (filt(e)) {
        | Some((s, _)) => compare(s, label) == 0
        | None => false
        }
      },
      es,
    );
  };
