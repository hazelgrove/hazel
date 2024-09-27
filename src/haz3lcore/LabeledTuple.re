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

// returns ordered list of (Some(string), TupLabel)
// and another of (None, not-TupLabel)
// TODO: Actually validate uniqueness in statics
// TODO: Make more efficient
// let validate_uniqueness:
//   'a.
//   ('a => option((t, 'a)), list('a)) =>
//   (bool, list((option(t), 'a)), list('a))
//  =
//   (get_label, es) => {
//     let results =
//       List.fold_left(
//         ((b, ls, ns), e) =>
//           switch (get_label(e)) {
//           | Some((s1, _))
//               when
//                 b
//                 && List.fold_left(
//                      (v, l) =>
//                        switch (l) {
//                        | (Some(s2), _) when v => compare(s1, s2) != 0
//                        | _ => false
//                        },
//                      true,
//                      ls,
//                    ) => (
//               b,
//               ls @ [(Some(s1), e)],
//               ns,
//             )
//           | None => (b, ls, ns @ [e])
//           | _ => (false, ls, ns)
//           },
//         (true, [], []),
//         es,
//       );
//     results;
//   };

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

// Assumes all labels are unique
// Rearranges all the labels in l2 to match the order of the labels in l1. Labels are optional and should me reordered for all present labels first and then unlabled fields matched up pairwise. So labeled fields can be reordered and unlabeled ones can't. Also add labels to the unlabeled.
// TODO Handle the unequal length case and extra labels case
let rec rearrange2:
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
        @ rearrange2(
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
          @ rearrange2(~show_b?, remaining_expectations, pre @ post)
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
        @ rearrange2(~show_b?, remaining_expectations, pre @ post)
      | None => remaining
      };
    };
  };

// Basically another way to call rearrange2 using the raw lists, functions to extract labels from TupLabels, and constructor for new TupLabels.
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
    let l2_reordered = rearrange2(l1', l2');
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
