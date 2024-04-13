open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = string;

let eq = String.equal;

let length = String.length;

let compare = String.compare;

let find_opt: ('a => bool, list('a)) => option('a) = List.find_opt;

// filt returns Some(string) if TupLabel or None if not a TupLabel
// returns ordered list of (Some(string), TupLabel)
// and another of (None, not-TupLabel)
// TODO: Need to check uniqueness earlier
// TODO: Make more efficient
let validate_uniqueness:
  ('a => option(t), list('a)) => (bool, list((option(t), 'a)), list('a)) =
  (filt, es) => {
    let results =
      List.fold_left(
        ((b, ls, ns), e) =>
          switch (filt(e)) {
          | Some(s1)
              when
                b
                && List.fold_left(
                     (v, l) =>
                       switch (l) {
                       | (Some(s2), _) when v => compare(s1, s2) != 0
                       | _ => false
                       },
                     true,
                     ls,
                   ) => (
              b,
              ls @ [(Some(s1), e)],
              ns,
            )
          | None => (b, ls, ns @ [e])
          | _ => (false, ls, ns)
          },
        (true, [], []),
        es,
      );
    results;
  };

// Rename and clean this
// Assumes all labels are unique
// filt returns Some(string) if TupLabel or None if not a TupLabel
// In order of operations:
// Checks all labeled pairs in l2 are in l1 and performs f on each pair
// Checks all labeled pairs in l1 are in l2 and performs f on each pair
// Checks remaining None pairs in order and performs f on each pair
let ana_tuple:
  (
    'b => (option(t), 'b),
    'c => (option(t), 'c),
    ('a, 'b, 'c) => 'a,
    'a,
    'a,
    list('b),
    list('c)
  ) =>
  'a =
  (filt1, filt2, f, accu, accu_fail, l1, l2) => {
    let filt1_lab = x => {
      let (s, _) = filt1(x);
      s;
    };
    let filt2_lab = x => {
      let (s, _) = filt2(x);
      s;
    };
    let (l1_valid, l1_lab, l1_none) = validate_uniqueness(filt1_lab, l1);
    let (l2_valid, l2_lab, _) = validate_uniqueness(filt2_lab, l2);
    // temporary solution if mess up earlier in tuple, such as make_term
    if (!l1_valid || !l2_valid) {
      accu_fail;
    } else {
      // this result represents to accu, and the matched l2 labels
      let (accu, l2_labels_matched) =
        List.fold_left(
          ((accu, l2_matched), l1_item) => {
            let l2_item =
              find_opt(
                l2_item => {
                  switch (l1_item, l2_item) {
                  | ((Some(s1), _), (Some(s2), _)) => compare(s1, s2) == 0
                  | (_, _) => false
                  }
                },
                l2_lab,
              );
            switch (l1_item, l2_item) {
            | ((_, l1_val), Some((l2_lab, l2_val))) => (
                f(accu, l1_val, l2_val),
                l2_matched @ [l2_lab],
              )
            | (_, None) => (accu_fail, l2_matched)
            };
          },
          (accu, []),
          l1_lab,
        );
      // short circuit on failure
      if (accu == accu_fail) {
        accu_fail;
      } else {
        // filter l2 to remove matched labels and remove labels
        // TODO: Can be optimized
        let l2_rem =
          List.fold_left(
            (l2_rem, item) => {
              let lab_opt = filt2(item);
              switch (lab_opt) {
              | (Some(s1), _)
                  when
                    List.exists(
                      l => {
                        switch (l) {
                        | Some(s2) => compare(s1, s2) == 0
                        | _ => false
                        }
                      },
                      l2_labels_matched,
                    ) => l2_rem
              | (Some(_), it)
              | (None, it) => l2_rem @ [it]
              };
            },
            [],
            l2,
          );
        // remaining checks are in order
        let accu =
          List.fold_left2(
            (accu, l1_val, l2_val) => f(accu, l1_val, l2_val),
            accu,
            l1_none,
            l2_rem,
          );
        accu;
      };
    };
  };

let find_label: ('a => option(t), list('a), t) => option('a) =
  (filt, es, label) => {
    find_opt(
      e => {
        switch (filt(e)) {
        | Some(s) => compare(s, label) == 0
        | None => false
        }
      },
      es,
    );
  };
