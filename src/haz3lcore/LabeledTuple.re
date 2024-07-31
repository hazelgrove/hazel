open Util;

exception Exception;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = string;

let eq = String.equal;

let length = String.length;

let compare = String.compare;

let find_opt: ('a => bool, list('a)) => option('a) = List.find_opt;

// returns a pair containing a list of option(t) and a list of 'a
let seperate_labels:
  ('a => option((t, 'a)), list('a)) => (list(option(t)), list('a)) =
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

// returns ordered list of (Some(string), TupLabel)
// and another of (None, not-TupLabel)
// TODO: Need to check uniqueness earlier
// TODO: Make more efficient
let validate_uniqueness:
  ('a => option((t, 'a)), list('a)) =>
  (bool, list((option(t), 'a)), list('a)) =
  (get_label, es) => {
    let results =
      List.fold_left(
        ((b, ls, ns), e) =>
          switch (get_label(e)) {
          | Some((s1, _))
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

// Assumes all labels are unique
// filt returns Some(string) if TupLabel or None if not a TupLabel
// returns a permutation of l2 that matches labels in l1
// other labels are in order, even if not matching.
let rearrange:
  (
    'a => option((t, 'a)),
    'b => option((t, 'b)),
    list('a),
    list('b),
    (t, 'b) => 'b
  ) =>
  list('b) =
  (get_label1, get_label2, l1, l2, constructor) => {
    // TODO: Error handling in case of bad arguments
    let (_, l1_lab, _) = validate_uniqueness(get_label1, l1);
    let (_, l2_lab, _) = validate_uniqueness(get_label2, l2);
    // Second item in the pair is the full tuplabel
    let l2_matched =
      List.fold_left(
        (l2_matched, l1_item) => {
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
          switch (l2_item) {
          | Some(l2_item) => l2_matched @ [l2_item]
          | None => l2_matched
          };
        },
        [],
        l1_lab,
      );
    // Second item in the pair is just the element half of the tuplabel
    let l2_rem =
      List.fold_left(
        (l2_rem, item) => {
          switch (get_label2(item)) {
          | Some((s1, _))
              when
                List.exists(
                  l => {
                    switch (l) {
                    | (Some(s2), _) => compare(s1, s2) == 0
                    | _ => false
                    }
                  },
                  l2_matched,
                ) => l2_rem
          | Some((s1, it)) => l2_rem @ [(Some(s1), it)]
          | _ => l2_rem @ [(None, item)]
          }
        },
        [],
        l2,
      );
    let rec rearrange_helper =
            (
              l1: list('x),
              l2_matched: list((option(t), 'y)),
              l2_rem: list((option(t), 'y)),
            )
            : list('y) =>
      switch (l1) {
      | [hd, ...tl] =>
        switch (get_label1(hd)) {
        | Some((s1, _)) =>
          switch (l2_matched) {
          | [] =>
            switch (l2_rem) {
            | [hd2, ...tl2] =>
              switch (hd2) {
              | (Some(s2), rem_val) =>
                [constructor(s2, rem_val)]
                @ rearrange_helper(tl, l2_matched, tl2)
              | (None, rem_val) =>
                [constructor(s1, rem_val)]
                @ rearrange_helper(tl, l2_matched, tl2)
              }
            | [] => raise(Exception)
            }
          | [hd2, ...tl2] =>
            switch (hd2) {
            | (Some(s2), l2_val) when compare(s1, s2) == 0 =>
              [l2_val] @ rearrange_helper(tl, tl2, l2_rem)
            | _ =>
              switch (l2_rem) {
              | [hd2, ...tl2] =>
                switch (hd2) {
                | (Some(s2), rem_val) =>
                  [constructor(s2, rem_val)]
                  @ rearrange_helper(tl, l2_matched, tl2)
                | (None, rem_val) =>
                  [constructor(s1, rem_val)]
                  @ rearrange_helper(tl, l2_matched, tl2)
                }
              | [] => raise(Exception)
              }
            }
          }
        | None =>
          switch (l2_rem) {
          | [(_, hd2), ...tl2] =>
            [hd2] @ rearrange_helper(tl, l2_matched, tl2)
          | [] => raise(Exception)
          }
        }
      | [] => []
      };
    rearrange_helper(l1, l2_matched, l2_rem);
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
    'b => option((t, 'b)),
    'c => option((t, 'c)),
    ('a, 'b, 'c) => 'a,
    'a,
    'a,
    list('b),
    list('c)
  ) =>
  'a =
  (get_label1, get_label2, f, accu, accu_fail, l1, l2) => {
    let (l1_valid, l1_lab, l1_none) = validate_uniqueness(get_label1, l1);
    let (l2_valid, l2_lab, _) = validate_uniqueness(get_label2, l2);
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
              switch (get_label2(item)) {
              | Some((s1, _))
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
              | _ => l2_rem @ [item]
              }
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

let find_label: ('a => option((t, 'a)), list('a), t) => option('a) =
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
