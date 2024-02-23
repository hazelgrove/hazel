open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = string;

let eq = String.equal;

let length = String.length;

let valid_regex =
  Re.Str.regexp("^\\([a-zA-Z]\\|_[_a-zA-Z0-9]\\)[_a-zA-Z0-9']*$");
let is_valid = s => Re.Str.string_match(valid_regex, s, 0);

let compare = String.compare;

let find_opt: ('a => bool, list('a)) => option('a) = List.find_opt;

// filt returns Some(string) if TupLabel or None if not a TupLabel
// returns ordered list of (Some(string), TupLabel)
// and another of (None, not-TupLabel)
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
              ls @ [(filt(e), e)],
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
    'b => option(string),
    'c => option(string),
    ('a, 'b, 'c) => 'a,
    'a,
    'a,
    list('b),
    list('c)
  ) =>
  'a =
  (filt1, filt2, f, accu, accu_fail, l1, l2) => {
    let (l1_valid, l1_lab, l1_none) = validate_uniqueness(filt1, l1);
    let (l2_valid, l2_lab, l2_none) = validate_uniqueness(filt2, l2);
    // temporary solution if mess up earlier in tuple, such as make_term
    if (!l1_valid || !l2_valid) {
      accu_fail;
    } else if (List.length(l1_none) != List.length(l2_none)) {
      accu_fail;
    } else {
      let accu =
        List.fold_left(
          (accu, l2_item) => {
            let l1_item =
              List.find_opt(
                l1_item => {
                  switch (l1_item, l2_item) {
                  | ((Some(s1), _), (Some(s2), _)) => compare(s1, s2) == 0
                  | (_, _) => false
                  }
                },
                l1_lab,
              );
            switch (l1_item, l2_item) {
            | (Some((_, l1_val)), (_, l2_val)) => f(accu, l1_val, l2_val)
            | (None, _) => accu_fail
            };
          },
          accu,
          l2_lab,
        );
      // TODO: Currently duplicating checks, for both directions
      let accu =
        List.fold_left(
          (accu, l1_item) => {
            let l2_item =
              List.find_opt(
                l2_item => {
                  switch (l1_item, l2_item) {
                  | ((Some(s1), _), (Some(s2), _)) => compare(s1, s2) == 0
                  | (_, _) => false
                  }
                },
                l2_lab,
              );
            switch (l1_item, l2_item) {
            | ((_, l1_val), Some((_, l2_val))) => f(accu, l1_val, l2_val)
            | (_, None) => accu_fail
            };
          },
          accu,
          l1_lab,
        );
      // None checks
      let accu =
        List.fold_left2(
          (accu, l1_val, l2_val) => f(accu, l1_val, l2_val),
          accu,
          l1_none,
          l2_none,
        );
      accu;
    };
  };
