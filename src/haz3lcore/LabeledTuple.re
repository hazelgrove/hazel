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

// Rename and clean this
// Assumes all labels are unique
// In order of operations:
// Checks all labeled pairs in l2 are in l1 and performs f on each pair
// Checks all labeled pairs in l1 are in l2 and performs f on each pair
// Checks remaining None pairs in order and performs f on each pair
let ana_tuple:
  (
    ('a, 'c, 'd) => 'a,
    'a,
    'a,
    list((option('b), 'c)),
    list((option('b), 'd))
  ) =>
  'a =
  (f, accu, accu_fail, l1, l2) => {
    let l1_lab = List.filter(((p, _)) => p != None, l1);
    let l1_none = List.filter(((p, _)) => p == None, l1);
    let l2_lab = List.filter(((p, _)) => p != None, l2);
    let l2_none = List.filter(((p, _)) => p == None, l2);
    // temporary solution if mess up earlier in tuple, such as make_term
    if (List.length(l1_none) != List.length(l2_none)) {
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
                  | ((None, _), (None, _)) => true
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
                  | ((None, _), (None, _)) => true
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
          (accu, (_, l1_val), (_, l2_val)) => f(accu, l1_val, l2_val),
          accu,
          l1_none,
          l2_none,
        );
      accu;
    };
  };
