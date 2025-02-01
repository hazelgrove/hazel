open Sets;
open Util;

let is_inconsistent_int = (xis: list(Constraint.t)): bool => {
  let (int_set, not_int_list) =
    List.fold_left(
      ((int_set, not_int_list), xi: Constraint.t) =>
        switch (xi) {
        | Int(n) => (IntSet.add(n, int_set), not_int_list)
        | NotInt(n) => (int_set, [n, ...not_int_list])
        | _ => failwith("input can only be Int | NotInt")
        },
      (IntSet.empty, []),
      xis,
    );
  IntSet.cardinal(int_set) > 1
  || List.exists(IntSet.mem(_, int_set), not_int_list);
};

let is_inconsistent_float = (xis: list(Constraint.t)): bool => {
  let (float_set, not_float_list) =
    List.fold_left(
      ((float_set, not_float_list), xi: Constraint.t) =>
        switch (xi) {
        | Float(n) => (FloatSet.add(n, float_set), not_float_list)
        | NotFloat(n) => (float_set, [n, ...not_float_list])
        | _ => failwith("input can only be Float | NotFloat")
        },
      (FloatSet.empty, []),
      xis,
    );
  FloatSet.cardinal(float_set) > 1
  || List.exists(FloatSet.mem(_, float_set), not_float_list);
};

let is_inconsistent_string = (xis: list(Constraint.t)): bool => {
  let (string_set, not_string_list) =
    List.fold_left(
      ((string_set, not_string_list), xi: Constraint.t) =>
        switch (xi) {
        | String(s) => (StringSet.add(s, string_set), not_string_list)
        | NotString(s) => (string_set, [s, ...not_string_list])
        | _ => failwith("input can only be String | NotString")
        },
      (StringSet.empty, []),
      xis,
    );
  StringSet.cardinal(string_set) > 1
  || List.exists(StringSet.mem(_, string_set), not_string_list);
};

/*
 exhaustiveness algorithm:

 if there is a wildcard anywhere in the list, return true
 if there is a pair anywhere in the list, split the list into two and recurse on each
 if there is one but not both of injR and injLR, return false
 if there are both injL and injR, then gather up their arguments and recurse

 */

[@deriving (show({with_path: false}), sexp, yojson)]
type matrix = list(list(Constraint.t)); // Row-major order

[@deriving (show({with_path: false}), sexp, yojson)]
type submatrices = {
  prod: matrix,
  injL: matrix,
  injR: matrix,
  unit: matrix,
  first_col_exhaustive: bool,
};

let empty_submatrices = {
  prod: [],
  injL: [],
  injR: [],
  unit: [],
  first_col_exhaustive: false,
};

type seen = {
  seen_prod: bool,
  seen_injL: bool,
  seen_injR: bool,
  seen_truth: bool,
};

let init_seen = {
  seen_prod: false,
  seen_injL: false,
  seen_injR: false,
  seen_truth: false,
};

let seen = (m: matrix): seen => {
  List.fold_left(
    (seen, row: list(Constraint.t)) =>
      switch (row) {
      | [Pair(_, _), ..._] => {...seen, seen_prod: true}
      | [InjL(_), ..._] => {...seen, seen_injL: true}
      | [InjR(_), ..._] => {...seen, seen_injR: true}
      | [Truth, ..._] => {...seen, seen_truth: true}
      | _ => seen
      },
    init_seen,
    m,
  );
};

let submatrices = (m: matrix): submatrices => {
  // TODO: compute seen's in a first pass, the below is incorrect
  // TODO: report errors with typechecking error reporting on constructors
  // TODO: not handling first column is Truth correctly -- second column never gets processed
  // (need to compute a remnant matrix if its all Truths)
  let {seen_prod, seen_injL, seen_injR, seen_truth} = seen(m);
  let include_unit = !seen_prod && !seen_injL && !seen_injR && seen_truth;
  let submatrices =
    List.fold_left(
      (submatrices, row: list(Constraint.t)) => {
        switch (row) {
        | [Pair(xi1, xi2), ...cols] => {
            ...submatrices,
            prod: [[xi1, xi2, ...cols], ...submatrices.prod],
          }
        | [InjL(xi), ...cols] => {
            ...submatrices,
            injL: [[xi, ...cols], ...submatrices.injL],
          }
        | [InjR(xi), ...cols] => {
            ...submatrices,
            injR: [[xi, ...cols], ...submatrices.injR],
          }
        | [Truth, ...cols] => {
            ...submatrices,
            prod:
              seen_prod
                ? [[Truth, Truth, ...cols], ...submatrices.prod]
                : submatrices.prod,
            injL:
              seen_injL
                ? [[Truth, ...cols], ...submatrices.injL] : submatrices.injL,
            injR:
              seen_injR
                ? [[Truth, ...cols], ...submatrices.injR] : submatrices.injR,
            unit:
              include_unit ? [cols, ...submatrices.unit] : submatrices.unit,
          }
        | _ => submatrices // TODO: other cases
        }
      },
      empty_submatrices,
      m,
    );
  let first_col_exhaustive =
    switch (seen_truth, seen_injR, seen_injL) {
    | (true, _, _) => true
    | (false, true, true) => true
    | (false, false, false) => true
    | (false, true, false)
    | (false, false, true) => false
    };
  {...submatrices, first_col_exhaustive};
};

let matrix_of_constraints = (xis: list(Constraint.t)) => {
  List.map(xi => [xi], xis);
};

let rec check_matrix = (m: matrix): bool => {
  // if it is a single column unit matrix, check that there is at least one truth row
  // else if it is of sum type, check that the first column contains both L and R
  // then compute the submatrices for L and R and recurse on each of them
  // else if it is of product type, compute the submatrix and recurse on it
  // else if it is of integer/float/string type, ... TODO ...
  print_endline(show_matrix(m));
  switch (m) {
  | [] => true // empty matrix, TODO: what about void types?
  | [[], ..._] => true // no columns in the matrix
  | _ =>
    let submatrices = submatrices(m);
    print_endline(show_submatrices(submatrices));
    if (!submatrices.first_col_exhaustive) {
      print_endline("First col not exhaustive");
      false;
    } else {
      check_matrix(submatrices.prod)
      && check_matrix(submatrices.injL)
      && check_matrix(submatrices.injR)
      && check_matrix(submatrices.unit);
    };
  };
};

let rec check = (xis: list(Constraint.t)): bool => {
  // convert to a matrix and call check_matrix
  check_matrix(
    matrix_of_constraints(xis),
  );
};

let rec is_exhaustive = (xi: Constraint.t): bool => {
  print_endline(Constraint.show(xi));
  switch (xi) {
  | Truth
  | Hole
  | Int(_)
  | Float(_)
  | String(_)
  | InjL(_)
  | InjR(_)
  | Pair(_, _)
  | Or(_) => is_exhaustive'(Constraint.truify(xi))
  | Falsity
  | NotInt(_)
  | NotFloat(_)
  | NotString(_)
  | And(_) =>
    print_endline(Constraint.show(xi));
    failwith("Invalid top-level constraint.");
  };
}
and is_exhaustive' = (xi: Constraint.t): bool => {
  switch (xi) {
  | Truth
  | Hole
  | Int(_)
  | Float(_)
  | String(_)
  | InjL(_)
  | InjR(_)
  | Pair(_, _) => is_exhaustive''([xi])
  | Or(xis) => is_exhaustive''(xis)
  | Falsity
  | NotInt(_)
  | NotFloat(_)
  | NotString(_)
  | And(_) =>
    print_endline(Constraint.show(xi));
    failwith("Invalid top-level constraint.'");
  };
}
and is_exhaustive'' = (xis: list(Constraint.t)): bool => {
  check(xis);
};

let is_redundant = (xi: Constraint.t, xis: Constraint.t): bool => {
  false;
       // TODO: implement me
};
