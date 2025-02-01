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

let rec partition_pairs =
        (xis: list(Constraint.t))
        : option((list(Constraint.t), list(Constraint.t))) =>
  switch (xis) {
  | [] => Some(([], []))
  | [Pair(xi1, xi2), ...xis'] =>
    switch (partition_pairs(xis')) {
    | Some((xisL, xisR)) => Some(([xi1, ...xisL], [xi2, ...xisR]))
    | None => None
    }
  | _ => None
  };

let all_pairs = (xis: list(Constraint.t)) => {
  let r =
    List.partition_map(
      xi =>
        switch (xi) {
        | Constraint.Pair(xi1, xi2) => Left((xi1, xi2))
        | xi => Right(xi)
        },
      xis,
    );
  switch (r) {
  | (xs, []) => Some(List.split(xs))
  | _ => None
  };
};

let all_or_pairs = (xis: list(Constraint.t)) => {
  let r =
    List.partition_map(
      xi =>
        switch (xi) {
        | Constraint.Or(xis') =>
          switch (all_pairs(xis')) {
          | Some(pairs) => Left(pairs)
          | None => Right(xi)
          }
        | xi => Right(xi)
        },
      xis,
    );
  switch (r) {
  | (xs, []) =>
    let (lefts, rights) = List.split(xs);
    Some((List.flatten(lefts), List.flatten(rights)));
  | _ => None
  };
};

let rec is_inconsistent = (xis: list(Constraint.t)): bool => {
  print_endline([%show: list(Constraint.t)](xis));
  // if every element of xis is an Or, then partition_pairs and recurse separately
  switch (all_or_pairs(xis)) {
  | Some((left, right)) =>
    print_endline("L R");
    print_endline([%show: list(Constraint.t)](left));
    print_endline([%show: list(Constraint.t)](right));
    print_endline([%show: bool](is_inconsistent'(left)));
    print_endline([%show: bool](is_inconsistent'(right)));
    is_inconsistent'(left) && is_inconsistent'(right);
  | None => is_inconsistent'(xis)
  };
}
and is_inconsistent' = (xis: list(Constraint.t)): bool =>
  switch (xis) {
  | [] => false
  | _
      when
        List.exists(Constraint.is_injL, xis)
        && List.exists(Constraint.is_injR, xis) =>
    true
  | [xi, ...xis'] =>
    switch (xi) {
    | Truth => is_inconsistent'(xis')
    | Falsity => true
    | Hole => assert(false) // Impossible
    | And(xis'') => is_inconsistent(xis' @ xis'')
    | Or(xis'') => List.for_all(xi => is_inconsistent([xi, ...xis']), xis'')
    | InjL(_) =>
      switch (List.partition(Constraint.is_injL, xis)) {
      | (injLs, []) =>
        injLs |> List.map(Constraint.unwrapL) |> is_inconsistent
      | (injLs, others) => is_inconsistent(others @ injLs)
      }
    | InjR(_) =>
      switch (List.partition(Constraint.is_injR, xis)) {
      | (injRs, []) =>
        injRs |> List.map(Constraint.unwrapR) |> is_inconsistent
      | (injRs, others) => is_inconsistent(others @ injRs)
      }
    | Int(_)
    | NotInt(_) =>
      switch (
        List.partition(
          fun
          | Constraint.Int(_)
          | NotInt(_) => true
          | _ => false,
          xis,
        )
      ) {
      | (ns, []) => is_inconsistent_int(ns)
      | (ns, others) => is_inconsistent(others @ ns)
      }
    | Float(_)
    | NotFloat(_) =>
      switch (
        List.partition(
          fun
          | Constraint.Float(_)
          | NotFloat(_) => true
          | _ => false,
          xis,
        )
      ) {
      | (fs, []) => is_inconsistent_float(fs)
      | (fs, others) => is_inconsistent(others @ fs)
      }
    | String(_)
    | NotString(_) =>
      switch (
        List.partition(
          fun
          | Constraint.String(_)
          | NotString(_) => true
          | _ => false,
          xis,
        )
      ) {
      | (ss, []) => is_inconsistent_string(ss)
      | (ss, others) => is_inconsistent(others @ ss)
      }
    | Pair(_, _) =>
      switch (
        List.partition(
          fun
          | Constraint.Pair(_) => true
          | _ => false,
          xis,
        )
      ) {
      | (pairs, []) =>
        let (xisL, xisR) =
          pairs |> List.map(Constraint.unwrap_pair) |> List.split;
        is_inconsistent(xisL) || is_inconsistent(xisR);
      | (pairs, others) => is_inconsistent(others @ pairs)
      }
    }
  };

let is_redundant = (xi_cur: Constraint.t, xi_pre: Constraint.t): bool =>
  false;
/*is_inconsistent(
    Constraint.[And(truify(xi_cur), dual(falsify(xi_pre)))],
  );*/

let is_exhaustive = (xi: Constraint.t): bool => {
  print_endline("is_exhaustive2:");
  print_endline(Constraint.show(xi));
  print_endline("done is_exhaustive");
  is_inconsistent(Constraint.[dual(truify(xi))]);
};

/*
 exhaustiveness algorithm:

 if there is a wildcard anywhere in the list, return true
 if there is a pair anywhere in the list, split the list into two and recurse on each
 if there is one but not both of injR and injLR, return false
 if there are both injL and injR, then gather up their arguments and recurse

 */

module Uncovered = {
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
                  ? [[Truth, ...cols], ...submatrices.injL]
                  : submatrices.injL,
              injR:
                seen_injR
                  ? [[Truth, ...cols], ...submatrices.injR]
                  : submatrices.injR,
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
  Uncovered.check(xis);
};

/*let data =
    List.fold_left(
      (data, xi: Constraint.t) => {
        switch (xi) {
        | Truth => {...data, contains_wildcard: true}
        | Falsity => data
        | Hole => data
        | Int(_) => data
        | NotInt(_) => data
        | Float(_) => data
        | NotFloat(_) => data
        | String(_) => data
        | NotString(_) => data
        | And(xis) => data
        | Or(xis) => data
        | InjL(xi) => {
            ...data,
            contains_injLs:
              switch (data.contains_injLs) {
              | None => Some([xi])
              | Some(xis') => Some([xi, ...xis'])
              },
          }
        | InjR(xi) => {
            ...data,
            contains_injRs:
              switch (data.contains_injRs) {
              | None => Some([xi])
              | Some(xis') => Some([xi, ...xis'])
              },
          }
        | Pair(xi1, xi2) => {
            ...data,
            contains_pairs:
              switch (data.contains_pairs) {
              | None => Some([(xi1, xi2)])
              | Some(pairs) => Some([(xi1, xi2), ...pairs])
              },
          }
        }
      },
      empty_data,
      xis,
    );
  print_endline(show_data(data));
  if (data.contains_wildcard) {
    true;
  } else {
    switch (data.contains_pairs, data.contains_injLs, data.contains_injRs) {
    | (Some(_), Some(_), Some(_))
    | (Some(_), Some(_), None)
    | (Some(_), None, Some(_)) => true // both pairs and injLs or injRs are present
    | (Some(pairs), None, None) =>
      let r =
        List.fold_left(
          ((ul, ur), (xil, xir)) => {
            (Uncovered.remove(ul, xil), Uncovered.remove(ur, xir))
          },
          Uncovered.init,
          pairs,
        );
      switch (r) {
      | (Falsity, Falsity) => true
      | _ => false
      };
    | (None, Some(_), None) => false
    | (None, None, Some(_)) => false
    | (None, Some(injLs), Some(injRs)) =>
      is_exhaustive''(injLs) && is_exhaustive''(injRs)
    | (None, None, None) => false
    };
  };*/
