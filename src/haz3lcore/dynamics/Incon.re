open Sets;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type row = {
  idx: int,
  cols: list(Constraint.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type matrix = list(row);

let contains_row = (idx: int, m: matrix): bool =>
  List.exists((row: row) => row.idx == idx, m);

let has_multiple_columns = (m: matrix): bool =>
  switch (m) {
  | [] => false
  | [{idx: _, cols: []}, ..._] => false
  | [{idx: _, cols: [_]}, ..._] => false
  | [{idx: _, cols: _}, ..._] => true
  };

let rev_matrix = (m: matrix): matrix => List.rev(m);

[@deriving (show({with_path: false}), sexp, yojson)]
type redundant_rows = list(int);

[@deriving (show({with_path: false}), sexp, yojson)]
type submatrices = {
  prod: matrix,
  injL: matrix,
  injR: matrix,
  unit: matrix,
  first_col_exhaustive: bool,
  first_col_redundant_rows: redundant_rows,
};

let rev_submatrices = (s: submatrices): submatrices => {
  ...s,
  prod: rev_matrix(s.prod),
  injL: rev_matrix(s.injL),
  injR: rev_matrix(s.injR),
  unit: rev_matrix(s.unit),
};

let empty_submatrices = {
  prod: [],
  injL: [],
  injR: [],
  unit: [],
  first_col_exhaustive: false,
  first_col_redundant_rows: [],
};

type seen = {
  seen_ints: IntSet.t,
  seen_floats: FloatSet.t,
  seen_strings: StringSet.t,
  seen_prod: bool,
  seen_injL: bool,
  seen_injR: bool,
  seen_truth: bool,
  first_col_redundant_rows: redundant_rows,
};

let init_seen = {
  seen_ints: IntSet.empty,
  seen_floats: FloatSet.empty,
  seen_strings: StringSet.empty,
  seen_prod: false,
  seen_injL: false,
  seen_injR: false,
  seen_truth: false,
  first_col_redundant_rows: [],
};

// data accumulation pass over the first column of the matrix
let seen = (m: matrix): seen => {
  List.fold_left(
    (seen, row: row) =>
      switch (row.cols) {
      | [Int(n), ..._] =>
        let first_col_redundant_rows =
          if (IntSet.mem(n, seen.seen_ints)) {
            [row.idx, ...seen.first_col_redundant_rows];
          } else {
            seen.first_col_redundant_rows;
          };
        {
          ...seen,
          seen_ints: seen.seen_ints |> IntSet.add(n),
          first_col_redundant_rows,
        };
      | [Float(x), ..._] =>
        let first_col_redundant_rows =
          if (FloatSet.mem(x, seen.seen_floats)) {
            [row.idx, ...seen.first_col_redundant_rows];
          } else {
            seen.first_col_redundant_rows;
          };
        {
          ...seen,
          seen_floats: seen.seen_floats |> FloatSet.add(x),
          first_col_redundant_rows,
        };
      | [String(s), ..._] =>
        let first_col_redundant_rows =
          if (StringSet.mem(s, seen.seen_strings)) {
            [row.idx, ...seen.first_col_redundant_rows];
          } else {
            seen.first_col_redundant_rows;
          };
        {
          ...seen,
          seen_strings: seen.seen_strings |> StringSet.add(s),
          first_col_redundant_rows,
        };
      | [Pair(_, _), ..._] =>
        let first_col_redundant_rows =
          if (seen.seen_prod) {
            [row.idx, ...seen.first_col_redundant_rows];
          } else {
            seen.first_col_redundant_rows;
          };
        {...seen, seen_prod: true, first_col_redundant_rows};
      | [InjL(_), ..._] =>
        let first_col_redundant_rows =
          if (seen.seen_injL) {
            [row.idx, ...seen.first_col_redundant_rows];
          } else {
            seen.first_col_redundant_rows;
          };
        {...seen, seen_injL: true, first_col_redundant_rows};
      | [InjR(_), ..._] =>
        let first_col_redundant_rows =
          if (seen.seen_injR) {
            [row.idx, ...seen.first_col_redundant_rows];
          } else {
            seen.first_col_redundant_rows;
          };
        {...seen, seen_injR: true, first_col_redundant_rows};
      | [Truth, ..._] =>
        let first_col_redundant_rows =
          switch (
            seen.seen_truth,
            seen.seen_prod,
            seen.seen_injL,
            seen.seen_injR,
          ) {
          | (true, _, _, _) => [row.idx, ...seen.first_col_redundant_rows]
          | (_, true, _, _) => [row.idx, ...seen.first_col_redundant_rows]
          | (_, _, true, true) => [row.idx, ...seen.first_col_redundant_rows]
          | (_, _, _, _) => seen.first_col_redundant_rows
          };
        {...seen, seen_truth: true, first_col_redundant_rows};
      | _ => seen // TODO: remove _
      },
    init_seen,
    m,
  );
};

let submatrices = (m: matrix): submatrices => {
  let {
    seen_ints,
    seen_floats,
    seen_strings,
    seen_prod,
    seen_injL,
    seen_injR,
    seen_truth,
    first_col_redundant_rows,
  } =
    seen(m);
  print_endline(
    "Seen: "
    ++ string_of_bool(seen_injL)
    ++ ", "
    ++ string_of_bool(seen_injR),
  );
  let include_unit =
    !seen_prod
    && !seen_injL
    && !seen_injR
    && seen_truth
    && has_multiple_columns(m);
  let submatrices =
    List.fold_left(
      (submatrices, row: row) => {
        switch (row.cols) {
        | [Pair(xi1, xi2), ...cols] => {
            ...submatrices,
            prod: [
              {idx: row.idx, cols: [xi1, xi2, ...cols]},
              ...submatrices.prod,
            ],
          }
        | [InjL(xi), ...cols] => {
            ...submatrices,
            injL: [
              {idx: row.idx, cols: [xi, ...cols]},
              ...submatrices.injL,
            ],
          }
        | [InjR(xi), ...cols] => {
            ...submatrices,
            injR: [
              {idx: row.idx, cols: [xi, ...cols]},
              ...submatrices.injR,
            ],
          }
        | [Truth, ...cols] => {
            ...submatrices,
            prod:
              seen_prod
                ? [
                  {idx: row.idx, cols: [Truth, Truth, ...cols]},
                  ...submatrices.prod,
                ]
                : submatrices.prod,
            injL:
              seen_injL
                ? [
                  {idx: row.idx, cols: [Truth, ...cols]},
                  ...submatrices.injL,
                ]
                : submatrices.injL,
            injR:
              seen_injR
                ? [
                  {idx: row.idx, cols: [Truth, ...cols]},
                  ...submatrices.injR,
                ]
                : submatrices.injR,
            unit:
              include_unit
                ? [{idx: row.idx, cols}, ...submatrices.unit]
                : submatrices.unit,
          }
        | _ => submatrices // TODO: other cases
        }
      },
      empty_submatrices,
      m,
    );
  let seen_int = !IntSet.is_empty(seen_ints);
  let seen_float = !FloatSet.is_empty(seen_floats);
  let seen_string = !StringSet.is_empty(seen_strings);
  let first_col_exhaustive =
    switch (
      seen_int,
      seen_float,
      seen_string,
      seen_truth,
      seen_injR,
      seen_injL,
    ) {
    | (_, _, _, true, _, _) => true
    | (true, _, _, false, _, _) => false
    | (_, true, _, false, _, _) => false
    | (_, _, true, false, _, _) => false
    | (_, _, _, _, true, true) => true
    | (_, _, _, _, false, false) => true
    | (_, _, _, _, true, false)
    | (_, _, _, _, false, true) => false
    };
  print_endline(
    "First col exhaustive: " ++ string_of_bool(first_col_exhaustive),
  );
  print_endline(
    "First col redundant rows: "
    ++ show_redundant_rows(first_col_redundant_rows),
  );
  // needed so that rows show up in order for redundancy checking
  let submatrices = rev_submatrices(submatrices);
  {...submatrices, first_col_exhaustive, first_col_redundant_rows};
};

let matrix_of_constraints = (xis: list(Constraint.t)): matrix => {
  List.mapi((idx, xi) => {idx, cols: [xi]}, xis);
};

[@deriving (show({with_path: false}), sexp, yojson)]
type check_result = {
  is_exhaustive: bool,
  redundant_rows: list(int),
};

let exhaustive_and_irredundant = {is_exhaustive: true, redundant_rows: []};

let rec check_matrix = (m: matrix): check_result => {
  print_endline(show_matrix(m));
  switch (m) {
  | [] => exhaustive_and_irredundant // empty matrix, TODO: what about void types?
  | [{idx: _, cols: []}, ..._] => exhaustive_and_irredundant // no columns in the matrix
  | _ =>
    let submatrices = submatrices(m);
    print_endline(show_submatrices(submatrices));
    let checked_prod = check_matrix(submatrices.prod);
    let checked_injL = check_matrix(submatrices.injL);
    let checked_injR = check_matrix(submatrices.injR);
    let checked_unit = check_matrix(submatrices.unit);
    let is_exhaustive =
      submatrices.first_col_exhaustive
      && checked_prod.is_exhaustive
      && checked_injL.is_exhaustive
      && checked_injR.is_exhaustive
      && checked_unit.is_exhaustive;

    /* a row is redundant if its first column is redundant and
       it is a redundant row in any submatrix in which it appears */
    let redundant_rows =
      List.filter(
        (idx: int) => {
          let p =
            !contains_row(idx, submatrices.prod)
            || List.mem(idx, checked_prod.redundant_rows);
          let iL =
            !contains_row(idx, submatrices.injL)
            || List.mem(idx, checked_injL.redundant_rows);
          let iR =
            !contains_row(idx, submatrices.injR)
            || List.mem(idx, checked_injR.redundant_rows);
          let u =
            !contains_row(idx, submatrices.unit)
            || List.mem(idx, checked_unit.redundant_rows); // todo do we need this?
          p && iL && iR && u;
        },
        submatrices.first_col_redundant_rows,
      );
    print_endline("Redundant rows: " ++ show_redundant_rows(redundant_rows));
    {is_exhaustive, redundant_rows};
  };
};

let check = (xis: list(Constraint.t)): check_result => {
  check_matrix(matrix_of_constraints(xis));
};

let is_exhaustive = (_xi: Constraint.t) => {
  true;
};
