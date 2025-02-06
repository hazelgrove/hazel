open Sets;
open Util;
open Util.Maps;

module Ctr = {
  module Map =
    MapUtil.Make({
      [@deriving (show({with_path: false}), sexp, yojson)]
      type t = Constructor.t;
      let compare = compare;
    });

  module Set =
    Set.Make({
      type t = Constructor.t;
      let compare = compare;
    });

  // we treat tuples like constructors for some purposes.
  // this should not be anotherwise valid constructor name.
  let tuple_ctr: Constructor.t = "tuple";
  let nil_ctr: Constructor.t = "nil";
  let cons_ctr: Constructor.t = "cons";

  type arity = int;

  let all_ctrs_of_typ = (ty: Typ.t): option(Map.t(arity)) =>
    switch (ty.term) {
    | Sum(map)
    | Rec(_, {term: Sum(map), _}) =>
      Some(
        map
        |> List.filter_map(
             fun
             | ConstructorMap.Variant(ctr, _, _) => Some(ctr)
             | BadEntry(_) => None,
           )
        |> Set.of_list,
      )
    | Prod(_) => Some(Set.singleton(tuple_ctr))
    | List(_) => Some(Set.of_list([nil_ctr, cons_ctr]))
    | Unknown(_)
    | Int
    | Float
    | Bool
    | String
    | Var(_)
    | Arrow(_)
    | Parens(_)
    | Ap(_)
    | Rec(_)
    | Forall(_) => None
    };

  let seen_all_ctrs = (seen_ctrs, all_ctrs) => {
    switch (all_ctrs) {
    | Some(all_ctrs) => Set.equal(seen_ctrs, all_ctrs)
    | None => false
    };
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type redundant_rows = list(int);

module Matrix = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type row = {
    idx: int,
    cols: list(Constraint.t),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(row);

  let of_constraints = (xis: list(Constraint.t)): t => {
    List.mapi((idx, xi) => {idx, cols: [xi]}, xis);
  };

  let contains_row = (idx: int, m: t): bool =>
    List.exists((row: row) => row.idx == idx, m);

  let has_multiple_columns = (m: t): bool =>
    switch (m) {
    | [] => false
    | [{idx: _, cols: []}, ..._] => false
    | [{idx: _, cols: [_]}, ..._] => false
    | [{idx: _, cols: _}, ..._] => true
    };

  let rev = (m: t): t => List.rev(m);
};

module Submatrices = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    ints: IntMap.t(Matrix.t),
    floats: FloatMap.t(Matrix.t),
    strings: StringMap.t(Matrix.t),
    ctrs: Ctr.Map.t(Matrix.t),
    unit: option(Matrix.t),
    first_col_exhaustive: bool,
    first_col_redundant_rows: redundant_rows,
  };

  let update_ctrs =
      (
        ctr: Constructor.t,
        idx: int,
        cols: list(Constraint.t),
        ctrs: Ctr.Map.t(Matrix.t),
      )
      : Ctr.Map.t(Matrix.t) =>
    Ctr.Map.update(
      ctr,
      (data: option(Matrix.t)) => {
        switch (data) {
        | Some(matrix) => Some([{idx, cols}, ...matrix])
        | None => Some([{idx, cols}])
        }
      },
      ctrs,
    );

  let rev = (s: t): t => {
    ...s,
    ints: IntMap.map(Matrix.rev, s.ints),
    floats: FloatMap.map(Matrix.rev, s.floats),
    strings: StringMap.map(Matrix.rev, s.strings),
    ctrs: Ctr.Map.map(Matrix.rev, s.ctrs),
    unit: Option.map(Matrix.rev, s.unit),
  };

  let empty = {
    ints: IntMap.empty,
    floats: FloatMap.empty,
    strings: StringMap.empty,
    ctrs: Ctr.Map.empty,
    unit: None,
    first_col_exhaustive: false,
    first_col_redundant_rows: [],
  };

  type seen = {
    seen_ints: IntSet.t,
    seen_floats: FloatSet.t,
    seen_strings: StringSet.t,
    seen_ctrs: Ctr.Set.t,
    seen_all_ctrs: bool,
    seen_truth: bool,
    first_col_redundant_rows: redundant_rows,
  };

  let init_seen = {
    seen_ints: IntSet.empty,
    seen_floats: FloatSet.empty,
    seen_strings: StringSet.empty,
    seen_ctrs: Ctr.Set.empty,
    seen_all_ctrs: false,
    seen_truth: false,
    first_col_redundant_rows: [],
  };

  // data accumulation pass over the first column of the matrix
  let seen = (m: Matrix.t, all_ctrs: option(Ctr.Set.t)): seen => {
    List.fold_left(
      (seen, row: Matrix.row) =>
        switch (row.cols) {
        | [] => seen
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
        | [Tuple(_), ..._] =>
          let first_col_redundant_rows =
            if (Ctr.Set.mem(Ctr.tuple_ctr, seen.seen_ctrs)) {
              [row.idx, ...seen.first_col_redundant_rows];
            } else {
              seen.first_col_redundant_rows;
            };
          let seen_ctrs = Ctr.Set.add(Ctr.tuple_ctr, seen.seen_ctrs);
          let seen_all_ctrs =
            seen.seen_all_ctrs || Ctr.seen_all_ctrs(seen_ctrs, all_ctrs);
          {...seen, seen_ctrs, seen_all_ctrs, first_col_redundant_rows};
        | [Ap(ctr, _), ..._] =>
          let first_col_redundant_rows =
            if (Ctr.Set.mem(ctr, seen.seen_ctrs)) {
              [row.idx, ...seen.first_col_redundant_rows];
            } else {
              seen.first_col_redundant_rows;
            };
          let seen_ctrs = Ctr.Set.add(ctr, seen.seen_ctrs);
          let seen_all_ctrs =
            seen.seen_all_ctrs || Ctr.seen_all_ctrs(seen_ctrs, all_ctrs);
          {...seen, seen_ctrs, seen_all_ctrs, first_col_redundant_rows};
        | [Truth, ..._] =>
          let first_col_redundant_rows =
            seen.seen_truth || seen.seen_all_ctrs
              ? [row.idx, ...seen.first_col_redundant_rows]
              : seen.first_col_redundant_rows;
          {...seen, seen_truth: true, first_col_redundant_rows};
        | [Falsity, ..._] => seen
        | [Hole, ..._] =>
          // holes act like truth for the purposes of exhaustiveness checking,
          // but are never redundant
          {...seen, seen_truth: true}
        },
      init_seen,
      m,
    );
  };

  let of_matrix = (m: Matrix.t, all_ctrs: option(Ctr.Set.t)): t => {
    let {
      seen_ints,
      seen_floats,
      seen_strings,
      seen_ctrs,
      seen_all_ctrs,
      seen_truth,
      first_col_redundant_rows,
    } =
      seen(m, all_ctrs);
    let include_unit = seen_all_ctrs && Matrix.has_multiple_columns(m);
    let submatrices =
      List.fold_left(
        (submatrices, row: Matrix.row) => {
          switch (row.cols) {
          | [] => submatrices
          | [Int(n), ...cols] => {
              ...submatrices,
              ints:
                IntMap.update(
                  n,
                  (data: option(Matrix.t)) => {
                    switch (data) {
                    | Some(matrix) =>
                      Some([{idx: row.idx, cols}, ...matrix])
                    | None => Some([{idx: row.idx, cols}])
                    }
                  },
                  submatrices.ints,
                ),
            }
          | [Float(x), ...cols] => {
              ...submatrices,
              floats:
                FloatMap.update(
                  x,
                  (data: option(Matrix.t)) => {
                    switch (data) {
                    | Some(matrix) =>
                      Some([{idx: row.idx, cols}, ...matrix])
                    | None => Some([{idx: row.idx, cols}])
                    }
                  },
                  submatrices.floats,
                ),
            }
          | [String(s), ...cols] => {
              ...submatrices,
              strings:
                StringMap.update(
                  s,
                  (data: option(Matrix.t)) => {
                    switch (data) {
                    | Some(matrix) =>
                      Some([{idx: row.idx, cols}, ...matrix])
                    | None => Some([{idx: row.idx, cols}])
                    }
                  },
                  submatrices.strings,
                ),
            }
          | [Tuple(xis), ...cols] =>
            let cols' = xis @ cols;
            {
              ...submatrices,
              ctrs:
                update_ctrs(Ctr.tuple_ctr, row.idx, cols', submatrices.ctrs),
            };
          | [Ap(ctr, arg), ...cols] =>
            let cols' =
              switch (arg) {
              | Some(arg) => [arg, ...cols]
              | None => cols
              };
            {
              ...submatrices,
              ctrs: update_ctrs(ctr, row.idx, cols', submatrices.ctrs),
            };
          | [Truth | Hole, ...cols] =>
            // holes act like truth for the purposes of exhaustiveness checking
            // update all submatrices for seen ctrs
            let ctrs =
              Ctr.Set.fold(
                (ctr, ctrs) => {
                  let arity = Ctr.arity(ctr);
                  let cols = List.make(arity, Truth) @ cols;
                  update_ctrs(ctr, row.idx, cols, ctrs),
                seen_ctrs,
                submatrices.ctrs,
              );

            {
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
            };
          | [Falsity, ..._] => submatrices
          }
        },
        empty,
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
    let submatrices = rev(submatrices);
    {...submatrices, first_col_exhaustive, first_col_redundant_rows};
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type result = {
  is_exhaustive: bool,
  redundant_rows,
};

let exhaustive_and_irredundant = {is_exhaustive: true, redundant_rows: []};
let inexhaustive_and_irredundant = {is_exhaustive: false, redundant_rows: []};

// We assume ty is already normalized.
let rec check_matrix = (m: Matrix.t, ty: Typ.t): result => {
  print_endline(Matrix.show(m));
  switch (m) {
  | [] =>
    Typ.is_void(ty)
      ? exhaustive_and_irredundant : inexhaustive_and_irredundant
  | [{idx: _, cols: []}, ..._] => failwith("No columns in the matrix.")
  | _ =>
    let all_ctrs = all_ctrs_of_typ(ty);
    let submatrices = Submatrices.of_matrix(m, all_ctrs);
    let checked_ints = IntMap.map(check_matrix, submatrices.ints);
    let checked_floats = FloatMap.map(check_matrix, submatrices.floats);
    let checked_strings = StringMap.map(check_matrix, submatrices.strings);
    let checked_ctrs = Ctr.Map.map(check_matrix, submatrices.ctrs);
    let checked_unit = Option.map(check_matrix, submatrices.unit);
    let is_exhaustive =
      submatrices.first_col_exhaustive
      && checked_prod.is_exhaustive
      && checked_injL.is_exhaustive
      && checked_injR.is_exhaustive
      && checked_unit.is_exhaustive
      && IntMap.for_all((_, c) => c.is_exhaustive, checked_ints)
      && FloatMap.for_all((_, c) => c.is_exhaustive, checked_floats)
      && StringMap.for_all((_, c) => c.is_exhaustive, checked_strings);

    /* a row is redundant if its first column is redundant and
       it is a redundant row in any submatrix in which it appears */
    let redundant_rows =
      List.filter(
        (idx: int) => {
          let i =
            IntMap.for_all(
              (_, m) =>
                !Matrix.contains_row(idx, m)
                || IntMap.exists(
                     (_, c) => List.mem(idx, c.redundant_rows),
                     checked_ints,
                   ),
              submatrices.ints,
            );
          let f =
            FloatMap.for_all(
              (_, m) =>
                !Matrix.contains_row(idx, m)
                || FloatMap.exists(
                     (_, c) => List.mem(idx, c.redundant_rows),
                     checked_floats,
                   ),
              submatrices.floats,
            );
          let s =
            StringMap.for_all(
              (_, m) =>
                !Matrix.contains_row(idx, m)
                || StringMap.exists(
                     (_, c) => List.mem(idx, c.redundant_rows),
                     checked_strings,
                   ),
              submatrices.strings,
            );
          let p =
            !Matrix.contains_row(idx, submatrices.prod)
            || List.mem(idx, checked_prod.redundant_rows);
          let iL =
            !Matrix.contains_row(idx, submatrices.injL)
            || List.mem(idx, checked_injL.redundant_rows);
          let iR =
            !Matrix.contains_row(idx, submatrices.injR)
            || List.mem(idx, checked_injR.redundant_rows);
          let u =
            !Matrix.contains_row(idx, submatrices.unit)
            || List.mem(idx, checked_unit.redundant_rows); // todo do we need this?
          i && f && s && p && iL && iR && u;
        },
        submatrices.first_col_redundant_rows,
      );
    print_endline("Redundant rows: " ++ show_redundant_rows(redundant_rows));
    {is_exhaustive, redundant_rows};
  };
};

let check = (xis: list(Constraint.t), ty: Typ.t): result => {
  check_matrix(Matrix.of_constraints(xis), ty);
};

let is_exhaustive = (_xi: Constraint.t) => {
  true /* TODO: delet*/;
};
