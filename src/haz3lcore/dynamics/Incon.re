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
  let of_int = string_of_int;
  let of_float = string_of_float;
  let of_string = s => "\"" ++ s; // don't need closing quote, just need to distinguish from others
  let tuple_ctr: Constructor.t = "tuple";
  let nil_ctr: Constructor.t = "nil";
  let cons_ctr: Constructor.t = "cons";
  let true_ctr: Constructor.t = "true";
  let false_ctr: Constructor.t = "false";

  // used when not all constructors have been seen to handle the unseen cases
  let default_ctr: Constructor.t = "_";

  [@deriving (show({with_path: false}), sexp, yojson)]
  type arity = list(Typ.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type all_ctrs = Map.t(arity);

  let arity_of = (ctr, all_ctrs: option(all_ctrs)): arity =>
    switch (all_ctrs) {
    | None => []
    | Some(all_ctrs) =>
      switch (Map.find_opt(ctr, all_ctrs)) {
      | Some(arity) => arity
      | None => []
      }
    };

  let all_ctrs_of_typ = (ty: Typ.t): option(all_ctrs) =>
    switch (ty.term) {
    | Sum(map)
    | Rec(_, {term: Sum(map), _}) =>
      Some(
        map
        |> List.filter_map(
             fun
             | ConstructorMap.Variant(ctr, _, None) => Some((ctr, []))
             | Variant(ctr, _, Some(arg_ty)) => Some((ctr, [arg_ty]))
             | BadEntry(_) => None,
           )
        |> Map.of_list,
      )
    | Prod(elts) => Some(Map.singleton(tuple_ctr, elts))
    | List(_) => Some(Map.of_list([(nil_ctr, []), (cons_ctr, [ty])]))
    | Bool => Some(Map.of_list([(true_ctr, []), (false_ctr, [])]))
    | Unknown(_)
    | Int
    | Float
    | String
    | Var(_)
    | Arrow(_)
    | Parens(_)
    | Ap(_)
    | Rec(_)
    | Forall(_) => None
    };

  let seen_all_ctrs = (seen_ctrs, all_ctrs: option(all_ctrs)) => {
    switch (all_ctrs) {
    | Some(all_ctrs) =>
      List.split(Map.bindings(all_ctrs))
      |> fst
      |> List.for_all(ctr => Set.mem(ctr, seen_ctrs))
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
    ctrs: Ctr.Map.t(Matrix.t),
    first_col_exhaustive: bool,
    first_col_redundant_rows: redundant_rows,
  };

  let rev = (s: t): t => {...s, ctrs: Ctr.Map.map(Matrix.rev, s.ctrs)};

  let empty = {
    ctrs: Ctr.Map.empty,
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
  let seen = (m: Matrix.t, all_ctrs: option(Ctr.all_ctrs)): seen => {
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
        | [Hole, ..._] =>
          // holes act like truth for the purposes of exhaustiveness checking,
          // but are never redundant
          {...seen, seen_truth: true}
        },
      init_seen,
      m,
    );
  };

  let add_row =
      (idx: int, cols: list(Constraint.t), data: option(Matrix.t))
      : option(Matrix.t) =>
    switch (data) {
    | Some(matrix) => Some([{idx, cols}, ...matrix])
    | None => Some([{idx, cols}])
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

  let of_matrix = (m: Matrix.t, all_ctrs: option(Ctr.all_ctrs)): t => {
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
    let include_default = !seen_all_ctrs;
    let submatrices =
      List.fold_left(
        (submatrices, row: Matrix.row) => {
          switch (row.cols) {
          | [] => submatrices
          | [Int(n), ...cols] => {
              ...submatrices,
              ctrs:
                update_ctrs(Ctr.of_int(n), row.idx, cols, submatrices.ctrs),
            }
          | [Float(x), ...cols] => {
              ...submatrices,
              ctrs:
                update_ctrs(
                  Ctr.of_float(x),
                  row.idx,
                  cols,
                  submatrices.ctrs,
                ),
            }
          | [String(s), ...cols] => {
              ...submatrices,
              ctrs:
                update_ctrs(
                  Ctr.of_string(s),
                  row.idx,
                  cols,
                  submatrices.ctrs,
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
                  let arity_len = List.length(Ctr.arity_of(ctr, all_ctrs));
                  let cols =
                    List.init(arity_len, _ => Constraint.Truth) @ cols;
                  update_ctrs(ctr, row.idx, cols, ctrs);
                },
                seen_ctrs,
                submatrices.ctrs,
              );

            let ctrs =
              include_default
                ? update_ctrs(Ctr.default_ctr, row.idx, cols, ctrs) : ctrs;

            {...submatrices, ctrs};
          }
        },
        empty,
        m,
      );
    let submatrices = rev(submatrices); // needed so that rows show up in order for redundancy checking

    let seen_int = !IntSet.is_empty(seen_ints);
    let seen_float = !FloatSet.is_empty(seen_floats);
    let seen_string = !StringSet.is_empty(seen_strings);
    let first_col_exhaustive =
      switch (seen_truth, seen_int, seen_float, seen_string, seen_all_ctrs) {
      | (true, _, _, _, _) => true
      | (_, true, _, _, _) => false
      | (_, _, true, _, _) => false
      | (_, _, _, true, _) => false
      | (_, _, _, _, true) => true
      | (_, _, _, _, _) => false
      };
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
let rec check_matrix = (m: Matrix.t, col_tys: list(Typ.t)): result => {
  switch (col_tys) {
  | [] => failwith("Empty column types.")
  | [first_col_ty, ...rem_col_tys] =>
    if (Typ.is_void(first_col_ty)) {
      {
        is_exhaustive: true,
        redundant_rows: List.init(List.length(m), i => i),
      };
    } else {
      // print_endline("type: " ++ [%derive.show: Typ.t](first_col_ty));
      let all_ctrs = Ctr.all_ctrs_of_typ(first_col_ty);
      // print_endline(
      //   "all_ctrs: " ++ [%derive.show: option(Ctr.all_ctrs)](all_ctrs),
      // );
      let submatrices = Submatrices.of_matrix(m, all_ctrs);
      // for each submatrix, recursively check_matrix, computing the col_tys based
      // on the first_col_ty and the constructor name.
      print_endline(
        "submatrices: " ++ [%derive.show: Submatrices.t](submatrices),
      );
      let (is_exhaustive, redundant_rows) =
        Ctr.Map.fold(
          (ctr, submatrix, (is_exhaustive, redundant_rows)) => {
            let arity = Ctr.arity_of(ctr, all_ctrs);
            let col_tys = arity @ rem_col_tys;
            switch (col_tys) {
            | [] => (is_exhaustive, redundant_rows)
            | _ =>
              let submatrix_check_result = check_matrix(submatrix, col_tys);
              let is_exhaustive =
                is_exhaustive && submatrix_check_result.is_exhaustive;
              let redundant_rows =
                List.filter(
                  (idx: int) => {
                    !Matrix.contains_row(idx, submatrix)
                    || List.mem(idx, submatrix_check_result.redundant_rows)
                  },
                  redundant_rows,
                );
              (is_exhaustive, redundant_rows);
            };
          },
          submatrices.ctrs,
          (
            submatrices.first_col_exhaustive,
            submatrices.first_col_redundant_rows,
          ),
        );
      print_endline(
        "returning: "
        ++ [%derive.show: result]({is_exhaustive, redundant_rows}),
      );
      {is_exhaustive, redundant_rows};
    }
  };
};

let check = (xis: list(Constraint.t), ty: Typ.t): result => {
  check_matrix(Matrix.of_constraints(xis), [ty]);
};

let is_exhaustive = (_xi: Constraint.t) => {
  true /* TODO: delet*/;
};
