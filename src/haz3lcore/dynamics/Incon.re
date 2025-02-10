open Sets;
open Util;
open Util.Maps;

module Ctr = {
  module M = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      ctr: Constructor.t,
      num_args: int,
      string_key: string,
    };
    let compare =
        (
          {ctr: _, num_args: _, string_key: string_key1},
          {ctr: _, num_args: _, string_key: string_key2},
        ) =>
      compare(string_key1, string_key2);
  };
  include M;

  let mk = (ctr, num_args) => {
    let string_key = string_of_int(num_args) ++ "~" ++ ctr;
    {ctr, num_args, string_key};
  };

  module Map = MapUtil.Make(M);

  module Set = Set.Make(M);

  // we treat tuples like constructors for some purposes.
  // this should not be anotherwise valid constructor name.
  let of_int = n => mk(string_of_int(n), 0);
  let of_float = x => mk(string_of_float(x), 0);
  let of_string = s => mk("\"" ++ s, 0); // don't need closing quote, just need to distinguish from others
  let tuple_ctr: int => t = n => mk("tuple", n);
  let nil_ctr = mk("nil", 0);
  let cons_ctr = mk("cons", 1);
  let true_ctr = mk("true", 0);
  let false_ctr = mk("false", 0);

  // used when not all constructors have been seen to handle the unseen cases
  let default_ctr = mk("_", 0);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type arity = list(Typ.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type all_ctrs =
    | Unknown
    | Infinite
    | Finite(Map.t(arity));

  let num_args_of = (ctr: t): int => ctr.num_args;

  let arity_of = (ctr, all_ctrs: all_ctrs): arity =>
    switch (all_ctrs) {
    | Unknown =>
      List.init(ctr.num_args, _ => TermBase.Unknown(Internal) |> Typ.temp)
    | Infinite =>
      List.init(ctr.num_args, _ => TermBase.Unknown(Internal) |> Typ.temp)
    | Finite(all_ctrs) =>
      switch (Map.find_opt(ctr, all_ctrs)) {
      | Some(arity) => arity
      | None =>
        List.init(ctr.num_args, _ => TermBase.Unknown(Internal) |> Typ.temp)
      }
    };

  let all_ctrs_of_typ = (ty: Typ.t): all_ctrs =>
    switch (ty.term) {
    | Sum(map)
    | Rec(_, {term: Sum(map), _}) =>
      Finite(
        map
        |> List.filter_map(
             fun
             | ConstructorMap.Variant(ctr, _, None) =>
               Some((mk(ctr, 0), []))
             | Variant(ctr, _, Some(arg_ty)) =>
               Some((mk(ctr, 1), [arg_ty]))
             | BadEntry(_) => None,
           )
        |> Map.of_list,
      )
    | Prod(elts) =>
      Finite(Map.singleton(tuple_ctr(List.length(elts)), elts))
    | List(_) => Finite(Map.of_list([(nil_ctr, []), (cons_ctr, [ty])]))
    | Bool => Finite(Map.of_list([(true_ctr, []), (false_ctr, [])]))
    | Unknown(_) => Unknown
    | Int
    | Float
    | String
    | Arrow(_)
    | Forall(_) => Infinite
    | Var(_)
    | Parens(_)
    | Ap(_)
    | Rec(_) =>
      failwith("all_ctrs_of_type called with a non-normalized type.")
    };

  let seen_all_ctrs = (seen_ctrs, all_ctrs: all_ctrs) => {
    switch (all_ctrs) {
    | Unknown => false
    | Infinite => false
    | Finite(all_ctrs) =>
      List.split(Map.bindings(all_ctrs))
      |> fst
      |> List.for_all(ctr => Set.mem(ctr, seen_ctrs))
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
    seen_hole: bool,
    first_col_redundant_rows: redundant_rows,
  };

  let init_seen = {
    seen_ints: IntSet.empty,
    seen_floats: FloatSet.empty,
    seen_strings: StringSet.empty,
    seen_ctrs: Ctr.Set.empty,
    seen_all_ctrs: false,
    seen_truth: false,
    seen_hole: false,
    first_col_redundant_rows: [],
  };

  // data accumulation pass over the first column of the matrix
  let seen = (m: Matrix.t, all_ctrs: Ctr.all_ctrs): seen => {
    List.fold_left(
      (seen, row: Matrix.row) =>
        switch (row.cols) {
        | [] => seen
        | [Int(n), ..._] =>
          let first_col_redundant_rows =
            if (IntSet.mem(n, seen.seen_ints) || seen.seen_truth) {
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
            if (FloatSet.mem(x, seen.seen_floats) || seen.seen_truth) {
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
            if (StringSet.mem(s, seen.seen_strings) || seen.seen_truth) {
              [row.idx, ...seen.first_col_redundant_rows];
            } else {
              seen.first_col_redundant_rows;
            };
          {
            ...seen,
            seen_strings: seen.seen_strings |> StringSet.add(s),
            first_col_redundant_rows,
          };
        | [Tuple(elts), ..._] =>
          let ctr = Ctr.tuple_ctr(List.length(elts));
          let first_col_redundant_rows =
            if (Ctr.Set.mem(ctr, seen.seen_ctrs) || seen.seen_truth) {
              [row.idx, ...seen.first_col_redundant_rows];
            } else {
              seen.first_col_redundant_rows;
            };
          let seen_ctrs = Ctr.Set.add(ctr, seen.seen_ctrs);
          let seen_all_ctrs =
            seen.seen_all_ctrs || Ctr.seen_all_ctrs(seen_ctrs, all_ctrs);
          {...seen, seen_ctrs, seen_all_ctrs, first_col_redundant_rows};
        | [Ap(c, arg), ..._] =>
          let ctr =
            Ctr.mk(
              c,
              switch (arg) {
              | Some(_) => 1
              | None => 0
              },
            );
          let first_col_redundant_rows =
            if (Ctr.Set.mem(ctr, seen.seen_ctrs) || seen.seen_truth) {
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
        | [Hole, ..._] => {...seen, seen_hole: true}
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
        ctr: Ctr.t,
        idx: int,
        cols: list(Constraint.t),
        ctrs: Ctr.Map.t(Matrix.t),
      )
      : Ctr.Map.t(Matrix.t) =>
    Ctr.Map.update(ctr, add_row(idx, cols), ctrs);

  let of_matrix = (m: Matrix.t, all_ctrs: Ctr.all_ctrs): t => {
    let {
      seen_ints: _,
      seen_floats: _,
      seen_strings: _,
      seen_ctrs,
      seen_all_ctrs,
      seen_truth,
      seen_hole,
      first_col_redundant_rows,
    } =
      seen(m, all_ctrs);

    let include_default =
      switch (all_ctrs) {
      | Unknown => false
      | Infinite => true
      | Finite(_) => !seen_all_ctrs
      };

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
                update_ctrs(
                  Ctr.tuple_ctr(List.length(xis)),
                  row.idx,
                  cols',
                  submatrices.ctrs,
                ),
            };
          | [Ap(c, arg), ...cols] =>
            let (ctr, cols') =
              switch (arg) {
              | Some(arg) => (Ctr.mk(c, 1), [arg, ...cols])
              | None => (Ctr.mk(c, 0), cols)
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
                  let num_args = Ctr.num_args_of(ctr);
                  let cols = List.init(num_args, _ => Constraint.Truth) @ cols;
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

    let first_col_exhaustive =
      switch (all_ctrs) {
      | Unknown => true
      | Infinite => seen_truth || seen_hole
      | Finite(_) => seen_truth || seen_hole || seen_all_ctrs
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
