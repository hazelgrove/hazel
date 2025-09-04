/* The pattern coverage checker implements a variant of Maranget's pattern matrix checker
   (see "Warnings for Pattern Matching", Maranget 2007).

   The main novelty here is that we handle holes following the developments in Yuan et al, OOPSLA 2023
   (the Peanut paper) + we handle unknowns in scrutinee types. */

open Util;
open Util.Sets;
open Util.Maps;

module Constraint = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Truth
    | Hole
    | BigInt(Bigint.t)
    | SInt(int)
    | Float(float)
    | String(string)
    | Ap(Constructor.t, option(t))
    | Tuple(list(t));

  let nil = Ap("nil", None);
  let cons = (hd, tl) => Ap("cons", Some(Tuple([hd, tl])));

  let true_ = Ap("true", None);
  let false_ = Ap("false", None);
};

module Ctr = {
  module M = {
    // Ctrs are Constructors equipped with arities.
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
  module Map = MapUtil.Make(M);
  module Set = Set.Make(M);

  let mk = (ctr, num_args) => {
    let string_key = string_of_int(num_args) ++ "~" ++ ctr;
    {
      ctr,
      num_args,
      string_key,
    };
  };
  let num_args_of = (ctr: t): int => ctr.num_args;

  // Ctrs for primitive types
  // Names here should not be anotherwise valid sum type constructor names.
  let of_int = n => mk(Bigint.to_string(n), 0);
  let of_sint = n => mk(string_of_int(n), 0);
  let of_float = x => mk(string_of_float(x), 0);
  let of_string = s => mk("\"" ++ s, 0); // don't need closing quote, just need to distinguish from others
  let tuple_ctr: int => t = n => mk("tuple", n);
  let nil_ctr = mk("nil", 0);
  let cons_ctr = mk("cons", 1);
  let true_ctr = mk("true", 0);
  let false_ctr = mk("false", 0);

  // used when not all constructors have been seen to handle the unseen cases when a wildcard appears
  let default_ctr = mk("_", 0);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type arity = list(Typ.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type all_ctrs =
    | Unknown
    | Infinite
    | Finite(Map.t(arity));

  let arity_of = (ctr, all_ctrs: all_ctrs): arity =>
    switch (all_ctrs) {
    | Unknown => List.init(ctr.num_args, _ => Unknown(Internal) |> Typ.temp)
    | Infinite => List.init(ctr.num_args, _ => Unknown(Internal) |> Typ.temp)
    | Finite(all_ctrs) =>
      switch (Map.find_opt(ctr, all_ctrs)) {
      | Some(arity) => arity
      | None => List.init(ctr.num_args, _ => Unknown(Internal) |> Typ.temp)
      }
    };

  let rec all_ctrs_of_typ = (~rec_count=0, ty: Typ.t): all_ctrs => {
    if (rec_count > 1000) {
      failwith("Recursion limit exceeded in all_ctrs_of_typ");
    };
    let all_ctrs_of_typ = all_ctrs_of_typ(~rec_count=rec_count + 1);
    switch (ty.term) {
    | Sum(map) =>
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
    | Rec({term: Var(w), _}, {term: Var(v), _}) when v == w => Unknown
    | Rec(_) => all_ctrs_of_typ(Typ.unroll(ty))
    | Prod(elts) =>
      Finite(Map.singleton(tuple_ctr(List.length(elts)), elts))
    | ProofOf(_) => Infinite
    | TupLabel(_, ty) => Finite(Map.singleton(tuple_ctr(1), [ty]))
    | List(elt_ty) =>
      Finite(
        Map.of_list([
          (nil_ctr, []),
          (cons_ctr, [Prod([elt_ty, ty]) |> Typ.temp]),
        ]),
      )
    | Atom(Bool) => Finite(Map.of_list([(true_ctr, []), (false_ctr, [])]))
    | Unknown(_) => Unknown
    | Atom(Int)
    | Atom(SInt) // technically sint and float are finite, but ya know
    | Atom(Float)
    | Atom(Nat)
    | Atom(String)
    | Arrow(_)
    | Poly(_)
    | Var(_) => Infinite
    | Parens(_)
    | Label(_) =>
      failwith(
        "all_ctrs_of_type called with a non-normalized type: " ++ Typ.show(ty),
      )
    };
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
    idx: int, // retaining row index from original matrix when constructing submatrices
    cols: list(Constraint.t),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(row);

  let of_constraints = (xis: list(Constraint.t)): t => {
    List.mapi(
      (idx, xi) =>
        {
          idx,
          cols: [xi],
        },
      xis,
    );
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

  let rev = (s: t): t => {
    ...s,
    ctrs: Ctr.Map.map(Matrix.rev, s.ctrs),
  };

  let empty = {
    ctrs: Ctr.Map.empty,
    first_col_exhaustive: false,
    first_col_redundant_rows: [],
  };

  type seen = {
    seen_ints: IntSet.t,
    seen_sints: SIntSet.t,
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
    seen_sints: SIntSet.empty,
    seen_floats: FloatSet.empty,
    seen_strings: StringSet.empty,
    seen_ctrs: Ctr.Set.empty,
    seen_all_ctrs: false,
    seen_truth: false,
    seen_hole: false,
    first_col_redundant_rows: [],
  };

  let add_redundant_row_if = (cond: bool, idx: int, redundant_rows) =>
    if (cond) {
      [idx, ...redundant_rows];
    } else {
      redundant_rows;
    };

  // data accumulation pass over the first column of the matrix
  let seen = (m: Matrix.t, all_ctrs: Ctr.all_ctrs): seen => {
    List.fold_left(
      (seen, row: Matrix.row) =>
        switch (row.cols) {
        | [] => seen
        | [BigInt(n), ..._] => {
            ...seen,
            seen_ints: IntSet.add(n, seen.seen_ints),
            first_col_redundant_rows:
              add_redundant_row_if(
                IntSet.mem(n, seen.seen_ints) || seen.seen_truth,
                row.idx,
                seen.first_col_redundant_rows,
              ),
          }
        | [SInt(n), ..._] => {
            ...seen,
            seen_sints: SIntSet.add(n, seen.seen_sints),
            first_col_redundant_rows:
              add_redundant_row_if(
                SIntSet.mem(n, seen.seen_sints) || seen.seen_truth,
                row.idx,
                seen.first_col_redundant_rows,
              ),
          }
        | [Float(x), ..._] => {
            ...seen,
            seen_floats: seen.seen_floats |> FloatSet.add(x),
            first_col_redundant_rows:
              add_redundant_row_if(
                FloatSet.mem(x, seen.seen_floats) || seen.seen_truth,
                row.idx,
                seen.first_col_redundant_rows,
              ),
          }
        | [String(s), ..._] => {
            ...seen,
            seen_strings: seen.seen_strings |> StringSet.add(s),
            first_col_redundant_rows:
              add_redundant_row_if(
                StringSet.mem(s, seen.seen_strings) || seen.seen_truth,
                row.idx,
                seen.first_col_redundant_rows,
              ),
          }
        | [Tuple(elts), ..._] =>
          let ctr = Ctr.tuple_ctr(List.length(elts));
          let seen_ctrs = Ctr.Set.add(ctr, seen.seen_ctrs);
          let seen_all_ctrs =
            seen.seen_all_ctrs || Ctr.seen_all_ctrs(seen_ctrs, all_ctrs);
          {
            ...seen,
            seen_ctrs,
            seen_all_ctrs,
            first_col_redundant_rows:
              add_redundant_row_if(
                Ctr.Set.mem(ctr, seen.seen_ctrs) || seen.seen_truth,
                row.idx,
                seen.first_col_redundant_rows,
              ),
          };
        | [Ap(c, arg), ..._] =>
          let ctr =
            Ctr.mk(
              c,
              switch (arg) {
              | Some(_) => 1
              | None => 0
              },
            );
          let seen_ctrs = Ctr.Set.add(ctr, seen.seen_ctrs);
          let seen_all_ctrs =
            seen.seen_all_ctrs || Ctr.seen_all_ctrs(seen_ctrs, all_ctrs);
          {
            ...seen,
            seen_ctrs,
            seen_all_ctrs,
            first_col_redundant_rows:
              add_redundant_row_if(
                Ctr.Set.mem(ctr, seen.seen_ctrs) || seen.seen_truth,
                row.idx,
                seen.first_col_redundant_rows,
              ),
          };
        | [Truth, ..._] => {
            ...seen,
            seen_truth: true,
            first_col_redundant_rows:
              add_redundant_row_if(
                seen.seen_truth || seen.seen_all_ctrs,
                row.idx,
                seen.first_col_redundant_rows,
              ),
          }
        | [Hole, ..._] => {
            ...seen,
            seen_hole: true,
            first_col_redundant_rows:
              add_redundant_row_if(
                seen.seen_truth || seen.seen_all_ctrs,
                row.idx,
                seen.first_col_redundant_rows,
              ),
          }
        },
      init_seen,
      m,
    );
  };

  let add_row =
      (idx: int, cols: list(Constraint.t), data: option(Matrix.t))
      : option(Matrix.t) =>
    switch (data) {
    | Some(matrix) =>
      Some([
        {
          idx,
          cols,
        },
        ...matrix,
      ])
    | None =>
      Some([
        {
          idx,
          cols,
        },
      ])
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
      seen_sints: _,
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
      | Unknown => true
      | Infinite => true
      | Finite(_) => !seen_all_ctrs
      };

    let submatrices =
      List.fold_left(
        (submatrices, row: Matrix.row) => {
          switch (row.cols) {
          | [] => submatrices
          | [SInt(n), ...cols] => {
              ...submatrices,
              ctrs:
                update_ctrs(Ctr.of_sint(n), row.idx, cols, submatrices.ctrs),
            }
          | [BigInt(n), ...cols] => {
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

            {
              ...submatrices,
              ctrs,
            };
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
    {
      ...submatrices,
      first_col_exhaustive,
      first_col_redundant_rows,
    };
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type result = {
  is_exhaustive: bool,
  redundant_rows,
};

// We assume col_tys is already normalized.
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
      let all_ctrs = Ctr.all_ctrs_of_typ(first_col_ty);
      let submatrices = Submatrices.of_matrix(m, all_ctrs);
      let (is_exhaustive, redundant_rows) =
        Ctr.Map.fold(
          (ctr, submatrix, (is_exhaustive, redundant_rows)) => {
            // for each submatrix, recursively check_matrix, computing the col_tys based
            // on the first_col_ty and the constructor name.
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
      {
        is_exhaustive,
        redundant_rows,
      };
    }
  };
};

// IMPORTANT: ty should already be fully normalized.
let check = (xis: list(Constraint.t), ty: Typ.t): result => {
  check_matrix(Matrix.of_constraints(xis), [ty]);
};
