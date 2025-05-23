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
    | Forall(_)
    | Var(_) => Infinite
    | Parens(_)
    | Ap(_)
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

module Seen = {
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
};

module UnseenPatternList = {
  include Seen;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(Grammar.pat_t(IdTagged.IdTag.t));

  let empty = [];

  /* prepend the constructor to the unseen pattern list. Based on the constructor
     amd the column type, the list will be modified in different ways.*/
  let prepend_ctr = (ctr: Ctr.t, col_type: Typ.t, unseen_pattern: t) => {
    switch (col_type.term) {
    | Sum(_)
    | Rec(_) =>
      // convert default ctr to a wildcard
      let pat_ctr =
        if (ctr == Ctr.default_ctr) {
          IdTagged.FreshGrammar.Pat.wild();
        } else {
          IdTagged.FreshGrammar.Pat.constructor(ctr.ctr, None);
        };

      if (Ctr.num_args_of(ctr) == 0) {
        [pat_ctr, ...unseen_pattern];
      } else {
        // absorb the args of the constructor
        // the empty case can happen if the example is providing a constructor
        // that has args, but no args are provided
        switch (unseen_pattern) {
        | [] => [
            IdTagged.FreshGrammar.Pat.ap(
              pat_ctr,
              IdTagged.FreshGrammar.Pat.empty_hole(),
            ),
            ...unseen_pattern,
          ]
        | [hd, ...tl] => [
            // absorb the args of the constructor
            IdTagged.FreshGrammar.Pat.ap(pat_ctr, hd),
            ...tl,
          ]
        };
      };
    | Prod(elts) =>
      // take the number of elements we need from the unseen list
      // and package them into the tuple
      let num_elts = List.length(elts);
      let rec partition_first_n = (n, list, acc) =>
        if (n == 0) {
          (acc, list);
        } else {
          switch (list) {
          | [] => (acc, list)
          | [hd, ...tl] => partition_first_n(n - 1, tl, [hd, ...acc])
          };
        };

      let (first_n, tl) = partition_first_n(num_elts, unseen_pattern, []);

      [IdTagged.FreshGrammar.Pat.tuple(List.rev(first_n)), ...tl];
    | TupLabel(body, _) =>
      // associate the tuple's labels to element in the unseen list
      switch (IdTagged.term_of(body)) {
      | Label(label) =>
        switch (unseen_pattern) {
        | [] => [
            IdTagged.FreshGrammar.Pat.label(label),
            IdTagged.FreshGrammar.Pat.empty_hole(),
          ]
        | [hd, ...tl] => [
            IdTagged.FreshGrammar.Pat.tup_label(
              IdTagged.FreshGrammar.Pat.label(label),
              hd,
            ),
            ...tl,
          ]
        }
      | _ => unseen_pattern
      }
    | List(_) =>
      switch (ctr.ctr) {
      | "nil" => [IdTagged.FreshGrammar.Pat.list_lit([]), ...unseen_pattern]
      | "cons" =>
        switch (unseen_pattern) {
        | [] => [
            IdTagged.FreshGrammar.Pat.cons(
              IdTagged.FreshGrammar.Pat.wild(),
              IdTagged.FreshGrammar.Pat.wild(),
            ),
            ...unseen_pattern,
          ]
        | [hd, ...tl] =>
          // the structure of the list should have a tuple that contains
          // the element in the first position, and a cons in the second.
          // The goal is to unwrap that and just get the cons.
          // Everything else is just making sure weird errors don't happen.
          let term = IdTagged.term_of(hd);
          let cons =
            switch (term) {
            | Tuple([_, snd]) =>
              IdTagged.FreshGrammar.Pat.cons(
                IdTagged.FreshGrammar.Pat.wild(),
                snd,
              )
            | _ =>
              IdTagged.FreshGrammar.Pat.cons(
                IdTagged.FreshGrammar.Pat.wild(),
                hd,
              )
            };
          [cons, ...tl];
        }
      | _ => [IdTagged.FreshGrammar.Pat.wild(), ...unseen_pattern]
      }
    | Atom(Bool) =>
      let boolTyp =
        switch (ctr.ctr) {
        | "true" => IdTagged.FreshGrammar.Pat.bool(true)
        | "false" => IdTagged.FreshGrammar.Pat.bool(false)
        | _ => IdTagged.FreshGrammar.Pat.wild()
        };
      [boolTyp, ...unseen_pattern];
    | Unknown(_) => [IdTagged.FreshGrammar.Pat.wild(), ...unseen_pattern]
    | Atom(Int) => [
        try(IdTagged.FreshGrammar.Pat.big_int(Bigint.of_string(ctr.ctr))) {
        | _ => IdTagged.FreshGrammar.Pat.wild()
        },
        ...unseen_pattern,
      ]
    | Atom(SInt) => [
        try(IdTagged.FreshGrammar.Pat.sint(int_of_string(ctr.ctr))) {
        | _ => IdTagged.FreshGrammar.Pat.wild()
        },
        ...unseen_pattern,
      ]
    | Atom(Float) => [
        try(IdTagged.FreshGrammar.Pat.float(float_of_string(ctr.ctr))) {
        | _ => IdTagged.FreshGrammar.Pat.wild()
        },
        ...unseen_pattern,
      ]
    | Atom(Nat) => [
        try(IdTagged.FreshGrammar.Pat.nat(Bigint.of_string(ctr.ctr))) {
        | _ => IdTagged.FreshGrammar.Pat.wild()
        },
        ...unseen_pattern,
      ]
    | Atom(String) => [
        // ctr has a " as the first character
        if (ctr == Ctr.default_ctr) {
          IdTagged.FreshGrammar.Pat.wild();
        } else {
          IdTagged.FreshGrammar.Pat.string(
            String.sub(ctr.ctr, 1, String.length(ctr.ctr) - 1),
          );
        },
        ...unseen_pattern,
      ]
    | Arrow(_)
    | Forall(_)
    | Var(_) => unseen_pattern
    | Parens(_)
    | Ap(_)
    | Label(_) =>
      failwith(
        "prepend_ctr called with a non-normalized type: "
        ++ Typ.show(col_type),
      )
    };
  };

  let get_first_unseen_ctr = (seen_in_col: seen, all_ctrs) => {
    seen_in_col.seen_all_ctrs
      ? Ctr.default_ctr
      : List.split(Ctr.Map.bindings(all_ctrs))
        |> fst
        |> List.find(ctr => !Ctr.Set.mem(ctr, seen_in_col.seen_ctrs));
  };

  /*
   Generated and prepend the new item to the list based on the type of the column.

   `use_type_default` is used to determine if we should use a default for the column
   rather than an unseen type.seen_ints
   - E.g. for ints/string/etc. this default type is a wildcard.
   */
  let prepend_with_type =
      (
        seen_in_first_col: seen,
        col_type: Typ.t,
        col_ctr: Ctr.t,
        unseen_pattern: t,
        use_type_default: bool,
      )
      : t => {
    let all_ctrs = Ctr.all_ctrs_of_typ(col_type);

    let (elt, unseen_pattern) =
      switch (col_type.term) {
      | Sum(_)
      | Rec(_) =>
        switch (all_ctrs) {
        | Unknown
        | Infinite => (Ctr.default_ctr, unseen_pattern)
        | Finite(all_ctrs) =>
          let new_ctr =
            if (use_type_default) {
              Ctr.Map.choose(all_ctrs) |> fst;
            } else {
              get_first_unseen_ctr(seen_in_first_col, all_ctrs);
            };

          // handle the case where the old constructor has arugments
          // that have accumulated in the list
          // Do this by just removing them, since the args will
          // be packeged into a tuple
          let unseen_pattern =
            switch (unseen_pattern) {
            | [_, ...tl] when Ctr.num_args_of(col_ctr) > 0 => tl
            | _ => unseen_pattern
            };

          if (Ctr.num_args_of(new_ctr) > 0) {
            (
              // if the new construct has args, we need to give it
              // an argument
              new_ctr,
              [IdTagged.FreshGrammar.Pat.wild(), ...unseen_pattern],
            );
          } else {
            (new_ctr, unseen_pattern);
          };
        }
      | Atom(Bool) =>
        switch (all_ctrs) {
        | Unknown
        | Infinite => (Ctr.default_ctr, unseen_pattern)
        | Finite(all_ctrs) => (
            if (use_type_default) {
              Ctr.false_ctr;
            } else {
              get_first_unseen_ctr(seen_in_first_col, all_ctrs);
            },
            unseen_pattern,
          )
        }
      | List(_) =>
        switch (all_ctrs) {
        | Unknown
        | Infinite => (col_ctr, unseen_pattern)
        | Finite(all_ctrs) =>
          if (use_type_default) {
            // the terminal cons/nil case will have 0 arguments,
            // so we want to generate a default constructor for it
            if (Ctr.num_args_of(col_ctr) <= 0) {
              (Ctr.default_ctr, unseen_pattern);
            } else {
              // otherwise, the non terminal character wants to generate
              // a new wildcard. So, discard the existing wildcard.
              // TODO: just update this function to not make a call to prepend_ctr
              switch (unseen_pattern) {
              | [] => (Ctr.default_ctr, unseen_pattern)
              | [_, ...tl] => (Ctr.default_ctr, tl)
              };
            };
          } else {
            let unseen_ctr =
              get_first_unseen_ctr(seen_in_first_col, all_ctrs);
            if (col_ctr == Ctr.nil_ctr) {
              (
                unseen_ctr,
                [IdTagged.FreshGrammar.Pat.wild(), ...unseen_pattern],
              );
            } else if (Ctr.num_args_of(col_ctr) > 0
                       && unseen_ctr == Ctr.nil_ctr) {
              // if the unseen ctr is a nil, and the current ctr has args,
              // it's a cons and we need to get rid of those args
              // it's guaranteed to be a tuple of whatever.
              // when the user is performing actions, unseen_pattern may be empty
              switch (unseen_pattern) {
              | [] => (unseen_ctr, unseen_pattern)
              | [_, ...tl] => (unseen_ctr, tl)
              };
            } else {
              (unseen_ctr, unseen_pattern);
            };
          }
        }
      | Prod(_) => (col_ctr, unseen_pattern) // will be ignored in the later prepend step
      | Unknown(_) => (col_ctr, unseen_pattern)
      | TupLabel(_) => (col_ctr, unseen_pattern)
      | Atom(Int)
      | Atom(Nat) => (
          if (use_type_default) {
            Ctr.default_ctr;
          } else {
            let rec first_unused_bigint = n => {
              let big_int = Bigint.of_int(n);
              IntSet.mem(big_int, seen_in_first_col.seen_ints)
                ? first_unused_bigint(n + 1) : Ctr.of_int(Bigint.of_int(n));
            };
            first_unused_bigint(0);
          },
          unseen_pattern,
        )
      | Atom(SInt) => (
          if (use_type_default) {
            Ctr.default_ctr;
          } else {
            let rec first_unused_sint = n => {
              SIntSet.mem(n, seen_in_first_col.seen_sints)
                ? first_unused_sint(n + 1) : Ctr.of_sint(n);
            };
            first_unused_sint(0);
          },
          unseen_pattern,
        )
      | Atom(Float) => (
          if (use_type_default) {
            Ctr.default_ctr;
          } else {
            let rec first_unused_float = n => {
              FloatSet.mem(n, seen_in_first_col.seen_floats)
                ? first_unused_float(n +. 1.) : Ctr.of_float(n);
            };
            first_unused_float(0.);
          },
          unseen_pattern,
        )
      | Atom(String) => (
          if (use_type_default) {
            Ctr.default_ctr;
          } else {
            let rec first_unused_str = n => {
              StringSet.mem(n, seen_in_first_col.seen_strings)
                ? first_unused_str(n ++ "*") : Ctr.of_string(n);
            };
            first_unused_str("");
          },
          unseen_pattern,
        )
      | Arrow(_)
      | Forall(_)
      | Var(_) => (Ctr.default_ctr, unseen_pattern)
      | Parens(_)
      | Ap(_)
      | Label(_) =>
        failwith(
          "prepend_with_type called with a non-normalized type: "
          ++ Typ.show(col_type),
        )
      };

    prepend_ctr(elt, col_type, unseen_pattern);
  };

  /*The unseen list as a grammatical pattern*/
  let to_pat = (unseen_pattern: t) => {
    Grammar.Pat(
      switch (List.length(unseen_pattern)) {
      | 1 => List.hd(unseen_pattern)
      | _ => IdTagged.FreshGrammar.Pat.tuple(unseen_pattern)
      },
    );
  };
};

module Submatrices = {
  include Seen;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    ctrs: Ctr.Map.t(Matrix.t),
    first_col_exhaustive: bool,
    first_col_redundant_rows: redundant_rows,
    prepend_first_col_unseen_ctr:
      (Ctr.t, UnseenPatternList.t, bool) => UnseenPatternList.t,
  };

  let rev = (s: t): t => {
    ...s,
    ctrs: Ctr.Map.map(Matrix.rev, s.ctrs),
  };

  let empty = {
    ctrs: Ctr.Map.empty,
    first_col_exhaustive: false,
    first_col_redundant_rows: [],
    prepend_first_col_unseen_ctr: (_, _, _) => {
      [];
    },
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

  let of_matrix =
      (m: Matrix.t, all_ctrs: Ctr.all_ctrs, first_col_ty: Typ.t): t => {
    let seen_data = seen(m, all_ctrs);
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
    } = seen_data;

    let include_default =
      switch (all_ctrs) {
      | Unknown => true
      | Infinite => true
      | Finite(_) => !seen_all_ctrs
      };

    // TODO: there seems to be a bug with wildcard handling
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

    let prepend_first_col_unseen_ctr =
      UnseenPatternList.prepend_with_type(seen_data, first_col_ty);

    {
      ...submatrices,
      first_col_exhaustive,
      first_col_redundant_rows,
      prepend_first_col_unseen_ctr,
    };
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type result = {
  is_exhaustive: bool,
  redundant_rows,
  unseen_pattern: UnseenPatternList.t,
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
        unseen_pattern: UnseenPatternList.empty,
      };
    } else {
      let all_ctrs = Ctr.all_ctrs_of_typ(first_col_ty);
      let submatrices = Submatrices.of_matrix(m, all_ctrs, first_col_ty);

      let (is_exhaustive, redundant_rows, unseen_pattern) =
        Ctr.Map.fold(
          (ctr, submatrix, (is_exhaustive, redundant_rows, unseen_pattern)) => {
            // for each submatrix, recursively check_matrix, computing the col_tys based
            // on the first_col_ty and the constructor name.
            let arity = Ctr.arity_of(ctr, all_ctrs);
            let col_tys = arity @ rem_col_tys;
            switch (col_tys) {
            | [] =>
              let unseen_pattern =
                submatrices.prepend_first_col_unseen_ctr(
                  ctr,
                  UnseenPatternList.empty,
                  submatrices.first_col_exhaustive,
                );
              (is_exhaustive, redundant_rows, unseen_pattern);
            | _ =>
              let submatrix_check_result = check_matrix(submatrix, col_tys);
              let is_still_exhaustive =
                is_exhaustive && submatrix_check_result.is_exhaustive;

              // update the unseen list based on exhaustiveness
              let unseen_pattern =
                if (is_still_exhaustive && !submatrices.first_col_exhaustive) {
                  // if the following column did not break exhaustiveness, but this one does,
                  // we place the unseen value into the list
                  submatrices.prepend_first_col_unseen_ctr(
                    ctr,
                    submatrix_check_result.unseen_pattern,
                    false,
                  );
                } else if (is_still_exhaustive
                           && submatrices.first_col_exhaustive) {
                  // if this column is exhaustive and the following column did not break exhaustiveness,
                  // use the default unseen value for the type
                  submatrices.prepend_first_col_unseen_ctr(
                    ctr,
                    submatrix_check_result.unseen_pattern,
                    true,
                  );
                } else if (is_exhaustive) {
                  // otherwise, we just use a default/known to exist ctr
                  UnseenPatternList.prepend_ctr(
                    ctr,
                    first_col_ty,
                    submatrix_check_result.unseen_pattern,
                  );
                } else {
                  unseen_pattern;
                };

              let redundant_rows =
                List.filter(
                  (idx: int) => {
                    !Matrix.contains_row(idx, submatrix)
                    || List.mem(idx, submatrix_check_result.redundant_rows)
                  },
                  redundant_rows,
                );
              (is_still_exhaustive, redundant_rows, unseen_pattern);
            };
          },
          submatrices.ctrs,
          (
            true,
            submatrices.first_col_redundant_rows,
            UnseenPatternList.empty,
          ),
        );

      {
        is_exhaustive: is_exhaustive && submatrices.first_col_exhaustive,
        redundant_rows,
        unseen_pattern,
      };
    }
  };
};

// IMPORTANT: ty should already be fully normalized.
let check = (xis: list(Constraint.t), ty: Typ.t): result => {
  let res = check_matrix(Matrix.of_constraints(xis), [ty]);
  res;
};
