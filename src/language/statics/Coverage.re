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
    | Tuple(list(t))
    | NEHole(t); // expresses that the constraint inside is inside of a hole

  let nil = Ap("nil", None);
  let cons = (hd, tl) => Ap("cons", Some(Tuple([hd, tl])));

  let true_ = Ap("true", None);
  let false_ = Ap("false", None);
};

module Ctr = {
  module M = {
    // A status marker used for building missing pattern examples.
    // The primary purpose is building examples when patterns with holes
    // exists.
    [@deriving (show({with_path: false}), sexp, yojson)]
    type status =
      | Unknown
      | Okay
      | NEHole // the first ctr inside a non-empty hole
      | InHole;

    // Ctrs are Constructors equipped with arities and a status.
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      ctr: Constructor.t,
      num_args: int,
      string_key: string,
      status,
    };

    let compare =
        (
          {ctr: _, num_args: _, string_key: string_key1, status: _},
          {ctr: _, num_args: _, string_key: string_key2, status: _},
        ) =>
      compare(string_key1, string_key2);
  };

  include M;
  module Map = MapUtil.Make(M);
  module Set = Set.Make(M);

  let mk = (ctr, num_args, status) => {
    let string_key = string_of_int(num_args) ++ "~" ++ ctr;
    {
      ctr,
      num_args,
      string_key,
      status,
    };
  };
  let num_args_of = (ctr: t): int => ctr.num_args;

  // Ctrs for primitive types
  // Names here should not be anotherwise valid sum type constructor names.
  let of_int = n => mk(Bigint.to_string(n), 0);
  let of_sint = n => mk(string_of_int(n), 0);
  let of_float = x => mk(string_of_float(x), 0);
  let of_string = s => mk("\"" ++ s, 0); // don't need closing quote, just need to distinguish from others
  let tuple_ctr = n => mk("tuple", n);
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
               Some((mk(ctr, 0, Unknown), []))
             | Variant(ctr, _, Some(arg_ty)) =>
               Some((mk(ctr, 1, Unknown), [arg_ty]))
             | BadEntry(_) => None,
           )
        |> Map.of_list,
      )
    | Rec({term: Var(w), _}, {term: Var(v), _}) when v == w => Unknown
    | Rec(_) => all_ctrs_of_typ(Typ.unroll(ty))
    | Prod(elts) =>
      Finite(Map.singleton(tuple_ctr(List.length(elts), Unknown), elts))
    | TupLabel(_, ty) =>
      Finite(Map.singleton(tuple_ctr(1, Unknown), [ty]))
    | List(elt_ty) =>
      Finite(
        Map.of_list([
          (nil_ctr(Unknown), []),
          (cons_ctr(Unknown), [Prod([elt_ty, ty]) |> Typ.temp]),
        ]),
      )
    | Atom(Bool) =>
      Finite(
        Map.of_list([(true_ctr(Unknown), []), (false_ctr(Unknown), [])]),
      )
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

  // is the status that is "most correct" of two constructors. That is,
  // - if any constructor is Okay, the lub is Okay.
  // - if one is InHole and the other is NEHole, the lub is NEHole
  // - if both are InHole, the lub is InHole
  let lub_ctr_status = (ctr_a: t, ctr_b: t) => {
    switch (ctr_a.status) {
    | Unknown => ctr_b.status
    | Okay => Okay
    | NEHole =>
      switch (ctr_b.status) {
      | Okay => Okay
      | Unknown
      | InHole
      | NEHole => NEHole
      }
    | InHole =>
      switch (ctr_b.status) {
      | Okay => Okay
      | Unknown
      | InHole => InHole
      | NEHole => NEHole
      }
    };
  };

  let is_status_hole = (status: status): bool =>
    switch (status) {
    | NEHole => true
    | Unknown
    | Okay
    | InHole => false
    };

  let is_in_hole = (ctr: t) => is_status_hole(ctr.status);
};

[@deriving (show({with_path: false}), sexp, yojson)]
type redundant_rows = list(int);

module Matrix = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type col = {
    cons: Constraint.t,
    in_hole: bool // primarily used for generating examples when holes exist
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type row = {
    idx: int, // retaining row index from original matrix when constructing submatrices
    cols: list(col),
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
      List.map(
        xi =>
          {
            cons: xi,
            in_hole: false,
          },
        xis,
      ),
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

module type UnseenPatternList = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type pat_t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t;

  let empty: t;

  let is_empty: t => bool;

  let has_holes: t => bool;

  /* prepend the constructor to the unseen pattern list. Based on the constructor
     amd the column type, the list will be modified in different ways.*/
  let cons_ctr: (Ctr.t, Typ.t, t) => t;

  /* Adds a wildcard with the given status to the beginning of the list*/
  let cons_wild: (Ctr.status, t) => t;

  /*
   Generate and prepend the new item to the list based on the type of the column.
   */
  let cons_from_type: (Seen.seen, Typ.t, Ctr.t, t) => t;

  /*
   Generate's a type's "default" constructor and adds it to the beginning of thhe
   unseen pattern list.
   */
  let cons_default_from_type: (Typ.t, Ctr.t, t) => t;

  /*The unseen list as a grammatical pattern*/
  let to_pat: t => Grammar.any_t(IdTagged.IdTag.t);

  let has_less_errors: (t, t) => bool;
};

// A list of expressions that represents an unseen pattern.
module UnseenPatternList: UnseenPatternList = {
  open IdTagged.FreshGrammar.Pat;

  include Seen;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type pat_t = Grammar.pat_t(IdTagged.IdTag.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    pat: list(pat_t),
    // these two aren't maintained precisely, but that's fine
    // More specifcally, when stuff gets removed, these numbers
    // don't decrease. Tuples also count as patterns
    num_holes: int,
    num_pats: int,
  };

  let empty = {
    pat: [],
    num_holes: 0,
    num_pats: 0,
  };

  let has_holes = unseen_pat => unseen_pat.num_holes > 0;

  let is_empty = unseen_pat => {
    switch (unseen_pat.pat) {
    | [] => true
    | _ => false
    };
  };

  let cons_pat_t = (status: Ctr.status, pat: pat_t, unseen_pat: t) => {
    num_pats: unseen_pat.num_pats + 1,
    num_holes: unseen_pat.num_holes + (Ctr.is_status_hole(status) ? 1 : 0),
    pat: [pat, ...unseen_pat.pat],
  };

  let cons_ctr = (ctr: Ctr.t, col_type: Typ.t, unseen_pattern: t) => {
    let pat_list = unseen_pattern.pat;
    let cons_pat_t = (pat, unseen_pattern) =>
      cons_pat_t(ctr.status, pat, unseen_pattern);

    switch (col_type.term) {
    // wildcards do nothing special
    // also resolves an edge case with ctr/col_type mismatch
    | _ when Ctr.compare(ctr, Ctr.default_ctr(ctr.status)) == 0 =>
      cons_pat_t(wild(), unseen_pattern)
    | Sum(_)
    | Rec(_) =>
      let pat_ctr = constructor(ctr.ctr, None);
      if (Ctr.num_args_of(ctr) == 0) {
        cons_pat_t(pat_ctr, unseen_pattern);
      } else {
        // absorb the args of the constructor
        // the empty case can happen if the example is providing a constructor
        // that has args, but no args are provided
        switch (pat_list) {
        | [] => cons_pat_t(ap(pat_ctr, wild()), unseen_pattern)
        | [hd, ...tl] =>
          cons_pat_t(
            // absorb the args of the constructor
            ap(pat_ctr, hd),
            {
              ...unseen_pattern,
              pat: tl,
            },
          )
        };
      };
    | Prod(_) =>
      // take the number of elements we need from the unseen list
      // and package them into the tuple
      let num_elts = Ctr.num_args_of(ctr);
      let rec partition_first_n = (n, list, acc) =>
        if (n == 0) {
          (acc, list);
        } else {
          switch (list) {
          | [] => (acc, list)
          | [hd, ...tl] => partition_first_n(n - 1, tl, [hd, ...acc])
          };
        };

      let (first_n, tl) = partition_first_n(num_elts, pat_list, []);

      cons_pat_t(
        tuple(List.rev(first_n)),
        {
          ...unseen_pattern,
          pat: tl,
        },
      );
    | TupLabel(body, _) =>
      // associate the tuple's labels to element in the unseen list
      switch (IdTagged.term_of(body)) {
      | Label(pat_label) =>
        switch (pat_list) {
        | [] =>
          cons_pat_t(wild(), unseen_pattern) |> cons_pat_t(label(pat_label))
        | [hd, ...tl] =>
          cons_pat_t(
            tup_label(label(pat_label), hd),
            {
              ...unseen_pattern,
              pat: tl,
            },
          )
        }
      | _ => failwith("TupLabel without a label in unseen pattern list")
      }
    | List(_) =>
      switch (ctr.ctr) {
      | "nil" => cons_pat_t(list_lit([]), unseen_pattern)
      | "cons" =>
        switch (pat_list) {
        | [] => cons_pat_t(cons(wild(), wild()), unseen_pattern) // this shouldn't happen
        | [hd, ...tl] =>
          // the structure of the list should have a tuple that contains
          // the element in the first position, and a cons in the second.
          // Everything else is just making sure weird errors don't happen.
          let term = IdTagged.term_of(hd);
          let cons =
            switch (term) {
            | Tuple([fst, snd]) => cons(fst, snd)
            | _ => cons(wild(), hd)
            };

          cons_pat_t(
            cons,
            {
              ...unseen_pattern,
              pat: tl,
            },
          );
        }
      | _ => cons_pat_t(wild(), unseen_pattern)
      }
    | Atom(Bool) =>
      let boolTyp =
        switch (ctr.ctr) {
        | "true" => bool(true)
        | "false" => bool(false)
        | _ => wild()
        };
      cons_pat_t(boolTyp, unseen_pattern);
    | Unknown(_) => cons_pat_t(wild(), unseen_pattern)
    | Atom(Int) =>
      cons_pat_t(
        // while the user is perfroming actions, parse errors can occur.
        // this just inserts a wildcard instead of that happens.
        try(big_int(Bigint.of_string(ctr.ctr))) {
        | _ => wild()
        },
        unseen_pattern,
      )
    | Atom(SInt) =>
      cons_pat_t(
        try(sint(int_of_string(ctr.ctr))) {
        | _ => wild()
        },
        unseen_pattern,
      )
    | Atom(Float) =>
      cons_pat_t(
        try(float(float_of_string(ctr.ctr))) {
        | _ => wild()
        },
        unseen_pattern,
      )
    | Atom(Nat) =>
      cons_pat_t(
        try(nat(Bigint.of_string(ctr.ctr))) {
        | _ => wild()
        },
        unseen_pattern,
      )
    | Atom(String) =>
      cons_pat_t(
        // treat any string wildcard as a normal wildcard
        if (Ctr.compare(ctr, Ctr.default_ctr(ctr.status)) == 0) {
          wild();
        } else {
          // ctr has a " as the first character
          string(
            String.sub(ctr.ctr, 1, String.length(ctr.ctr) - 1),
          );
        },
        unseen_pattern,
      )
    | Arrow(_)
    | Forall(_)
    | Var(_) => unseen_pattern
    | Parens(_)
    | Label(_) =>
      failwith(
        "prepend_ctr called with a non-normalized type: "
        ++ Typ.show(col_type),
      )
    };
  };

  let cons_wild = (status: Ctr.status) =>
    cons_ctr(Ctr.default_ctr(status), Typ.temp(Unknown(Internal)));

  let find_first_unseen_ctr = (seen_in_col: seen, all_ctrs: Ctr.Map.t('a)) => {
    seen_in_col.seen_all_ctrs
      ? Ctr.default_ctr(Unknown)
      : List.split(Ctr.Map.bindings(all_ctrs))
        |> fst
        |> List.find(ctr => !Ctr.Set.mem(ctr, seen_in_col.seen_ctrs));
  };

  // add a sum type constructor to the unseen pattern list
  let cons_sum =
      (col_ctr: Ctr.t, col_type: Typ.t, new_ctr: Ctr.t, unseen_pattern: t) => {
    // handle the case where the old constructor has arguments
    // that have accumulated in the list
    // Do this by just removing them, since the args will
    // be packaged into a tuple
    let unseen_pattern_list =
      switch (unseen_pattern.pat) {
      | [_, ...tl] when Ctr.num_args_of(col_ctr) > 0 => {
          ...unseen_pattern,
          pat: tl,
        }
      | _ => unseen_pattern
      };

    if (Ctr.num_args_of(new_ctr) > 0) {
      cons_ctr(new_ctr, col_type, cons_wild(Ctr.Okay, unseen_pattern_list));
    } else {
      cons_ctr(new_ctr, col_type, unseen_pattern_list);
    };
  };

  let cons_from_type =
      (seen_in_col: seen, col_type: Typ.t, col_ctr: Ctr.t, unseen_pattern: t)
      : t => {
    let all_ctrs = Ctr.all_ctrs_of_typ(col_type);
    let pat_list = unseen_pattern.pat;

    switch (col_type.term) {
    | Sum(_)
    | Rec(_) =>
      switch (all_ctrs) {
      | Unknown
      | Infinite => cons_wild(col_ctr.status, unseen_pattern)
      | Finite(all_ctrs) =>
        let new_ctr = find_first_unseen_ctr(seen_in_col, all_ctrs);
        cons_sum(col_ctr, col_type, new_ctr, unseen_pattern);
      }
    | Atom(Bool) =>
      switch (all_ctrs) {
      | Unknown
      | Infinite => cons_wild(col_ctr.status, unseen_pattern)
      | Finite(all_ctrs) =>
        cons_ctr(
          find_first_unseen_ctr(seen_in_col, all_ctrs),
          col_type,
          unseen_pattern,
        )
      }
    | List(_) =>
      switch (all_ctrs) {
      | Unknown
      | Infinite => cons_ctr(col_ctr, col_type, unseen_pattern)
      | Finite(all_ctrs) =>
        let unseen_ctr = find_first_unseen_ctr(seen_in_col, all_ctrs);
        let is_unseen_ctr_nil =
          Ctr.compare(unseen_ctr, Ctr.nil_ctr(col_ctr.status)) == 0;
        if (Ctr.compare(col_ctr, Ctr.nil_ctr(col_ctr.status)) == 0) {
          cons_ctr(
            unseen_ctr,
            col_type,
            cons_wild(col_ctr.status, unseen_pattern),
          );
        } else if (Ctr.num_args_of(col_ctr) > 0 && is_unseen_ctr_nil) {
          // if the unseen ctr is a nil, and the current ctr has args,
          // it's a cons and we need to get rid of those args
          // it's guaranteed to be a tuple of whatever.
          // when the user is performing actions, unseen_pattern may be empty
          switch (pat_list) {
          | [] => cons_ctr(unseen_ctr, col_type, unseen_pattern)
          | [_, ...tl] =>
            cons_ctr(
              unseen_ctr,
              col_type,
              {
                ...unseen_pattern,
                pat: tl,
              },
            )
          };
        } else {
          cons_ctr(unseen_ctr, col_type, unseen_pattern);
        };
      }
    | Prod(_) => cons_ctr(col_ctr, col_type, unseen_pattern)
    | Unknown(_) => cons_ctr(col_ctr, col_type, unseen_pattern)
    | TupLabel(_) => cons_ctr(col_ctr, col_type, unseen_pattern)
    | Atom(Int)
    | Atom(Nat) =>
      let rec first_unused_bigint = n => {
        let big_int = Bigint.of_int(n);
        IntSet.mem(big_int, seen_in_col.seen_ints)
          ? first_unused_bigint(n + 1)
          : Ctr.of_int(Bigint.of_int(n), col_ctr.status);
      };

      cons_ctr(first_unused_bigint(0), col_type, unseen_pattern);
    | Atom(SInt) =>
      let rec first_unused_sint = n => {
        SIntSet.mem(n, seen_in_col.seen_sints)
          ? first_unused_sint(n + 1) : Ctr.of_sint(n, col_ctr.status);
      };

      cons_ctr(first_unused_sint(0), col_type, unseen_pattern);
    | Atom(Float) =>
      let rec first_unused_float = n => {
        FloatSet.mem(n, seen_in_col.seen_floats)
          ? first_unused_float(n +. 1.) : Ctr.of_float(n, col_ctr.status);
      };

      cons_ctr(first_unused_float(0.), col_type, unseen_pattern);
    | Atom(String) =>
      let rec first_unused_str = n => {
        StringSet.mem(n, seen_in_col.seen_strings)
          ? first_unused_str(n ++ "*") : Ctr.of_string(n, col_ctr.status);
      };

      cons_ctr(first_unused_str(""), col_type, unseen_pattern);
    | Arrow(_)
    | Forall(_)
    | Var(_) => cons_wild(col_ctr.status, unseen_pattern)
    | Parens(_)
    | Label(_) =>
      failwith(
        "cons_from_type called with a non-normalized type: "
        ++ Typ.show(col_type),
      )
    };
  };

  let cons_default_from_type =
      (col_type: Typ.t, col_ctr: Ctr.t, unseen_pattern: t) => {
    let all_ctrs = Ctr.all_ctrs_of_typ(col_type);
    let pat_list = unseen_pattern.pat;

    let cons_wild = cons_wild(col_ctr.status);

    switch (col_type.term) {
    | Sum(_)
    | Rec(_) =>
      switch (all_ctrs) {
      | Unknown
      | Infinite => cons_wild(unseen_pattern)
      | Finite(all_ctrs) =>
        let new_ctr = Ctr.Map.choose(all_ctrs) |> fst;
        cons_sum(col_ctr, col_type, new_ctr, unseen_pattern);
      }
    | Atom(Bool) =>
      switch (all_ctrs) {
      | Unknown
      | Infinite => cons_wild(unseen_pattern)
      | Finite(_) =>
        cons_ctr(Ctr.false_ctr(col_ctr.status), col_type, unseen_pattern)
      }
    | List(_) =>
      switch (all_ctrs) {
      | Unknown
      | Infinite => unseen_pattern
      | Finite(_) =>
        // the terminal cons/nil case will have 0 arguments,
        // so we want to generate a default constructor for it
        if (Ctr.num_args_of(col_ctr) <= 0) {
          cons_wild(unseen_pattern);
        } else {
          // otherwise, the non-terminal ctr wants to generate
          // a new argument. So, discard the existing arg
          switch (pat_list) {
          | [] => cons_wild(unseen_pattern)
          | [_, ...tl] =>
            cons_wild({
              ...unseen_pattern,
              pat: tl,
            })
          };
        }
      }
    | Prod(_) => cons_ctr(col_ctr, col_type, unseen_pattern)
    | Unknown(_) => unseen_pattern
    | TupLabel(_) => cons_ctr(col_ctr, col_type, unseen_pattern)
    | Atom(Int)
    | Atom(Nat) => cons_wild(unseen_pattern)
    | Atom(SInt) => cons_wild(unseen_pattern)
    | Atom(Float) => cons_wild(unseen_pattern)
    | Atom(String) => cons_wild(unseen_pattern)
    | Arrow(_)
    | Forall(_)
    | Var(_) => cons_wild(unseen_pattern)
    | Parens(_)
    | Label(_) =>
      failwith(
        "prepend_from_type called with a non-normalized type: "
        ++ Typ.show(col_type),
      )
    };
  };

  let has_less_errors = (a, b) => {
    switch (a.num_pats, b.num_pats) {
    | (0, 0) => false
    | (_, 0) => true
    | (0, _) => false
    | (_, _) => a.num_holes / a.num_pats < b.num_holes / b.num_pats
    };
  };

  let to_pat = (unseen_pattern: t) => {
    let pat_list = unseen_pattern.pat;
    Grammar.Pat(
      switch (List.length(pat_list)) {
      | 1 => List.hd(pat_list)
      | 0 => wild()
      | _ => tuple(pat_list)
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
    cons_unseen_ctr: (Ctr.t, UnseenPatternList.t) => UnseenPatternList.t,
    cons_unseen_type_default:
      (Ctr.t, UnseenPatternList.t) => UnseenPatternList.t,
  };

  let rev = (s: t): t => {
    ...s,
    ctrs: Ctr.Map.map(Matrix.rev, s.ctrs),
  };

  let empty = {
    ctrs: Ctr.Map.empty,
    first_col_exhaustive: false,
    first_col_redundant_rows: [],
    cons_unseen_ctr: (_, _) => {
      UnseenPatternList.empty;
    },
    cons_unseen_type_default: (_, _) => {
      UnseenPatternList.empty;
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
    let rec seen' = (seen: seen, row: Matrix.row) => {
      switch (row.cols) {
      | [] => seen
      | [{cons: BigInt(n), in_hole: _}, ..._] => {
          ...seen,
          seen_ints: IntSet.add(n, seen.seen_ints),
          first_col_redundant_rows:
            add_redundant_row_if(
              IntSet.mem(n, seen.seen_ints) || seen.seen_truth,
              row.idx,
              seen.first_col_redundant_rows,
            ),
        }
      | [{cons: SInt(n), in_hole: _}, ..._] => {
          ...seen,
          seen_sints: SIntSet.add(n, seen.seen_sints),
          first_col_redundant_rows:
            add_redundant_row_if(
              SIntSet.mem(n, seen.seen_sints) || seen.seen_truth,
              row.idx,
              seen.first_col_redundant_rows,
            ),
        }
      | [{cons: Float(x), in_hole: _}, ..._] => {
          ...seen,
          seen_floats: seen.seen_floats |> FloatSet.add(x),
          first_col_redundant_rows:
            add_redundant_row_if(
              FloatSet.mem(x, seen.seen_floats) || seen.seen_truth,
              row.idx,
              seen.first_col_redundant_rows,
            ),
        }
      | [{cons: String(s), in_hole: _}, ..._] => {
          ...seen,
          seen_strings: seen.seen_strings |> StringSet.add(s),
          first_col_redundant_rows:
            add_redundant_row_if(
              StringSet.mem(s, seen.seen_strings) || seen.seen_truth,
              row.idx,
              seen.first_col_redundant_rows,
            ),
        }
      | [{cons: Tuple(elts), in_hole: _}, ..._] =>
        let ctr = Ctr.tuple_ctr(List.length(elts), Unknown);
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
      | [{cons: Ap(c, arg), in_hole: _}, ..._] =>
        let ctr =
          Ctr.mk(
            c,
            switch (arg) {
            | Some(_) => 1
            | None => 0
            },
            Unknown,
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
      | [{cons: Truth, in_hole: _}, ..._] => {
          ...seen,
          seen_truth: true,
          first_col_redundant_rows:
            add_redundant_row_if(
              seen.seen_truth || seen.seen_all_ctrs,
              row.idx,
              seen.first_col_redundant_rows,
            ),
        }
      | [{cons: Hole, in_hole: _}, ..._] => {
          ...seen,
          seen_hole: true,
          first_col_redundant_rows:
            add_redundant_row_if(
              seen.seen_truth || seen.seen_all_ctrs,
              row.idx,
              seen.first_col_redundant_rows,
            ),
        }
      | [{cons: NEHole(arg), in_hole: _}, ..._] =>
        seen'(
          seen,
          {
            cols: [
              {
                cons: arg,
                in_hole: true,
              },
            ],
            idx: row.idx,
          },
        )
      };
    };
    List.fold_left(
      (seen, row: Matrix.row) => {seen'(seen, row)},
      init_seen,
      m,
    );
  };

  let add_row =
      (idx: int, cols: list(Matrix.col), data: option(Matrix.t))
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

  let add_col = (cols, cons, in_hole) => {
    [
      Matrix.{
        cons,
        in_hole,
      },
      ...cols,
    ];
  };

  let update_ctrs =
      (
        ctr: Ctr.t,
        idx: int,
        cols: list(Matrix.col),
        ctrs: Ctr.Map.t(Matrix.t),
      )
      : Ctr.Map.t(Matrix.t) => {
    // overwrite the key's status to be the "most useful" status
    // for the purposes of generating complete examples.
    let status =
      switch (
        Ctr.Map.find_first_opt(ct => {Ctr.compare(ct, ctr) == 0}, ctrs)
      ) {
      | Some((other_ctr, _)) => Ctr.lub_ctr_status(ctr, other_ctr)
      | None => ctr.status
      };

    let ctr = {
      ...ctr,
      status,
    };

    Ctr.Map.update(ctr, add_row(idx, cols), ctrs);
  };

  let of_matrix =
      (m: Matrix.t, all_ctrs: Ctr.all_ctrs, first_col_ty: Typ.t): t => {
    let seen_data = seen(m, all_ctrs);
    let {
      seen_ints,
      seen_sints,
      seen_floats,
      seen_strings,
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

    // is the current status if `in_hole` is false,
    // otherwise overrides the current status with `InHole`
    let get_status = (ctr_status, in_hole) => {
      in_hole ? Ctr.InHole : ctr_status;
    };

    let is_in_hole = (ctr_status, in_hole) =>
      in_hole || Ctr.is_status_hole(ctr_status);

    let ctr_set_of_list = (to_ctr, elts) => {
      Ctr.Set.of_list(List.map(to_ctr, elts));
    };

    let submatrices = () => {
      open Matrix;

      let rec submatrix =
              (submatrices, row: Matrix.row, col_ctr_status: Ctr.status) => {
        let get_status = get_status(col_ctr_status);
        let is_in_hole = is_in_hole(col_ctr_status);

        switch (row.cols) {
        | [] => submatrices
        | [{cons: SInt(n), in_hole}, ...cols] => {
            ...submatrices,
            ctrs:
              update_ctrs(
                Ctr.of_sint(n, get_status(in_hole)),
                row.idx,
                cols,
                submatrices.ctrs,
              ),
          }
        | [{cons: BigInt(n), in_hole}, ...cols] => {
            ...submatrices,
            ctrs:
              update_ctrs(
                Ctr.of_int(n, get_status(in_hole)),
                row.idx,
                cols,
                submatrices.ctrs,
              ),
          }
        | [{cons: Float(x), in_hole}, ...cols] => {
            ...submatrices,
            ctrs:
              update_ctrs(
                Ctr.of_float(x, get_status(in_hole)),
                row.idx,
                cols,
                submatrices.ctrs,
              ),
          }
        | [{cons: String(s), in_hole}, ...cols] => {
            ...submatrices,
            ctrs:
              update_ctrs(
                Ctr.of_string(s, get_status(in_hole)),
                row.idx,
                cols,
                submatrices.ctrs,
              ),
          }
        | [{cons: Tuple(xis), in_hole}, ...cols] =>
          let cols' =
            List.map(
              cons =>
                {
                  cons,
                  in_hole: is_in_hole(in_hole),
                },
              xis,
            )
            @ cols;
          {
            ...submatrices,
            ctrs:
              update_ctrs(
                Ctr.tuple_ctr(List.length(xis), get_status(in_hole)),
                row.idx,
                cols',
                submatrices.ctrs,
              ),
          };
        | [{cons: Ap(c, arg), in_hole}, ...cols] =>
          let (ctr, cols') =
            switch (arg) {
            | Some(cons) => (
                Ctr.mk(c, 1),
                add_col(cols, cons, is_in_hole(in_hole)),
              )
            | None => (Ctr.mk(c, 0), cols)
            };
          {
            ...submatrices,
            ctrs:
              update_ctrs(
                ctr(get_status(in_hole)),
                row.idx,
                cols',
                submatrices.ctrs,
              ),
          };
        | [{cons: Truth, in_hole} | {cons: Hole, in_hole}, ...cols] =>
          // holes act like truth for the purposes of exhaustiveness checking

          // update all submatrices for seen ctrs
          let update_ctrs_with_truth = (seen_ctrs, ctrs) =>
            Ctr.Set.fold(
              (ctr, ctrs) => {
                let num_args = Ctr.num_args_of(ctr);
                let cols =
                  List.init(num_args, _ =>
                    {
                      cons: Constraint.Truth,
                      in_hole: Ctr.is_in_hole(ctr),
                    }
                  )
                  @ cols;
                update_ctrs(ctr, row.idx, cols, ctrs);
              },
              seen_ctrs,
              ctrs,
            );

          // update seen ctrs, ints, sints, floats, and strings to be truths
          let ctrs =
            update_ctrs_with_truth(seen_ctrs, submatrices.ctrs)
            |> update_ctrs_with_truth(
                 ctr_set_of_list(
                   elt => {Ctr.of_int(elt, get_status(in_hole))},
                   IntSet.to_list(seen_ints),
                 ),
               )
            |> update_ctrs_with_truth(
                 ctr_set_of_list(
                   elt => {Ctr.of_sint(elt, get_status(in_hole))},
                   SIntSet.to_list(seen_sints),
                 ),
               )
            |> update_ctrs_with_truth(
                 ctr_set_of_list(
                   elt => {Ctr.of_float(elt, get_status(in_hole))},
                   FloatSet.to_list(seen_floats),
                 ),
               )
            |> update_ctrs_with_truth(
                 ctr_set_of_list(
                   elt => {Ctr.of_string(elt, get_status(in_hole))},
                   StringSet.to_list(seen_strings),
                 ),
               );

          let ctrs =
            include_default
              ? update_ctrs(
                  Ctr.default_ctr(get_status(in_hole)),
                  row.idx,
                  cols,
                  ctrs,
                )
              : ctrs;

          {
            ...submatrices,
            ctrs,
          };
        | [{cons: NEHole(arg), in_hole}, ...cols] =>
          // assign the status of the immediate argument to NEHole
          submatrix(
            submatrices,
            {
              cols: add_col(cols, arg, is_in_hole(in_hole)),
              idx: row.idx,
            },
            Ctr.NEHole,
          )
        };
      };

      List.fold_left((sm, row) => {submatrix(sm, row, Ctr.Okay)}, empty, m);
    };

    let submatrices = rev(submatrices()); // needed so that rows show up in order for redundancy checking

    let first_col_exhaustive =
      switch (all_ctrs) {
      | Unknown => true
      | Infinite => seen_truth || seen_hole
      | Finite(_) => seen_truth || seen_hole || seen_all_ctrs
      };

    // partially applied function that is returned so the caller
    // can pass in additional information to update the unseen list
    // based on this column
    //
    // this is designed this way to avoid recomputation/returning
    // of seen data
    let cons_unseen_ctr =
      UnseenPatternList.cons_from_type(seen_data, first_col_ty);
    let cons_unseen_type_default =
      UnseenPatternList.cons_default_from_type(first_col_ty);

    {
      ...submatrices,
      first_col_exhaustive,
      first_col_redundant_rows,
      cons_unseen_ctr,
      cons_unseen_type_default,
    };
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type result = {
  is_exhaustive: bool,
  redundant_rows,
  unseen_pattern: UnseenPatternList.t,
};

module type CheckMatrix = {
  let check: (list(Constraint.t), Typ.t) => result;
};

module CheckMatrix: CheckMatrix = {
  let extend_empty_unseen_pat =
      (
        ctr: Ctr.t,
        first_col_exhaustive: bool,
        cons_unseen_ctr: (Ctr.t, UnseenPatternList.t) => UnseenPatternList.t,
        cons_unseen_type_default:
          (Ctr.t, UnseenPatternList.t) => UnseenPatternList.t,
        unseen_pattern: UnseenPatternList.t,
      ) => {
    let unseen_pat_has_holes =
      UnseenPatternList.(
        has_holes(unseen_pattern) || is_empty(unseen_pattern)
      );

    switch (ctr.status) {
    | Okay =>
      if (first_col_exhaustive) {
        cons_unseen_type_default(ctr, UnseenPatternList.empty);
      } else {
        cons_unseen_ctr(ctr, UnseenPatternList.empty);
      }
    | Ctr.InHole when unseen_pat_has_holes => unseen_pattern
    | Ctr.NEHole when unseen_pat_has_holes =>
      UnseenPatternList.cons_wild(Ctr.NEHole, UnseenPatternList.empty)
    | Ctr.NEHole
    | Ctr.InHole
    | Ctr.Unknown => unseen_pattern
    };
  };

  let extend_unseen_pat =
      (
        ctr: Ctr.t,
        first_col_ty: Typ.t,
        is_still_exhaustive: bool,
        first_col_exhaustive: bool,
        is_submatrix_exhaustive: bool,
        submatrix_unseen_pattern: UnseenPatternList.t,
        cons_unseen_ctr: (Ctr.t, UnseenPatternList.t) => UnseenPatternList.t,
        cons_unseen_type_default:
          (Ctr.t, UnseenPatternList.t) => UnseenPatternList.t,
        unseen_pattern: UnseenPatternList.t,
      ) => {
    let submatrix_pat_has_less_errs =
      UnseenPatternList.has_less_errors(
        submatrix_unseen_pattern,
        unseen_pattern,
      )
      || UnseenPatternList.is_empty(unseen_pattern);

    // update the unseen list based on exhaustiveness
    switch (ctr.status) {
    | Ctr.Okay =>
      let new_unseen_pattern =
        if (is_still_exhaustive && !first_col_exhaustive) {
          // if the following column did not break exhaustiveness, but this one does,
          // we place the unseen value into the list
          cons_unseen_ctr(
            ctr,
            submatrix_unseen_pattern,
          );
        } else if (is_still_exhaustive && first_col_exhaustive) {
          // If the following column did not break exhaustiveness,
          // and this one also doesn't, use the default unseen value
          // for the type
          cons_unseen_type_default(
            ctr,
            submatrix_unseen_pattern,
          );
        } else if (!is_submatrix_exhaustive) {
          // otherwise, we just use a default/known to exist ctr
          // from an inexhaustive pattern.
          // This effectively builds a chain of "known" values that
          // are already in a pattern, so we don't have to "make stuff up".
          UnseenPatternList.cons_ctr(
            ctr,
            first_col_ty,
            submatrix_unseen_pattern,
          );
        } else {
          unseen_pattern;
        };

      if (UnseenPatternList.has_holes(new_unseen_pattern)) {
        if (UnseenPatternList.has_less_errors(
              new_unseen_pattern,
              unseen_pattern,
            )
            || first_col_exhaustive) {
          new_unseen_pattern;
        } else {
          unseen_pattern;
        };
      } else {
        new_unseen_pattern;
      };
    | Ctr.InHole when submatrix_pat_has_less_errs => submatrix_unseen_pattern
    | Ctr.NEHole when submatrix_pat_has_less_errs =>
      UnseenPatternList.cons_wild(Ctr.NEHole, submatrix_unseen_pattern)
    | Ctr.NEHole
    | Ctr.InHole
    | Ctr.Unknown => unseen_pattern
    };
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
        let Submatrices.{
          ctrs,
          first_col_exhaustive,
          first_col_redundant_rows,
          cons_unseen_ctr,
          cons_unseen_type_default,
        } =
          Submatrices.of_matrix(m, all_ctrs, first_col_ty);

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
                  extend_empty_unseen_pat(
                    ctr,
                    first_col_exhaustive,
                    cons_unseen_ctr,
                    cons_unseen_type_default,
                    unseen_pattern,
                  );
                (is_exhaustive, redundant_rows, unseen_pattern);
              | _ =>
                let {
                  is_exhaustive: is_submatrix_exhaustive,
                  redundant_rows: submatrix_redundant_rows,
                  unseen_pattern: submatrix_unseen_pattern,
                } =
                  check_matrix(submatrix, col_tys);

                let is_still_exhaustive =
                  is_exhaustive && is_submatrix_exhaustive;

                let unseen_pattern =
                  extend_unseen_pat(
                    ctr,
                    first_col_ty,
                    is_still_exhaustive,
                    first_col_exhaustive,
                    is_submatrix_exhaustive,
                    submatrix_unseen_pattern,
                    cons_unseen_ctr,
                    cons_unseen_type_default,
                    unseen_pattern,
                  );

                let redundant_rows =
                  List.filter(
                    (idx: int) => {
                      !Matrix.contains_row(idx, submatrix)
                      || List.mem(idx, submatrix_redundant_rows)
                    },
                    redundant_rows,
                  );

                (is_still_exhaustive, redundant_rows, unseen_pattern);
              };
            },
            ctrs,
            (
              true, // fold initialized to true regardless of current column so unseen checks work.
              first_col_redundant_rows,
              UnseenPatternList.empty,
            ),
          );

        {
          is_exhaustive: is_exhaustive && first_col_exhaustive,
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
};

let check = CheckMatrix.check;
