open Ppx_compare_lib.Builtin;
open Sexplib.Std;

// [@deriving (show({with_path: false}), sexp, yojson)]
type solution =
  | Typ(Typ.term)
  | Multi(list(solution));

let cyclic_solution = Typ(Unknown(Hole(CycleHole) |> Prov.anonymous));

type canonical_constramnot =
  | Con(Prov.t, Typ.term);

module StringProv = {
  type t = (string, Id.t);
  let compare = ((k1, id1), (k2, id2)) => {
    let id_compare = Id.compare(id1, id2);
    if (id_compare != 0) {
      id_compare;
    } else {
      String.compare(k1, k2);
    };
  };

  let of_prov = (p: Prov.t): t => (
    Prov.to_string(Prov.term_of(p)),
    IdTagged.rep_id(p),
  );
};

module ProvMap = Map.Make(StringProv);
module SolutionMap: {
  include (module type of ProvMap);
  type t = ProvMap.t(solution);
} = {
  include ProvMap;
  type t = ProvMap.t(solution);
};

let rec provs_in_typ = (~include_prov=_ => true, t: Typ.term): list(Prov.t) => {
  switch (t) {
  | Unknown(p) when include_prov(p) => [p]
  | Unknown(_) => []
  | Atom(_) => []
  | Arrow(t1, t2) =>
    provs_in_typ(~include_prov, t1 |> Typ.term_of)
    @ provs_in_typ(~include_prov, t2 |> Typ.term_of)
  | Prod(args) =>
    List.map(t => provs_in_typ(~include_prov, t |> Typ.term_of), args)
    |> List.flatten
  | Label(_) => []
  | TupLabel(_, arg) => provs_in_typ(~include_prov, arg |> Typ.term_of)
  | List(elt) => provs_in_typ(~include_prov, elt |> Typ.term_of)
  | Sum(_) => []
  | Parens(term) => provs_in_typ(~include_prov, term |> Typ.term_of)
  | Ap(fn, args) =>
    provs_in_typ(~include_prov, fn |> Typ.term_of)
    @ provs_in_typ(~include_prov, args |> Typ.term_of)
  | Rec(_, ty) => provs_in_typ(~include_prov, ty |> Typ.term_of)
  | Forall(_, ty) => provs_in_typ(~include_prov, ty |> Typ.term_of)
  | Var(_) => []
  };
};

let unsolved_provs_in_typ = (t: Typ.term, sm: SolutionMap.t) => {
  let filter = (p: Prov.t) => !SolutionMap.mem(StringProv.of_prov(p), sm);
  provs_in_typ(t, ~include_prov=filter);
};

let rec all_provs_in_sol = (s: solution): list(Prov.t) => {
  switch (s) {
  | Typ(typ) => provs_in_typ(typ)
  | Multi(ss) => List.concat_map(all_provs_in_sol, ss)
  };
};

let terms_of_equiv = (equiv: Typ.equivalence) => {
  let Con(leftType, rightType) = equiv;
  (leftType |> Typ.term_of, rightType |> Typ.term_of);
};

// precondition: recieves a consistent constramnot
// postondition: returns an equivalent list of canonical (left side is hole) constriants
let rec unfold_constramnot =
        (equiv: Typ.equivalence): list(canonical_constramnot) => {
  let Con(left_equiv, right_equiv) = equiv;

  switch (terms_of_equiv(equiv)) {
  | (Parens(paren_ty), _) => unfold_constramnot(Con(paren_ty, right_equiv))
  | (_, Parens(paren_ty)) => unfold_constramnot(Con(left_equiv, paren_ty))
  // | (Unknown({term: Hole(EmptyHole), _}), _) => []
  // | (_, Unknown({term: Hole(EmptyHole), _})) => []
  | (Unknown(p), Unknown({term: Join(j1, j2), _})) => [
      Con(p, Unknown(j1)),
      Con(p, Unknown(j2)),
    ]
  | (Unknown({term: Join(j1, j2), _}), Unknown(p)) => [
      Con(p, Unknown(j1)),
      Con(p, Unknown(j2)),
    ]
  | (Unknown(p), t) => [Con(p, t)]
  | (t, Unknown(p)) => [Con(p, t)]
  | (Arrow(l1, l2), Arrow(r1, r2)) =>
    unfold_constramnot(Con(l1, r1)) @ unfold_constramnot(Con(l2, r2))
  | (Prod(l_args), Prod(r_args)) =>
    unfold_constramnot_prod(l_args, r_args, [])
  | (Label(_), Label(_)) => []
  | (TupLabel(_, l_typ), TupLabel(_, r_typ)) =>
    unfold_constramnot(Con(l_typ, r_typ)) // TODO: (THI) might we want to constrain labels?
  | (Ap(l_fun, l_args), Ap(r_fun, r_args)) =>
    unfold_constramnot(Con(l_fun, r_fun))
    @ unfold_constramnot(Con(l_args, r_args))
  | (Atom(_), Atom(_)) => []
  | (Sum(_), Sum(_)) => []
  | (List(l), List(r)) => unfold_constramnot(Con(l, r))
  | (Var(_), Var(_)) => []
  | (Rec(_, l_ty), Rec(_, r_ty)) => unfold_constramnot(Con(l_ty, r_ty))
  | (Forall(_, l_ty), Forall(_, r_ty)) =>
    unfold_constramnot(Con(l_ty, r_ty))
  | (Atom(_), _)
  | (_, Atom(_)) => []
  | (Arrow(_), _)
  | (_, Arrow(_)) => []
  | (Var(_), _)
  | (_, Var(_)) => []
  | (Prod(_), _)
  | (_, Prod(_)) => []
  | (Label(_), _)
  | (_, Label(_)) => []
  | (TupLabel(_), _)
  | (_, TupLabel(_)) => []
  | (Ap(_), _)
  | (_, Ap(_)) => []
  | (Sum(_), _)
  | (_, Sum(_)) => []
  | (List(_), _)
  | (_, List(_)) => []
  | (Rec(_), _)
  | (_, Rec(_)) => []
  // | (Forall(_), _) | (_, Forall(_)) => []
  };
}
and unfold_constramnot_prod = (args1, args2, acc) => {
  switch (args1, args2) {
  | ([l_hd, ...l_tl], [r_hd, ...r_tl]) =>
    unfold_constramnot_prod(
      l_tl,
      r_tl,
      List.append(acc, unfold_constramnot(Con(l_hd, r_hd))),
    )
  | _ => acc
  };
};

let unfold_constramnots: list(Typ.equivalence) => list(canonical_constramnot) =
  List.concat_map(unfold_constramnot);

// let rec provs_in_constramnots: list(canonical_constramnot) => list(Prov.t) =
//   fun
//   | [] => []
//   | [(p, t), ...tl] => [p] @ provs_in_typ(t) @ provs_in_constramnots(tl);

// let uniq_provs: list(Prov.t) => list(Prov.t) =
//   List.sort_uniq((p1, p2) =>
//     String.compare(string_of_prov(p1), string_of_prov(p2))
//   );

// module PossibleType = {
//   type t = (Htyp.t, String.t)

//   let compare = ((_, s1): t, (_, s2): t): int => {
//     String.compare(s1, s2)
//   };
// };

// TODO: this needs to be a proper set to get rid of duplicate types
// Temp fix just prevent duplicaste insertion
module PossibleTypeSet: {
  type t = list(Typ.term);
  let union: (t, t) => t;
  let empty: t;
  let singleton: Typ.term => t;
  let to_list: t => t;
  let add: (Typ.term, t) => t;
} = {
  type t = list(Typ.term);

  let set_contains = (x: Typ.term, ts: t) =>
    List.exists((y: Typ.term) => Typ.equal(Typ.temp(y), Typ.temp(x)), ts);

  let add = (x: Typ.term, ts: t) => set_contains(x, ts) ? ts : [x, ...ts];

  // Fold for dedup
  let union = (a, b) => List.fold_left((acc, t) => add(t, acc), a, b);
  let empty = [];
  let singleton = (t: Typ.term): t => [t];
  let to_list = (t: t) => t;
};

module PossibleProvTypesMap: {
  include (module type of ProvMap);
  type data = (Prov.t, list(Prov.t), PossibleTypeSet.t);
  type data_elem = UnionFind.elem(data);
  type t = ProvMap.t(data_elem);

  let of_constramnots: (list(canonical_constramnot), SolutionMap.t) => t;
  let find_dominant_provs: t => (list(Prov.t), bool);
  let lookup: (Prov.t, t) => data_elem;
} = {
  include ProvMap;
  type data = (Prov.t, list(Prov.t), PossibleTypeSet.t);
  type data_elem = UnionFind.elem(data);
  type t = ProvMap.t(data_elem);

  let lookup = (p: Prov.t, m: t): data_elem =>
    ProvMap.find(StringProv.of_prov(p), m);
  let lookup_get = (p: Prov.t, m: t): data => UnionFind.get(lookup(p, m));

  let merge_data = ((p, l1, l2): data, (_, l3, l4): data): data => {
    (p, l1 @ l3, PossibleTypeSet.union(l2, l4));
  };

  let update_data = (p: Prov.t, d: data, m: t): unit => {
    let elem_p = lookup(p, m);
    UnionFind.set(elem_p, merge_data(UnionFind.get(elem_p), d));
  };

  let add_if_absent = (p: Prov.t, m: t): t =>
    if (!ProvMap.mem(StringProv.of_prov(p), m)) {
      ProvMap.add(
        StringProv.of_prov(p),
        UnionFind.make((p, [], PossibleTypeSet.empty)),
        m,
      );
    } else {
      m;
    };

  let update_prov_map_of_constramnot =
      (c: canonical_constramnot, m: t, sm: SolutionMap.t): t => {
    switch (c) {
    | Con(p, Unknown(q))
        when
          !(
            SolutionMap.mem(StringProv.of_prov(p), sm)
            || SolutionMap.mem(StringProv.of_prov(q), sm)
          ) =>
      let m = add_if_absent(p, m);
      let m = add_if_absent(q, m);
      let _ = UnionFind.merge(merge_data, lookup(p, m), lookup(q, m));
      m;
    | Con(p, t) when !SolutionMap.mem(StringProv.of_prov(p), sm) =>
      let m = add_if_absent(p, m);
      let qs = unsolved_provs_in_typ(t, sm);
      let m = List.fold_left((m, q) => add_if_absent(q, m), m, qs);

      List.iter(
        q => {
          update_data(
            q,
            (Internal |> Prov.anonymous, [q], PossibleTypeSet.empty),
            m,
          )
        },
        qs,
      );
      update_data(
        p,
        (Internal |> Prov.anonymous, [], PossibleTypeSet.singleton(t)),
        m,
      );
      m;
    | _ => m
    };
  };

  let of_constramnots =
      (cs: list(canonical_constramnot), sm: SolutionMap.t): t => {
    List.fold_left(
      (m, c) => update_prov_map_of_constramnot(c, m, sm),
      ProvMap.empty,
      cs,
    );
  };

  let find_dominant_provs = (m: t): (list(Prov.t), bool) => {
    let dom =
      List.filter_map(
        ((_, d)) => {
          let (p, qs, _) = UnionFind.get(d);
          if (List.is_empty(qs)) {
            Some(p);
          } else {
            None;
          };
        },
        ProvMap.bindings(m),
      );

    if (List.is_empty(dom)) {
      switch (ProvMap.bindings(m)) {
      | [] => ([], true)
      | [(_, d), ..._] =>
        let (p, _, _) = UnionFind.get(d);
        ([p], true);
      };
    } else {
      (dom, false);
    };
  };
};

let rec solution_of_typ = (p: Prov.t, t: Typ.term) => {
  // switch (t) {
  // | EHole => EHole
  // | CycleHole(_) => Cyclic
  // | Hole(q) => Hole(q)
  // | Num => Num
  // | Bool => Bool
  // | Arrow(t1, t2) => Arrow(solution_of_typ(p, t1), solution_of_typ(p, t2))
  // };
  failwith(
    "unimplmented: solution_of_typ",
  );
};

let rec refine_solution = (p: Prov.t, s: solution, t: Typ.term): solution => {
  // switch (s, t) {
  // | (EHole, t) => solution_of_typ(p, t)
  // | (Hole(_), t) => solution_of_typ(p, t)
  // | (s, Hole(_)) => s
  // | (s, EHole) => s
  // | (s, CycleHole(_)) => s
  // | (Num, Num) => Num
  // | (Bool, Bool) => Bool
  // | (Num, Bool)
  // | (Bool, Num) => Multi([Num, Bool])
  // | (Num, Arrow(_)) => Multi([Num, solution_of_typ(p, t)])
  // | (Bool, Arrow(_)) => Multi([Bool, solution_of_typ(p, t)])
  // | (Arrow(s1, s2), Num)
  // | (Arrow(s1, s2), Bool) => Multi([Num, Arrow(s1, s2)])
  // | (Arrow(s1, s2), Arrow(t1, t2)) =>
  //   Arrow(refine_solution(p, s1, t1), refine_solution(p, s2, t2))
  // | (Multi(ss), t) => Multi(ss @ [solution_of_typ(p, t)]) // TODO: compress possibilities
  // // | (Multi([]), _)
  // // | (Multi([Hole, ..._]), _)
  // // | (Multi([Multi(_), ..._]), _)
  // // | (Multi([Cyclic, ..._]), _) => failwith("impossible")
  // // | (Multi([Num, ...ss]), Num) => Multi([Num, ...ss])
  // // | (Multi([Arrow(s1, s2), ...ss]), Num) =>
  // //   Multi([Num, Arrow(s1, s2), ...ss])
  // // | (Multi([Num, ...ss]), Arrow(t1, t2)) => Multi(todo)
  // // | (Multi(ss), Arrow(t1, t2)) => Multi(todo)
  // | (Cyclic, _) => Multi([Cyclic, solution_of_typ(p, t)])
  // };
  failwith(
    "unimplmented: refine_solution",
  );
};

let solve_prov = (p: Prov.t, m: PossibleProvTypesMap.t): solution => {
  let (_, _, ts) =
    UnionFind.get(PossibleProvTypesMap.find(StringProv.of_prov(p), m));
  let ts_list = PossibleTypeSet.to_list(ts);
  // print_endline(
  //   string_of_prov(p)
  //   ++ "  constrained to "
  //   ++ String.concat(",", List.map(string_of_htyp, ts_list)),
  // );
  List.fold_left(
    refine_solution(p),
    Typ(Unknown(Hole(EmptyHole) |> Prov.anonymous)),
    ts_list,
  );
};

let rec typ_of_solution = (s: solution): Typ.term => {
  failwith(
    "unimplemented: typ_of_solution",
    // switch (s) {
    // | EHole => EHole
    // | Hole(p) => Hole(p)
    // | Num => Num
    // | Bool => Bool
    // | Arrow(s1, s2) => Arrow(typ_of_solution(s1), typ_of_solution(s2))
    // | Multi(_) => EHole
    // | Cyclic => CycleHole(Syn(-1))
    // };
  );
};

let solution_typ = (s: solution): Typ.t => {
  failwith(
    "unimplemented: solution_typ",
    // switch (s) {
    // | EHole => EHole
    // | Hole(_) => EHole
    // | Multi(_) => EHole
    // | Cyclic => EHole
    // | Num
    // | Bool
    // | Arrow(_) => typ_of_solution(s)
    // };
  );
};

// let string_of_constramnot = (Con(t1, t2): constramnot): string => {
//   string_of_htyp(t1) ++ "~" ++ string_of_htyp(t2);
// };

// let string_of_constramnots = (cs: list(constramnot)): string => {
//   "{" ++ String.concat("\n", List.map(string_of_constramnot, cs)) ++ "}";
// };

// let string_of_data = ((_, ps, ts): data): string =>
//   "["
//   ++ String.concat(", ", List.map(string_of_prov, ps))
//   ++ "] | ["
//   ++ String.concat(
//        ", ",
//        List.map(string_of_htyp, PossibleTypeSet.to_list(ts)),
//      )
//   ++ "]";

// let string_of_prov_map = (m: prov_map): string => {
//   let f: ((string, data_elem)) => string =
//     ((p, d)) => p ++ ": " ++ string_of_data(UnionFind.get(d));
//   let l: list((string, data_elem)) = StringMap.bindings(m);
//   "{" ++ String.concat("\n", List.map(f, l)) ++ "}";
// };

// let rec string_of_solution =
//   fun
//   | EHole => "?"
//   | Hole(p) => "?{" ++ string_of_prov(p) ++ "}"
//   | Num => "Num"
//   | Bool => "Bool"
//   | Arrow(s1, s2) =>
//     "(" ++ string_of_solution(s1) ++ "->" ++ string_of_solution(s2) ++ ")"
//   | Multi(ss) =>
//     "{" ++ String.concat("|", List.map(string_of_solution, ss)) ++ "}"
//   | Cyclic => "{Cyclic}";

// let string_of_sol_map = (m: sol_map): string => {
//   let f: ((string, solution)) => string =
//     ((p, d)) => p ++ ": " ++ string_of_solution(d);
//   let l: list((string, solution)) = StringMap.bindings(m);
//   "{" ++ String.concat("\n", List.map(f, l)) ++ "}";
// };

let rec solution_typ_replace_typ =
        (
          prov: StringProv.t,
          typ: Typ.term,
          sol_typ: Typ.term,
          m: PossibleProvTypesMap.t,
        )
        : Typ.term => {
  failwith(
    "unimplemented: solution_typ_replace_typ",
    // switch (t) {
    // | Hole(q) when UnionFind.eq(lookup(p, m), lookup(q, m)) => st
    // | Hole(q) when p == string_of_prov(q) => sol_typ
    // // | Hole(q) => Hole(q)
    // | Hole(Surface(u)) => Hole(Surface(u))
    // | Hole(Syn(u)) => Hole(Syn(u))
    // | Hole(LArrow(q)) => Hole(LArrow(q))
    // | Hole(RArrow(q)) => Hole(RArrow(q))
    // | EHole => EHole
    // | CycleHole(p) => CycleHole(p)
    // | Num => Num
    // | Bool => Bool
    // | Arrow(t1, t2) =>
    //   Arrow(
    //     solution_typ_replace_typ(p, t1, st, m),
    //     solution_typ_replace_typ(p, t2, st, m),
    //   )
    // };
  );
};

let rec solution_replace_solution =
        (prov: StringProv.t, sol: solution, sol': solution): (solution, bool) => {
  // switch (s) {
  // | Hole(q) when p == string_of_prov(q) => (s', true)
  // | Hole(_) => (s, false)
  // | Cyclic => (s, false)
  // | Multi(ss) =>
  //   let (ss', changed) =
  //     List.fold_left(
  //       ((sols, changed), s) => {
  //         let (s', c) = solution_replace_solution(p, s, s');
  //         ([s', ...sols], c || changed);
  //       },
  //       ([], false),
  //       ss,
  //     );
  //   (Multi(List.rev(ss')), changed);
  // | EHole => (s, false)
  // | Num => (Num, false)
  // | Bool => (Bool, false)
  // | Arrow(s1, s2) =>
  //   let (s1', changed1) = solution_replace_solution(p, s1, s');
  //   let (s2', changed2) = solution_replace_solution(p, s2, s');
  //   (Arrow(s1', s2'), changed1 || changed2);
  // };
  failwith(
    "unimplemented: solution replace solution",
  );
};

let solution_typ_replace_con =
    (
      prov_str: StringProv.t,
      Con(t1, t2): Typ.equivalence,
      sol_typ: Typ.term,
      m: PossibleProvTypesMap.t,
    )
    : Typ.equivalence => {
  Con(
    solution_typ_replace_typ(prov_str, t1 |> Typ.term_of, sol_typ, m)
    |> Typ.temp,
    solution_typ_replace_typ(prov_str, t2 |> Typ.term_of, sol_typ, m)
    |> Typ.temp,
  );
};

let solution_typ_replace_cons =
    (
      p: StringProv.t,
      cs: list(Typ.equivalence),
      sol_typ: Typ.term,
      m: PossibleProvTypesMap.t,
    )
    : list(Typ.equivalence) =>
  List.map(c => solution_typ_replace_con(p, c, sol_typ, m), cs);

let extend_sol_map =
    (
      cs: list(Typ.equivalence),
      sm: SolutionMap.t,
      cyclic_provs: list(StringProv.t),
    )
    : option((list(Typ.equivalence), SolutionMap.t, list(StringProv.t))) => {
  // print_endline("Constraints:");
  // print_endline(string_of_constramnots(cs));
  let canonical_cs = unfold_constramnots(cs); // make constraints canonical
  let m = PossibleProvTypesMap.of_constramnots(canonical_cs, sm); // compute provenance map
  // print_endline("Provenance Map:");
  // print_endline(string_of_prov_map(m));
  switch (PossibleProvTypesMap.find_dominant_provs(m)) {
  // if you find a dominant provenance...
  | ([], _) => None
  | ([p, ..._], is_cyclic) =>
    Some(
      {
        // print_endline("Solving: " ++ string_of_prov(p));
        let s = solve_prov(p, m); // solve it
        // print_endline("Solution: " ++ string_of_solution(s));
        let equiv_provs: list(StringProv.t) =
          List.filter_map(
            ((p', _)) =>
              if (UnionFind.eq(
                    PossibleProvTypesMap.lookup(p, m),
                    PossibleProvTypesMap.find(p', m),
                  )) {
                Some
                  (p');
                  // let (canonical_p, _, _) = UnionFind.get(p_elem);
                  // Some(canonical_p);
              } else {
                None;
              },
            PossibleProvTypesMap.bindings(m),
          );
        // print_endline(
        //   "Equivalent provs: " ++ String.concat(",", equiv_provs),
        // );

        let cyclic_provs' =
          if (is_cyclic) {
            List.append(cyclic_provs, equiv_provs);
          } else {
            cyclic_provs;
          };

        let st = solution_typ(s); // turn it into a type

        let cs' =
          List.fold_left(
            (cs_acc, pss) => {
              solution_typ_replace_cons(pss, cs_acc, st |> Typ.term_of, m)
            },
            cs,
            equiv_provs,
          ); // replace it with the solution type in constraints

        // let sm' = solution_typ_replace_sol_map(...)
        let sm' =
          List.fold_left(
            (sm_acc, pss) => SolutionMap.add(pss, s, sm_acc),
            sm,
            equiv_provs,
          ); // and extend the solution map

        let all_provs_in_sol =
          List.map(StringProv.of_prov, all_provs_in_sol(s));
        let sm'' =
          List.fold_left(
            (sm_acc, pss) => {
              let cyclic = List.mem(pss, all_provs_in_sol);
              SolutionMap.map(
                sol => {
                  let (s', replaced) =
                    solution_replace_solution(pss, sol, s);
                  if (replaced && cyclic) {
                    s;
                  } else {
                    s';
                  };
                },
                sm_acc,
              );
            },
            sm',
            equiv_provs,
          ); // and replace it with the solution in the existing solutions

        (cs', sm'', cyclic_provs');
      },
    )
  };
};

let rec solve_rec =
        (
          cs: list(Typ.equivalence),
          sm: SolutionMap.t,
          cyclic_provs: list(StringProv.t),
        )
        : SolutionMap.t => {
  switch (extend_sol_map(cs, sm, cyclic_provs)) {
  | None =>
    print_endline("No dominant provenances");
    // relax solution to have no cycles
    let sm' =
      List.fold_left(
        (sm, pss) => {
          PossibleProvTypesMap.map(
            sol => {
              let (sol, _) =
                solution_replace_solution(pss, sol, cyclic_solution);
              sol;
            },
            sm,
          )
        },
        sm,
        cyclic_provs,
      );
    // print_endline(string_of_constramnots(cs));
    sm';
  | Some((cs', sm', cyclic_provs')) => solve_rec(cs', sm', cyclic_provs')
  };
};

let solve = (cs: list(Typ.equivalence)): SolutionMap.t => {
  print_endline("SOLVING");
  solve_rec(cs, SolutionMap.empty, []);
};

let go = (cs: list(Typ.equivalence)): SolutionMap.t => {
  solve(
    cs,
    // print_endline(string_of_sol_map(sm));
    // let cs = unfold_constramnots(cs);
    // let m = prov_map_of_constramnots(cs);
    // print_endline("go2");
    // print_endline(string_of_int(List.length(StringMap.to_list(m))));
    // print_endline(string_of_prov_map(m));
  );
};
