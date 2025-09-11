open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type solution =
  | Unknown(Prov.t)
  | Atom(Atom.cls)
  | List(solution)
  | Arrow(solution, solution)
  | Sum(ConstructorMap.t(Typ.t))
  | Prod(list(solution))
  | Label(string)
  | TupLabel(solution, solution)
  | Ap(solution, solution)
  | Rec(TPat.term, solution)
  | Forall(TPat.term, solution)
  | Var(string)
  | Multi(list(solution));

let cyclic_solution: solution = Unknown(Hole(CycleHole) |> Prov.anonymous);

type canonical_constramnot =
  | Con(Prov.t, Typ.term);

module StringProv = {
  [@deriving (show({with_path: false}), sexp, yojson)]
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

module ProvMap = {
  include Map.Make(StringProv);
};

module SolutionMap: {
  include (module type of ProvMap);
  type t = ProvMap.t(solution);
} = {
  include ProvMap;
  type t = ProvMap.t(solution);
};

let is_identified_providence = (p: Prov.t) =>
  IdTagged.rep_id(p) != Id.invalid;

let rec provs_in_typ = (~include_prov=_ => true, t: Typ.term): list(Prov.t) => {
  switch (t) {
  | Unknown(p) when is_identified_providence(p) && include_prov(p) => [p]
  | Unknown(_) => []
  | Atom(_) => []
  | Arrow(t1, t2) =>
    provs_in_typ(~include_prov, t1 |> Typ.term_of)
    @ provs_in_typ(~include_prov, t2 |> Typ.term_of)
  | Prod(args) =>
    List.map(t => provs_in_typ(~include_prov, t |> Typ.term_of), args)
    |> List.flatten
  | Label(_) => []
  | TupLabel(label, arg) =>
    provs_in_typ(~include_prov, label |> Typ.term_of)
    @ provs_in_typ(~include_prov, arg |> Typ.term_of)
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
  | Unknown(p) when is_identified_providence(p) => [p]
  | Unknown(_) => []
  | Atom(_) => []
  | Arrow(t1, t2) => all_provs_in_sol(t1) @ all_provs_in_sol(t2)
  | List(elt) => all_provs_in_sol(elt)
  | Prod(args) => List.concat_map(all_provs_in_sol, args)
  | Label(_) => []
  | Sum(_) => []
  | TupLabel(l, r) => all_provs_in_sol(l) @ all_provs_in_sol(r)
  | Ap(fn, args) => all_provs_in_sol(fn) @ all_provs_in_sol(args)
  | Rec(_, ty) => all_provs_in_sol(ty)
  | Forall(_, ty) => all_provs_in_sol(ty)
  | Var(_) => []
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
  | (Unknown(p), Unknown(q)) =>
    if (is_identified_providence(p) && is_identified_providence(q)) {
      [Con(p, Unknown(q))];
    } else {
      [];
    }
  | (Unknown(p), t) =>
    if (is_identified_providence(p)) {
      [Con(p, t)];
    } else {
      [];
    }
  | (t, Unknown(p)) =>
    if (is_identified_providence(p)) {
      [Con(p, t)];
    } else {
      [];
    }
  | (Arrow(l1, l2), Arrow(r1, r2)) =>
    unfold_constramnot(Con(l1, r1)) @ unfold_constramnot(Con(l2, r2))
  | (Prod(l_args), Prod(r_args)) => unfold_constramnot_prod(l_args, r_args)
  | (Label(_), Label(_)) => []
  | (TupLabel(l_label, l_typ), TupLabel(r_label, r_typ)) =>
    unfold_constramnot(Con(l_label, r_label))
    @ unfold_constramnot(Con(l_typ, r_typ))
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
and unfold_constramnot_prod = (args1, args2): list(canonical_constramnot) =>
  // if both lists do not have identical labels or lengths,
  // we should treat them as two different tuples
  //
  if (List.length(args1) == List.length(args2)) {
    List.fold_left2(
      (acc, t1, t2) => acc @ unfold_constramnot(Con(t1, t2)),
      [],
      args1,
      args2,
    );
  } else {
    [];
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
  let lookup: (StringProv.t, t) => data_elem;
  let lookup_prov: (Prov.t, t) => data_elem;
} = {
  include ProvMap;
  type data = (Prov.t, list(Prov.t), PossibleTypeSet.t);
  type data_elem = UnionFind.elem(data);
  type t = ProvMap.t(data_elem);

  let lookup = (p: StringProv.t, m: t): data_elem => {
    let res = ProvMap.find(p, m);
    res;
  };
  let lookup_prov = (p: Prov.t, m: t): data_elem =>
    lookup(StringProv.of_prov(p), m);
  let lookup_get = (p: Prov.t, m: t): data =>
    UnionFind.get(lookup_prov(p, m));

  let merge_data = ((p, l1, l2): data, (_, l3, l4): data): data => {
    (p, l1 @ l3, PossibleTypeSet.union(l2, l4));
  };

  let update_data = (p: Prov.t, d: data, m: t): unit => {
    let elem_p = lookup_prov(p, m);
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
      let _ =
        UnionFind.merge(merge_data, lookup_prov(p, m), lookup_prov(q, m));
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
  switch (t) {
  | Atom(t) => Atom(t)
  | Unknown(u) => Unknown(u)
  | Sum(s) => Sum(s)
  | Prod(elts) =>
    Prod(List.map(e => solution_of_typ(p, Typ.term_of(e)), elts))
  | Rec(pat, ty) =>
    Rec(pat |> IdTagged.term_of, solution_of_typ(p, ty |> Typ.term_of))
  | Forall(pat, ty) =>
    Forall(pat |> IdTagged.term_of, solution_of_typ(p, ty |> Typ.term_of))
  | List(elt) => List(solution_of_typ(p, elt |> Typ.term_of))
  | Label(s) => Label(s)
  | TupLabel(l, t) =>
    TupLabel(
      solution_of_typ(p, l |> Typ.term_of),
      solution_of_typ(p, t |> Typ.term_of),
    )
  | Ap(fn, arg) =>
    Ap(
      solution_of_typ(p, fn |> Typ.term_of),
      solution_of_typ(p, arg |> Typ.term_of),
    )
  | Var(v) => Var(v)
  | Parens(term) => solution_of_typ(p, term |> Typ.term_of)
  | Arrow(t1, t2) =>
    Arrow(
      solution_of_typ(p, t1 |> Typ.term_of),
      solution_of_typ(p, t2 |> Typ.term_of),
    )
  };
};

// multiholes idk lol???
let rec refine_solution = (p: Prov.t, s: solution, t: Typ.term): solution => {
  switch (s, t) {
  | (Unknown({term: Hole(CycleHole), _}), _) =>
    Multi([
      Unknown(Hole(CycleHole) |> Prov.anonymous),
      solution_of_typ(p, t),
    ])
  | (Unknown(_), t) => solution_of_typ(p, t)
  | (s, Unknown(_)) => s
  | (Atom(a1), Atom(a2)) when a1 == a2 => Atom(a1)
  | (Atom(a1), Atom(a2)) => Multi([Atom(a1), Atom(a2)])
  | (List(l1), List(l2)) => List(refine_solution(p, l1, l2 |> Typ.term_of))
  | (Sum(s1), Sum(s2)) =>
    if (s1 == s2) {
      Sum(s1);
    } else {
      Multi([Sum(s1), Sum(s2)]);
    }
  | (Prod(p1), Prod(p2)) =>
    if (List.length(p1) == List.length(p2)) {
      Prod(
        List.map2(
          (e1, e2) => refine_solution(p, e1, e2 |> Typ.term_of),
          p1,
          p2,
        ),
      );
    } else {
      Multi([
        Prod(p1),
        Prod(List.map(e => solution_of_typ(p, e |> Typ.term_of), p2)),
      ]);
    }
  | (Label(s1), Label(s2)) =>
    if (s1 == s2) {
      Label(s1);
    } else {
      Multi([Label(s1), Label(s2)]);
    }
  | (TupLabel(l1, r1), TupLabel(l2, r2)) =>
    TupLabel(
      refine_solution(p, l1, l2 |> Typ.term_of),
      refine_solution(p, r1, r2 |> Typ.term_of),
    )
  | (Ap(fn1, arg1), Ap(fn2, arg2)) =>
    Ap(
      refine_solution(p, fn1, fn2 |> Typ.term_of),
      refine_solution(p, arg1, arg2 |> Typ.term_of),
    )
  | (Rec(pat1, ty1), Rec(pat2, ty2)) =>
    if (pat1 == (pat2 |> IdTagged.term_of)) {
      Rec(pat1, refine_solution(p, ty1, ty2 |> Typ.term_of));
    } else {
      Multi([
        Rec(pat1, ty1),
        Rec(
          pat2 |> IdTagged.term_of,
          solution_of_typ(p, ty2 |> Typ.term_of),
        ),
      ]);
    }
  | (Forall(pat1, ty1), Forall(pat2, ty2)) =>
    if (pat1 == (pat2 |> IdTagged.term_of)) {
      Forall(pat1, refine_solution(p, ty1, ty2 |> Typ.term_of));
    } else {
      Multi([
        Forall(pat1, ty1),
        Forall(
          pat2 |> IdTagged.term_of,
          solution_of_typ(p, ty2 |> Typ.term_of),
        ),
      ]);
    }
  // | (Num, Arrow(_)) => Multi([Num, solution_of_typ(p, t)])
  // | (Bool, Arrow(_)) => Multi([Bool, solution_of_typ(p, t)])
  // | (Arrow(s1, s2), Num)
  // | (Arrow(s1, s2), Bool) => Multi([Num, Arrow(s1, s2)])
  | (Arrow(s1, s2), Arrow(t1, t2)) =>
    Arrow(
      refine_solution(p, s1, t1 |> Typ.term_of),
      refine_solution(p, s2, t2 |> Typ.term_of),
    )
  | (Multi(ss), t) => Multi(ss @ [solution_of_typ(p, t)]) // TODO: compress possibilities
  | (Atom(_) as s, t)
  | (List(_) as s, t)
  | (Label(_) as s, t)
  | (TupLabel(_, _) as s, t)
  | (Ap(_, _) as s, t)
  | (Rec(_, _) as s, t)
  | (Arrow(_, _) as s, t)
  | (Prod(_) as s, t)
  | (Sum(_) as s, t)
  | (Var(_) as s, t)
  | (Forall(_, _) as s, t) => Multi([s, solution_of_typ(p, t)])
  // | (Multi([]), _)
  // | (Multi([Hole, ..._]), _)
  // | (Multi([Multi(_), ..._]), _)
  // | (Multi([Cyclic, ..._]), _) => failwith("impossible")
  // | (Multi([Num, ...ss]), Num) => Multi([Num, ...ss])
  // | (Multi([Arrow(s1, s2), ...ss]), Num) =>
  //   Multi([Num, Arrow(s1, s2), ...ss])
  // | (Multi([Num, ...ss]), Arrow(t1, t2)) => Multi(todo)
  // | (Multi(ss), Arrow(t1, t2)) => Multi(todo)
  };
};

let solve_prov = (p: Prov.t, m: PossibleProvTypesMap.t): solution => {
  let (_, _, ts) =
    UnionFind.get(PossibleProvTypesMap.find(StringProv.of_prov(p), m));
  let ts_list = PossibleTypeSet.to_list(ts);
  List.fold_left(
    refine_solution(p),
    Unknown(Hole(EmptyHole) |> Prov.anonymous),
    ts_list,
  );
};

let rec typ_of_solution = (s: solution): Typ.term => {
  switch (s) {
  | Unknown(p) => Unknown(p)
  | Atom(a) => Atom(a)
  | Arrow(s1, s2) =>
    Arrow(typ_of_solution(s1) |> Typ.temp, typ_of_solution(s2) |> Typ.temp)
  | Multi(_) => Unknown(Hole(EmptyHole) |> Prov.anonymous)
  | List(elt) => List(typ_of_solution(elt) |> Typ.temp)
  | Sum(sm) => Sum(sm)
  | Prod(elts) => Prod(List.map(e => typ_of_solution(e) |> Typ.temp, elts))
  | Label(l) => Label(l)
  | TupLabel(label, ty) =>
    TupLabel(
      typ_of_solution(label) |> Typ.temp,
      typ_of_solution(ty) |> Typ.temp,
    )
  | Ap(fn, args) =>
    Ap(typ_of_solution(fn) |> Typ.temp, typ_of_solution(args) |> Typ.temp)
  | Rec(pat, ty) =>
    Rec(pat |> IdTagged.temp, typ_of_solution(ty) |> Typ.temp)
  | Forall(pat, ty) =>
    Forall(pat |> IdTagged.temp, typ_of_solution(ty) |> Typ.temp)
  | Var(v) => Var(v)
  };
};

let solution_typ = (s: solution): Typ.term => {
  switch (s) {
  | Unknown(_)
  | Multi(_) => Unknown(Hole(EmptyHole) |> Prov.anonymous)
  | Atom(_)
  | Sum(_)
  | List(_)
  | Prod(_)
  | Var(_)
  | Label(_)
  | TupLabel(_, _)
  | Ap(_, _)
  | Rec(_, _)
  | Forall(_, _)
  | Arrow(_) => typ_of_solution(s)
  };
};

// let string_of_constramnot = (Con(t1, t2): constramnot): string => {
//   string_of_htyp(t1) ++ "~" ++ string_of_htyp(t2);
// };

// let string_of_constramnots = (cs: list(constramnot)): string => {
//   "{" ++ String.concat("\n", List.map(string_of_constramnot, cs)) ++ "}";
// };

let string_of_data = ((_, ps, ts): PossibleProvTypesMap.data): string =>
  "["
  ++ String.concat(
       ", ",
       List.map(p => StringProv.of_prov(p) |> StringProv.show, ps),
     )
  ++ "] | ["
  ++ String.concat(
       ", ",
       List.map(TermBase.show_typ_term, PossibleTypeSet.to_list(ts)),
     )
  ++ "]";

let string_of_prov_map = (m: PossibleProvTypesMap.t): string => {
  let f: ((StringProv.t, PossibleProvTypesMap.data_elem)) => string =
    ((p, d)) =>
      StringProv.show(p) ++ ": " ++ string_of_data(UnionFind.get(d));
  let l: list((StringProv.t, PossibleProvTypesMap.data_elem)) =
    ProvMap.bindings(m);
  "{" ++ String.concat("\n", List.map(f, l)) ++ "}";
};

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
  switch (typ) {
  | Unknown(q) when prov == StringProv.of_prov(q) => sol_typ
  // | Hole(q) => Hole(q)
  | Unknown(_) as u => u
  | Atom(_) as atom => atom
  | List(t) =>
    List(
      solution_typ_replace_typ(prov, t |> Typ.term_of, sol_typ, m) |> Typ.temp,
    )
  | Ap(fn, args) =>
    Ap(
      solution_typ_replace_typ(prov, fn |> Typ.term_of, sol_typ, m)
      |> Typ.temp,
      solution_typ_replace_typ(prov, args |> Typ.term_of, sol_typ, m)
      |> Typ.temp,
    )
  | Forall(pat, body) =>
    Forall(
      pat,
      solution_typ_replace_typ(prov, body |> Typ.term_of, sol_typ, m)
      |> Typ.temp,
    )
  | Sum(_) as sum => sum
  | Var(_) as var => var
  | Prod(args) =>
    Prod(
      List.map(
        arg =>
          solution_typ_replace_typ(prov, arg |> Typ.term_of, sol_typ, m)
          |> Typ.temp,
        args,
      ),
    )
  | Label(_) as label => label
  | TupLabel(label, ty) =>
    TupLabel(
      solution_typ_replace_typ(prov, label |> Typ.term_of, sol_typ, m)
      |> Typ.temp,
      solution_typ_replace_typ(prov, ty |> Typ.term_of, sol_typ, m)
      |> Typ.temp,
    )
  | Parens(term) =>
    solution_typ_replace_typ(prov, term |> Typ.term_of, sol_typ, m)
  | Rec(pat, body) =>
    Rec(
      pat,
      solution_typ_replace_typ(prov, body |> Typ.term_of, sol_typ, m)
      |> Typ.temp,
    )
  | Arrow(t1, t2) =>
    Arrow(
      solution_typ_replace_typ(prov, t1 |> Typ.term_of, sol_typ, m)
      |> Typ.temp,
      solution_typ_replace_typ(prov, t2 |> Typ.term_of, sol_typ, m)
      |> Typ.temp,
    )
  };
};

let rec solution_replace_solution =
        (prov: StringProv.t, sol: solution, sol': solution): (solution, bool) => {
  let fold_solutions =
    List.fold_left(
      ((sols, changed), sol) => {
        let (sol', c) = solution_replace_solution(prov, sol, sol');
        ([sol', ...sols], c || changed);
      },
      ([], false),
    );

  switch (sol) {
  | Unknown({term: Hole(CycleHole), _}) => (sol, false)
  | Unknown(q) when prov == StringProv.of_prov(q) => (sol', true)
  | Unknown(_) => (sol, false)
  | Prod(ss) =>
    let (ss', changed) = fold_solutions(ss);
    (Prod(List.rev(ss')), changed);
  | Multi(ss) =>
    let (ss', changed) = fold_solutions(ss);
    (Multi(List.rev(ss')), changed);
  | Atom(_) => (sol, false)
  | Sum(_) => (sol, false)
  | Var(_) => (sol, false)
  | Label(_) => (sol, false)
  | TupLabel(label, body) =>
    let (label', changed1) = solution_replace_solution(prov, label, sol');
    let (body', changed2) = solution_replace_solution(prov, body, sol');
    (TupLabel(label', body'), changed1 || changed2);
  | Ap(fn, args) =>
    let (fn', fn_changed) = solution_replace_solution(prov, fn, sol');
    let (args', args_changed) = solution_replace_solution(prov, args, sol');
    (Ap(fn', args'), fn_changed || args_changed);
  | Rec(pat, body) =>
    let (body', changed) = solution_replace_solution(prov, body, sol');
    (Rec(pat, body'), changed);
  | Forall(pat, body) =>
    let (body', changed) = solution_replace_solution(prov, body, sol');
    (Forall(pat, body'), changed);
  | List(t) =>
    let (t', changed) = solution_replace_solution(prov, t, sol');
    (List(t'), changed);
  | Arrow(s1, s2) =>
    let (s1', changed1) = solution_replace_solution(prov, s1, sol');
    let (s2', changed2) = solution_replace_solution(prov, s2, sol');
    (Arrow(s1', s2'), changed1 || changed2);
  };
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
  print_endline(string_of_prov_map(m));
  switch (PossibleProvTypesMap.find_dominant_provs(m)) {
  // if you find a dominant provenance...
  | ([], _) => None
  | ([p, ..._], is_cyclic) =>
    Some(
      {
        print_endline(
          "Solving: " ++ (StringProv.of_prov(p) |> StringProv.show),
        );
        let s = solve_prov(p, m); // solve it
        print_endline("Solution: " ++ show_solution(s));
        let equiv_provs: list(StringProv.t) =
          List.filter_map(
            ((p', _)) =>
              if (UnionFind.eq(
                    PossibleProvTypesMap.lookup_prov(p, m),
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
        print_endline(
          "Equivalent provs: "
          ++ String.concat(",", List.map(StringProv.show, equiv_provs)),
        );

        let cyclic_provs' =
          if (is_cyclic) {
            List.append(cyclic_provs, equiv_provs);
          } else {
            cyclic_provs;
          };

        let st = solution_typ(s); // turn it into a type

        let cs' =
          List.fold_left(
            (cs_acc, pss) => {solution_typ_replace_cons(pss, cs_acc, st, m)},
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
