open Util;

// TODO:
// * I just kind aput stuff for the prod types, they probably need actual thought

let is_identified_provenance = (p: Prov.t) =>
  IdTagged.rep_id(p) != Id.invalid;

// computes the cartesian product of a list of lists
let rec cartesian_product = (lists: list(list(Typ.t))): list(list(Typ.t)) =>
  switch (lists) {
  | [] => [[]]
  | [hd, ...tl] =>
    let tl_product = cartesian_product(tl);
    List.concat_map(h => List.map(t => [h, ...t], tl_product), hd);
  };

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

module Solution = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Unknown(Prov.t)
    | Atom(Atom.cls)
    | List(t)
    | Arrow(t, t)
    | Sum(ConstructorMap.t(Typ.t))
    | Prod(list(t))
    | Label(string)
    | TupLabel(t, t)
    | Rec(TPat.t, t)
    | Poly(TPat.t, t)
    | Var(string)
    | ExplicitNonlabel
    | ProofOf(Exp.t)
    | ProdProjection(t, t)
    | ProdExtension(t, t)
    | Multi(list(t))
  and t = Grammar.Annotated.t(term, IdTagged.IdTag.t);

  let term_of = IdTagged.term_of;
  let temp = IdTagged.temp;

  let rec to_typ = (sol: t): Typ.t => {
    let (unwrapped_sol, rewrap): (term, Typ.term => Typ.t) =
      IdTagged.unwrap(sol);
    (
      switch (unwrapped_sol) {
      | Unknown(p) => Unknown(p)
      | Atom(a) => Atom(a)
      | Arrow(s1, s2) => Arrow(to_typ(s1), to_typ(s2))
      | Multi(_) => Unknown(Hole(EmptyHole) |> Prov.anonymous)
      | List(elt) => List(to_typ(elt))
      | Sum(sm) => Sum(sm)
      | Prod(elts) => Prod(List.map(e => to_typ(e), elts))
      | Label(l) => Label(l)
      | TupLabel(label, ty) => TupLabel(to_typ(label), to_typ(ty))
      | Rec(pat, ty) => Rec(pat, to_typ(ty))
      | Poly(pat, ty) => Poly(pat, to_typ(ty))
      | Var(v) => Var(v)
      | ProofOf(exp) => ProofOf(exp)
      | ExplicitNonlabel => ExplicitNonlabel
      | ProdExtension(ty1, ty2) => ProdExtension(to_typ(ty1), to_typ(ty2))
      | ProdProjection(ty1, ty2) =>
        ProdProjection(to_typ(ty1), to_typ(ty2))
      }
    )
    |> rewrap;
  };

  let rec all_provs_in_sol = (sol: t): list(Prov.t) => {
    switch (sol |> term_of) {
    | Unknown(p) when is_identified_provenance(p) => [p]
    | Unknown(_) => []
    | Atom(_) => []
    | ProdExtension(t1, t2) => all_provs_in_sol(t1) @ all_provs_in_sol(t2)
    | ProdProjection(t1, t2) => all_provs_in_sol(t1) @ all_provs_in_sol(t2)
    | Arrow(t1, t2) => all_provs_in_sol(t1) @ all_provs_in_sol(t2)
    | List(elt) => all_provs_in_sol(elt)
    | Prod(args) => List.concat_map(all_provs_in_sol, args)
    | Label(_) => []
    | Sum(_) => []
    | ExplicitNonlabel => []
    | TupLabel(l, r) => all_provs_in_sol(l) @ all_provs_in_sol(r)
    | Rec(_, ty) => all_provs_in_sol(ty)
    | Poly(_, ty) => all_provs_in_sol(ty)
    | Var(_) => []
    | Multi(ss) => List.concat_map(all_provs_in_sol, ss)
    | ProofOf(_) => []
    };
  };
  ();

  let solution_typ = (sol: t): Typ.t => {
    let (unwrapped_sol, rewrap): (term, Typ.term => Typ.t) =
      IdTagged.unwrap(sol);
    switch (unwrapped_sol) {
    | Unknown(_)
    | Multi(_) => Unknown(Hole(EmptyHole) |> Prov.anonymous) |> rewrap
    | Atom(_)
    | Sum(_)
    | List(_)
    | Prod(_)
    | Var(_)
    | Label(_)
    | TupLabel(_, _)
    | Rec(_, _)
    | Poly(_, _)
    | ProofOf(_)
    | ExplicitNonlabel
    | ProdExtension(_)
    | ProdProjection(_)
    | Arrow(_) => to_typ(sol)
    };
  };

  let rec of_typ = (typ: Typ.t): t => {
    let (unwrapped_typ, rewrap): (TermBase.typ_term, term => t) =
      IdTagged.unwrap(typ);
    switch (unwrapped_typ) {
    | Atom(t) => Atom(t) |> rewrap
    | Unknown(u) => Unknown(u) |> rewrap
    | Sum(s) => Sum(s) |> rewrap
    | Prod(elts) => Prod(List.map(e => of_typ(e), elts)) |> rewrap
    | Rec(pat, ty) => Rec(pat, of_typ(ty)) |> rewrap
    | Poly(pat, ty) => Poly(pat, of_typ(ty)) |> rewrap
    | List(elt) => List(of_typ(elt)) |> rewrap
    | ProofOf(exp) => ProofOf(exp) |> rewrap
    | ExplicitNonlabel => ExplicitNonlabel |> rewrap
    | Label(s) => Label(s) |> rewrap
    | TupLabel(l, t) => TupLabel(of_typ(l), of_typ(t)) |> rewrap
    | Var(v) => Var(v) |> rewrap
    | Parens(term) => of_typ(term)
    | Arrow(t1, t2) => Arrow(of_typ(t1), of_typ(t2)) |> rewrap
    | ProdProjection(t1, t2) =>
      ProdProjection(of_typ(t1), of_typ(t2)) |> rewrap
    | ProdExtension(t1, t2) =>
      ProdExtension(of_typ(t1), of_typ(t2)) |> rewrap
    };
  };

  /*
   * Is true if the solution expresses more than one possible type. In other words,
   * when the solution possesses a multi anywhere
   */
  let rec has_multiple_types = (sol: t): bool =>
    switch (sol |> term_of) {
    | ProofOf(_)
    | ExplicitNonlabel => false
    | Label(_) => false
    | TupLabel(ty1, ty2) =>
      has_multiple_types(ty1) || has_multiple_types(ty2)
    | ProdExtension(ty1, ty2)
    | ProdProjection(ty1, ty2)
    | Arrow(ty1, ty2) => has_multiple_types(ty1) || has_multiple_types(ty2)
    | Atom(_) => false
    | Var(_) => false
    | Unknown(_) => false
    | Poly(_, ty) => has_multiple_types(ty)
    | Rec(_, ty) => has_multiple_types(ty)
    | List(ty) => has_multiple_types(ty)
    | Sum(_) => false
    | Prod(tys) => List.exists(has_multiple_types, tys)
    | Multi(_) => true
    };

  /*
   * Is all *combinatorial* types a given solution represents.
   *
   * This function will try to filter out any types that are invalid
   *
   * TODO: (THI) maybe for type validity checking, we could run each output type
   * through statics. Would have a performance penalty though
   */
  let rec all_types_from_solution = (sol: t): list(Typ.t) => {
    // should IDs be preserved?
    switch (sol |> term_of) {
    | ProofOf(exp) => [ProofOf(exp) |> Typ.temp]
    | ExplicitNonlabel => [ExplicitNonlabel |> Typ.temp]
    | Label(l) => [Label(l) |> Typ.temp]
    | TupLabel(l, r) =>
      let t1_tys = all_types_from_solution(l);
      let t2_tys = all_types_from_solution(r);
      List.concat_map(
        t1 => List.map(t2 => {TupLabel(t1, t2) |> Typ.temp}, t2_tys),
        t1_tys,
      );
    | ProdProjection(t1, t2) =>
      let t1_tys = all_types_from_solution(t1);
      let t2_tys = all_types_from_solution(t2);
      List.concat_map(
        t1 => List.map(t2 => {ProdProjection(t1, t2) |> Typ.temp}, t2_tys),
        t1_tys,
      );
    | ProdExtension(t1, t2) =>
      let t1_tys = all_types_from_solution(t1);
      let t2_tys = all_types_from_solution(t2);
      List.concat_map(
        t1 => List.map(t2 => {ProdExtension(t1, t2) |> Typ.temp}, t2_tys),
        t1_tys,
      );
    | Arrow(t1, t2) =>
      let t1_tys = all_types_from_solution(t1);
      let t2_tys = all_types_from_solution(t2);
      List.concat_map(
        t1 => List.map(t2 => {Arrow(t1, t2) |> Typ.temp}, t2_tys),
        t1_tys,
      );
    | Atom(a) => [Atom(a) |> Typ.temp]
    | Var(v) => [Var(v) |> Typ.temp]
    | Unknown(p) => [Unknown(p) |> Typ.temp]
    // TODO: i am not sure if the patterns should preserve their ids
    | Poly(pat, ty) =>
      List.map(
        ty => {Poly(pat, ty) |> Typ.temp},
        all_types_from_solution(ty),
      )
    | Rec(pat, ty) =>
      List.map(
        ty => {Rec(pat, ty) |> Typ.temp},
        all_types_from_solution(ty),
      )
    | Sum(sm) => [Sum(sm) |> Typ.temp]
    | Multi(ss) => List.concat_map(all_types_from_solution, ss)
    | List(sol) =>
      List.map(t => {List(t) |> Typ.temp}, all_types_from_solution(sol))
    | Prod(args) =>
      let args_tys = List.map(all_types_from_solution, args);

      // compute the cartesian products of the arguments
      // any tuples that contain a duplicate labels are filtered out
      //
      // this is fine to do as this should never eliminate
      // all possible solutions; that is to say that whenever this
      // case occurs, there must be at least two valid types that
      // get constrainted together to create an invalid type
      List.map(
        ts => {Prod(ts) |> Typ.temp},
        List.filter(
          ps => {
            List.is_empty(
              LabeledTuple.get_duplicate_labels(Typ.match_tup_label, ps),
            )
          },
          cartesian_product(args_tys),
        ),
      );
    };
  };

  let rec replace_solution =
          (prov_to_replace: StringProv.t, sol: t, sol': t): (t, bool) => {
    let fold_solutions =
      List.fold_left(
        ((sols, changed), sol) => {
          let (sol', c) = replace_solution(prov_to_replace, sol, sol');
          ([sol', ...sols], c || changed);
        },
        ([], false),
      );

    let (unwrapped_sol, rewrap) = IdTagged.unwrap(sol);
    switch (unwrapped_sol) {
    | Unknown({term: Hole(CycleHole), _}) => (sol, false)
    | Unknown(q) when prov_to_replace == StringProv.of_prov(q) => (
        sol',
        true,
      )
    | Unknown(_) => (sol, false)
    | Prod(ss) =>
      let (ss', changed) = fold_solutions(ss);
      (Prod(List.rev(ss')) |> rewrap, changed);
    | Multi(ss) =>
      let (ss', changed) = fold_solutions(ss);
      (Multi(List.rev(ss')) |> rewrap, changed);
    | Atom(_) => (sol, false)
    | Sum(_) => (sol, false)
    | Var(_) => (sol, false)
    | ExplicitNonlabel => (sol, false)
    | Label(_) => (sol, false)
    | TupLabel(label, body) =>
      let (label', changed1) =
        replace_solution(prov_to_replace, label, sol');
      let (body', changed2) = replace_solution(prov_to_replace, body, sol');
      (TupLabel(label', body') |> rewrap, changed1 || changed2);
    | Rec(pat, body) =>
      let (body', changed) = replace_solution(prov_to_replace, body, sol');
      (Rec(pat, body') |> rewrap, changed);
    | Poly(pat, body) =>
      let (body', changed) = replace_solution(prov_to_replace, body, sol');
      (Poly(pat, body') |> rewrap, changed);
    | List(t) =>
      let (t', changed) = replace_solution(prov_to_replace, t, sol');
      (List(t') |> rewrap, changed);
    | Arrow(s1, s2) =>
      let (s1', changed1) = replace_solution(prov_to_replace, s1, sol');
      let (s2', changed2) = replace_solution(prov_to_replace, s2, sol');
      (Arrow(s1', s2') |> rewrap, changed1 || changed2);
    | ProdProjection(s1, s2) =>
      let (s1', changed1) = replace_solution(prov_to_replace, s1, sol');
      let (s2', changed2) = replace_solution(prov_to_replace, s2, sol');
      (ProdProjection(s1', s2') |> rewrap, changed1 || changed2);
    | ProdExtension(s1, s2) =>
      let (s1', changed1) = replace_solution(prov_to_replace, s1, sol');
      let (s2', changed2) = replace_solution(prov_to_replace, s2, sol');
      (ProdExtension(s1', s2') |> rewrap, changed1 || changed2);
    | ProofOf(_) => (sol, false)
    };
  };

  // multiholes idk lol???
  let rec refine_solution = (prov: Prov.t, sol: t, typ: Typ.t): t => {
    let (unwrapped_sol, rewrap_sol) = IdTagged.unwrap(sol);
    let (unwrapped_typ, rewrap_typ) = IdTagged.unwrap(typ);

    // TODO: I am not sure if just trashing the type's ID is the right
    // approach, but what are you going to do...
    switch (unwrapped_sol, unwrapped_typ) {
    | (s, Unknown({term: Hole(CycleHole), _}) as t)
    | (Unknown({term: Hole(CycleHole), _}) as s, t) =>
      Multi([s |> rewrap_sol, of_typ(t |> rewrap_typ)]) |> temp
    | (Unknown(p), t) when !is_identified_provenance(p) =>
      of_typ(t |> rewrap_typ)
    | (s, Unknown(p)) when !is_identified_provenance(p) => s |> rewrap_sol
    | (Unknown(_) as s, _) => s |> rewrap_sol
    | (_, Unknown(_) as t) => of_typ(t |> rewrap_typ)

    | (Atom(a1), Atom(a2)) when a1 == a2 => Atom(a1) |> rewrap_sol
    | (Atom(_) as s, Atom(_) as t) =>
      Multi([s |> rewrap_sol, of_typ(t |> rewrap_typ)]) |> temp
    | (List(l1), List(l2)) =>
      List(refine_solution(prov, l1, l2)) |> rewrap_sol
    | (Sum(s1), Sum(s2)) when s1 == s2 => Sum(s1) |> rewrap_sol
    | (Prod(p1), Prod(p2) as t) =>
      if (List.length(p1) == List.length(p2)) {
        Prod(List.map2(refine_solution(prov), p1, p2)) |> rewrap_sol;
      } else {
        Multi([Prod(p1) |> rewrap_sol, of_typ(t |> rewrap_typ)]) |> temp;
      }
    | (ProofOf(exp1), ProofOf(exp2)) when Exp.equal(exp1, exp2) =>
      ProofOf(exp1) |> rewrap_sol
    | (ExplicitNonlabel, ExplicitNonlabel) => ExplicitNonlabel |> rewrap_sol
    | (Label(s1), Label(s2) as t) =>
      if (s1 == s2) {
        Label(s1) |> rewrap_sol;
      } else {
        Multi([Label(s1) |> rewrap_sol, of_typ(t |> rewrap_typ)]) |> temp;
      }
    | (TupLabel(l1, r1), TupLabel(l2, r2)) =>
      TupLabel(refine_solution(prov, l1, l2), refine_solution(prov, r1, r2))
      |> rewrap_sol
    | (Rec(pat1, ty1), Rec(pat2, ty2) as t) =>
      if (TPat.equal(pat1, pat2)) {
        Rec(pat1, refine_solution(prov, ty1, ty2)) |> rewrap_sol;
      } else {
        Multi([Rec(pat1, ty1) |> rewrap_sol, of_typ(t |> rewrap_typ)])
        |> temp;
      }
    | (Poly(pat1, ty1), Poly(pat2, ty2) as t) =>
      // TODO: hmm how does equality work here
      if (TPat.equal(pat1, pat2)) {
        Poly(pat1, refine_solution(prov, ty1, ty2)) |> rewrap_sol;
      } else {
        Multi([Poly(pat1, ty1) |> rewrap_sol, of_typ(t |> rewrap_typ)])
        |> temp;
      }
    | (Arrow(s1, s2), Arrow(t1, t2)) =>
      Arrow(refine_solution(prov, s1, t1), refine_solution(prov, s2, t2))
      |> rewrap_sol
    | (ProdProjection(s1, s2), ProdProjection(t1, t2)) =>
      ProdProjection(
        refine_solution(prov, s1, t1),
        refine_solution(prov, s2, t2),
      )
      |> rewrap_sol
    | (ProdExtension(s1, s2), ProdExtension(t1, t2)) =>
      ProdExtension(
        refine_solution(prov, s1, t1),
        refine_solution(prov, s2, t2),
      )
      |> rewrap_sol
    | (Multi(ss), t) => Multi(ss @ [of_typ(t |> rewrap_typ)]) |> rewrap_sol // TODO: compress possibilities
    | (Atom(_) as s, t)
    | (List(_) as s, t)
    | (ExplicitNonlabel as s, t)
    | (Label(_) as s, t)
    | (TupLabel(_, _) as s, t)
    | (Rec(_, _) as s, t)
    | (Arrow(_, _) as s, t)
    | (ProdProjection(_) as s, t)
    | (ProdExtension(_) as s, t)
    | (Prod(_) as s, t)
    | (Sum(_) as s, t)
    | (Var(_) as s, t)
    | (Poly(_, _) as s, t)
    | (ProofOf(_) as s, t) =>
      Multi([s |> rewrap_sol, of_typ(t |> rewrap_typ)]) |> temp
    };
  };
};

let mk_cyclic_solution: Solution.t =
  Solution.Unknown(Hole(CycleHole) |> Prov.anonymous) |> Solution.temp;

[@deriving (show({with_path: false}), sexp, yojson)]
type canonical_constramnot =
  | Con(Prov.t, Typ.t);

module ProvMap = {
  [@deriving (sexp, yojson)]
  type binding('v) = (StringProv.t, 'v);

  include Map.Make(StringProv);

  let sexp_of_t = (sexp_of_v, map) =>
    map |> bindings |> Sexplib.Std.sexp_of_list(sexp_of_binding(sexp_of_v));

  let t_of_sexp = (v_of_sexp, sexp) =>
    sexp
    |> Sexplib.Std.list_of_sexp(binding_of_sexp(v_of_sexp))
    |> List.to_seq
    |> of_seq;

  let yojson_of_t = (yojson_of_v, map) =>
    map |> bindings |> yojson_of_list(yojson_of_binding(yojson_of_v));

  let t_of_yojson = (v_of_yojson, json) =>
    json
    |> list_of_yojson(binding_of_yojson(v_of_yojson))
    |> List.to_seq
    |> of_seq;

  let pp = (pp_v, fmt, map) =>
    bindings(map)
    |> List.iter(((k, v)) =>
         Format.fprintf(fmt, "%a -> %a\n", StringProv.pp, k, pp_v, v)
       );
};

module SolutionMap: {
  include (module type of ProvMap);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = ProvMap.t(Solution.t);

  let lookup_prov: (Prov.t, t) => option(Solution.t);
  let replace_cycles: (t, list(StringProv.t)) => t;
} = {
  include ProvMap;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = ProvMap.t(Solution.t);

  let lookup_prov = (p: Prov.t, m: t): option(Solution.t) =>
    ProvMap.find_opt(StringProv.of_prov(p), m);

  // relax solution to have no cycles (i.e. replace any un-substituted
  // provnances with cycles)
  let replace_cycles =
    List.fold_left((sol_map, prov) => {
      map(
        sol => {
          let (sol, _) =
            Solution.replace_solution(prov, sol, mk_cyclic_solution);
          sol;
        },
        sol_map,
      )
    });
};

let rec provs_in_typ = (~include_prov=_ => true, t: Typ.t): list(Prov.t) => {
  switch (t |> Typ.term_of) {
  | Unknown(p) when is_identified_provenance(p) && include_prov(p) => [p]
  | Unknown(_) => []
  | Atom(_) => []
  | Arrow(t1, t2) =>
    provs_in_typ(~include_prov, t1) @ provs_in_typ(~include_prov, t2)
  | Prod(args) =>
    List.map(t => provs_in_typ(~include_prov, t), args) |> List.flatten
  | Label(_) => []
  | TupLabel(label, arg) =>
    provs_in_typ(~include_prov, label) @ provs_in_typ(~include_prov, arg)
  | List(elt) => provs_in_typ(~include_prov, elt)
  | Sum(_) => []
  | Parens(term) => provs_in_typ(~include_prov, term)
  | Rec(_, ty) => provs_in_typ(~include_prov, ty)
  | Poly(_, ty) => provs_in_typ(~include_prov, ty)
  | Var(_) => []
  | ProofOf(_) => []
  | ExplicitNonlabel => []
  | ProdProjection(ty1, ty2)
  | ProdExtension(ty1, ty2) =>
    provs_in_typ(~include_prov, ty1) @ provs_in_typ(~include_prov, ty2)
  };
};

let unsolved_provs_in_typ = (t: Typ.t, sm: SolutionMap.t) => {
  let filter = (p: Prov.t) => !SolutionMap.mem(StringProv.of_prov(p), sm);
  provs_in_typ(t, ~include_prov=filter);
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
    if (is_identified_provenance(p) && is_identified_provenance(q)) {
      [Con(p, Unknown(q) |> Typ.temp)];
    } else {
      [];
    }
  | (Unknown(p), t) =>
    if (is_identified_provenance(p)) {
      [Con(p, t |> Typ.temp)];
    } else {
      [];
    }
  | (t, Unknown(p)) =>
    if (is_identified_provenance(p)) {
      [Con(p, t |> Typ.temp)];
    } else {
      [];
    }
  | (Arrow(l1, l2), Arrow(r1, r2)) =>
    unfold_constramnot(Con(l1, r1)) @ unfold_constramnot(Con(l2, r2))
  | (Prod(l_args), Prod(r_args)) =>
    unfold_constramnot_produdct(l_args, r_args)
  | (Label(_), Label(_)) => []
  | (TupLabel(l_label, l_typ), TupLabel(r_label, r_typ)) =>
    unfold_constramnot(Con(l_label, r_label))
    @ unfold_constramnot(Con(l_typ, r_typ))
  | (Atom(_), Atom(_)) => []
  | (Sum(_), Sum(_)) => []
  | (List(l), List(r)) => unfold_constramnot(Con(l, r))
  | (Var(_), Var(_)) => []
  | (Rec(_, l_ty), Rec(_, r_ty)) => unfold_constramnot(Con(l_ty, r_ty))
  | (Poly(_, l_ty), Poly(_, r_ty)) => unfold_constramnot(Con(l_ty, r_ty))
  | (ProdProjection(l1, l2), ProdProjection(r1, r2))
  | (ProdExtension(l1, l2), ProdExtension(r1, r2)) =>
    unfold_constramnot(Con(l1, r1)) @ unfold_constramnot(Con(l2, r2))
  | (Atom(_), _)
  | (Arrow(_), _)
  | (Var(_), _)
  | (Prod(_), _)
  | (Label(_), _)
  | (TupLabel(_), _)
  | (Sum(_), _)
  | (List(_), _)
  | (Rec(_), _)
  | (Poly(_), _)
  | (ProdExtension(_), _)
  | (ProdProjection(_), _)
  | (ExplicitNonlabel, _)
  | (ProofOf(_), _) => []
  };
}
and unfold_constramnot_produdct = (args1, args2): list(canonical_constramnot) =>
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
  type t = list(Typ.t);
  let union: (t, t) => t;
  let empty: t;
  let singleton: Typ.t => t;
  let to_list: t => t;
  let add: (Typ.t, t) => t;
} = {
  type t = list(Typ.t);

  // let set_contains = (x: Typ.term, ts: t) =>
  //   List.exists(
  //     (y: Typ.term) =>
  //       Typ.equal(
  //         ~consider_prov_equivalence=true,
  //         Typ.temp(y),
  //         Typ.temp(x),
  //       ),
  //     ts,
  //   );

  let add = (x: Typ.t, ts: t) => [x, ...ts];

  // Fold for dedup
  let union = (a, b) => List.fold_left((acc, t) => add(t, acc), a, b);
  let empty = [];
  let singleton = (t: Typ.t): t => [t];
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
      (c: canonical_constramnot, prov_map: t, sol_map: SolutionMap.t): t => {
    switch (c) {
    // a provenance is directly constrained to another provenance, in which
    // case once solved, both of them should have identical solutions, so
    // they are merged
    | Con(prov, {term: Unknown(other_prov), _})
        when
          !(
            SolutionMap.mem(StringProv.of_prov(prov), sol_map)
            || SolutionMap.mem(StringProv.of_prov(other_prov), sol_map)
          ) =>
      let prov_map' =
        add_if_absent(prov, prov_map) |> add_if_absent(other_prov);
      let _ =
        UnionFind.merge(
          merge_data,
          lookup_prov(prov, prov_map'),
          lookup_prov(other_prov, prov_map'),
        );
      prov_map';

    // a provenance is constraint to a type (e.g. ?1 ~ ?2 -> ?3), in which case
    // the provenance should dominate all provenances in the type
    | Con(prov, constrained_typ)
        when !SolutionMap.mem(StringProv.of_prov(prov), sol_map) =>
      let prov_map = add_if_absent(prov, prov_map);

      let provs_in_constrained_typ =
        unsolved_provs_in_typ(constrained_typ, sol_map);
      let prov_map =
        List.fold_left(
          (m, q) => add_if_absent(q, m),
          prov_map,
          provs_in_constrained_typ,
        );

      // the provenances in the type are dominated by prov
      List.iter(
        q => {
          update_data(
            q,
            (Internal |> Prov.anonymous, [prov], PossibleTypeSet.empty),
            prov_map,
          )
        },
        provs_in_constrained_typ,
      );

      update_data(
        prov,
        (
          Internal |> Prov.anonymous,
          [],
          PossibleTypeSet.singleton(constrained_typ),
        ),
        prov_map,
      );
      prov_map;
    | _ => prov_map
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

  /* finds a dominant provenance, or if there is none, then picks one that
      is cyclic

     An example of dominant provenance is a in: ?a ~ ?L(a) -> ?R(a)
      */
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

let solve_prov =
    (prov: Prov.t, prov_tys_map: PossibleProvTypesMap.t): Solution.t => {
  let (_, _, ts) =
    UnionFind.get(
      PossibleProvTypesMap.find(StringProv.of_prov(prov), prov_tys_map),
    );
  let ts_list = PossibleTypeSet.to_list(ts);
  List.fold_left(
    Solution.refine_solution(prov),
    Solution.Unknown(Hole(EmptyHole) |> Prov.anonymous) |> Solution.temp,
    ts_list,
  );
};

let string_of_constramnots = (cs: list(Typ.equivalence)): string => {
  "{" ++ String.concat("\n", List.map(Typ.show_equivalence, cs)) ++ "}";
};

let string_of_data = ((_, ps, ts): PossibleProvTypesMap.data): string =>
  "["
  ++ String.concat(
       ", ",
       List.map(p => StringProv.of_prov(p) |> StringProv.show, ps),
     )
  ++ "] | ["
  ++ String.concat(
       ", ",
       List.map(
         t => t |> Typ.term_of |> TermBase.show_typ_term,
         PossibleTypeSet.to_list(ts),
       ),
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
          typ: Typ.t,
          sol_typ: Typ.t,
          prov_map: PossibleProvTypesMap.t,
        )
        : Typ.t => {
  let (unwrapped_typ, rewrap_typ) = Typ.unwrap(typ);
  switch (unwrapped_typ) {
  | Unknown(q) when prov == StringProv.of_prov(q) => sol_typ
  // | Hole(q) => Hole(q)
  | Unknown(_) as u => u |> rewrap_typ
  | Atom(_) as atom => atom |> rewrap_typ
  | List(t) =>
    List(solution_typ_replace_typ(prov, t, sol_typ, prov_map)) |> rewrap_typ
  | Poly(pat, body) =>
    Poly(pat, solution_typ_replace_typ(prov, body, sol_typ, prov_map))
    |> rewrap_typ
  | Sum(_) as sum => sum |> rewrap_typ
  | Var(_) as var => var |> rewrap_typ
  | Prod(args) =>
    Prod(
      List.map(
        arg => solution_typ_replace_typ(prov, arg, sol_typ, prov_map),
        args,
      ),
    )
    |> rewrap_typ
  | Label(_) as label => label |> rewrap_typ
  | TupLabel(label, ty) =>
    TupLabel(
      solution_typ_replace_typ(prov, label, sol_typ, prov_map),
      solution_typ_replace_typ(prov, ty, sol_typ, prov_map),
    )
    |> rewrap_typ
  | Parens(term) => solution_typ_replace_typ(prov, term, sol_typ, prov_map)
  | Rec(pat, body) =>
    Rec(pat, solution_typ_replace_typ(prov, body, sol_typ, prov_map))
    |> rewrap_typ
  | Arrow(t1, t2) =>
    Arrow(
      solution_typ_replace_typ(prov, t1, sol_typ, prov_map),
      solution_typ_replace_typ(prov, t2, sol_typ, prov_map),
    )
    |> rewrap_typ
  | ProofOf(_) as st => st |> rewrap_typ
  | ExplicitNonlabel as st => st |> rewrap_typ
  | ProdExtension(t1, t2) =>
    ProdExtension(
      solution_typ_replace_typ(prov, t1, sol_typ, prov_map),
      solution_typ_replace_typ(prov, t2, sol_typ, prov_map),
    )
    |> rewrap_typ
  | ProdProjection(t1, t2) =>
    ProdProjection(
      solution_typ_replace_typ(prov, t1, sol_typ, prov_map),
      solution_typ_replace_typ(prov, t2, sol_typ, prov_map),
    )
    |> rewrap_typ
  };
};

let solution_typ_replace_con =
    (
      prov_to_replace: StringProv.t,
      Con(cons_t1, cons_t2): Typ.equivalence,
      sol_typ: Typ.t,
      prov_map: PossibleProvTypesMap.t,
    )
    : Typ.equivalence => {
  Con(
    solution_typ_replace_typ(prov_to_replace, cons_t1, sol_typ, prov_map),
    solution_typ_replace_typ(prov_to_replace, cons_t2, sol_typ, prov_map),
  );
};

let solution_typ_replace_cons =
    (
      prov_to_replace: StringProv.t,
      constraints: list(Typ.equivalence),
      sol_typ: Typ.t,
      prov_map: PossibleProvTypesMap.t,
    )
    : list(Typ.equivalence) =>
  List.map(
    c => solution_typ_replace_con(prov_to_replace, c, sol_typ, prov_map),
    constraints,
  );

let extend_sol_map =
    (
      constraints: list(Typ.equivalence),
      sol_map: SolutionMap.t,
      cyclic_provs: list(StringProv.t),
    )
    : option((list(Typ.equivalence), SolutionMap.t, list(StringProv.t))) => {
  // print_endline("Constraints:");
  // print_endline(string_of_constramnots(constraints));
  let canonical_cs = unfold_constramnots(constraints); // make constraints canonical
  // String.concat(
  //   "\n",
  //   List.map(s => show_canonical_constramnot(s), canonical_cs),
  // )
  // |> print_endline;
  let prov_map = PossibleProvTypesMap.of_constramnots(canonical_cs, sol_map); // compute provenance map
  // print_endline("Provenance Map:");
  // print_endline(string_of_prov_map(m));
  switch (PossibleProvTypesMap.find_dominant_provs(prov_map)) {
  // if you find a dominant provenance...
  | ([], _) => None
  | ([prov_to_solve, ..._], is_solution_cyclic) =>
    Some(
      {
        // print_endline(
        //   "Solving: " ++ (StringProv.of_prov(p) |> StringProv.show),
        // );
        let sol = solve_prov(prov_to_solve, prov_map); // solve it
        // print_endline("Solution: " ++ show_solution(s));

        // identify all provenances that are merged with the provenance
        // that was just solved
        let equiv_provs: list(StringProv.t) =
          List.filter_map(
            ((other_prov, _)) => {
              let are_provs_equivalent =
                UnionFind.eq(
                  PossibleProvTypesMap.lookup_prov(prov_to_solve, prov_map),
                  PossibleProvTypesMap.find(other_prov, prov_map),
                );
              if (are_provs_equivalent) {
                Some(other_prov);
              } else {
                None;
              };
            },
            PossibleProvTypesMap.bindings(prov_map),
          );
        // print_endline(
        //   "Equivalent provs: "
        //   ++ String.concat(",", List.map(StringProv.show, equiv_provs)),
        // );

        let cyclic_provs' =
          if (is_solution_cyclic) {
            List.append(cyclic_provs, equiv_provs);
          } else {
            cyclic_provs;
          };

        let solution_type = Solution.solution_typ(sol); // turn it into a type

        // replace the unsolved provenances in the constraints
        // with the solution type we just derived
        // e.g. ?1 ~ Int; { ?1 } -> { ?2 } ==> { Int } -> { ?2 }
        let constraints' =
          List.fold_left(
            (cs_acc, pss) => {
              solution_typ_replace_cons(pss, cs_acc, solution_type, prov_map)
            },
            constraints,
            equiv_provs,
          );

        // extend the solution map with the provenances we just solved
        // once solved, a provenance will never be re-added to the map
        let sol_map' =
          List.fold_left(
            (sm_acc, pss) => SolutionMap.add(pss, sol, sm_acc),
            sol_map,
            equiv_provs,
          );

        // identify all the provenances in the solution, so we can
        // later check if the solution contains a provenance that
        // we just solved
        let all_provs_in_sol =
          List.map(StringProv.of_prov, Solution.all_provs_in_sol(sol));

        // replace the solutions of all existing provenances with
        // we just generated
        let sol_map'' =
          List.fold_left(
            (sol_map_acc, curr_prov) => {
              // a prov is defined to be cyclic if we solved for it, but it
              // appears inside the solution
              let is_prov_cyclic = List.mem(curr_prov, all_provs_in_sol);
              SolutionMap.map(
                sol_to_update => {
                  let (updated_sol, replaced_any) =
                    Solution.replace_solution(curr_prov, sol_to_update, sol);

                  if (replaced_any && is_prov_cyclic) {
                    // if the solution is cyclic, then the original solution should
                    // be identical to sol, except with possibly a few more substitutions
                    // of the cyclic solution.
                    // to make the substitution count consistent, replace the old solution
                    // TODO: i think this might have edge cases, when a cyclic solution is nested
                    sol;
                  } else {
                    updated_sol;
                  };
                },
                sol_map_acc,
              );
            },
            sol_map',
            equiv_provs,
          );

        (constraints', sol_map'', cyclic_provs');
      },
    )
  };
};

let rec solve_rec =
        (
          constraints: list(Typ.equivalence),
          sol_map: SolutionMap.t,
          cyclic_provs: list(StringProv.t),
        )
        : SolutionMap.t => {
  switch (extend_sol_map(constraints, sol_map, cyclic_provs)) {
  | None =>
    // print_endline("No dominant provenances");
    // print_endline(string_of_constramnots(cs));
    SolutionMap.replace_cycles(sol_map, cyclic_provs)
  | Some((cs', sm', cyclic_provs')) => solve_rec(cs', sm', cyclic_provs')
  };
};

let solve = (cs: list(Typ.equivalence)): SolutionMap.t => {
  // print_endline("SOLVING");
  solve_rec(cs, SolutionMap.empty, []);
};

let go = (cs: list(Typ.equivalence)): SolutionMap.t => {
  solve(
    cs,
    // let cs = unfold_constramnots(cs);
    // let m = prov_map_of_constramnots(cs);
    // print_endline("go2");
    // print_endline(string_of_int(List.length(StringMap.to_list(m))));
    // print_endline(string_of_prov_map(m));
  );
};
