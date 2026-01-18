open Util;

module type SolType = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t;
  let equal: (t, t) => bool;
};

module type SolutionBase = {
  module SolType: SolType;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Grammar.Annotated.t(term, IdTagged.IdTag.t);

  let term_of: t => term;
  let temp: term => t;

  let all_provs_of: t => list(Prov.t);
  let solution_term: t => SolType.t;
  let of_sol_term: SolType.t => t;

  /*
   * Is true if the solution expresses more than one possible type. In other words,
   * when the solution possesses a multi anywhere
   */
  let has_multiple_solutions: t => bool;
  let expand_solution: t => list(SolType.t);
  let replace_solution: (StringProv.t, t, t) => (t, bool);
  let refine_solution: (t, SolType.t) => t;

  let anon_unknown: t;
};

module TPatSolution: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Unknown(Prov.t)
    | Var(string)
    | Multi(list(t))
  [@deriving (show({with_path: false}), sexp, yojson)]
  and t = Grammar.Annotated.t(term, IdTagged.IdTag.t);
  include
    SolutionBase with module SolType = TPat with type term := term with
      type t := t;
} = {
  module SolType = TPat;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Unknown(Prov.t)
    | Var(string)
    | Multi(list(t))
  and t = Grammar.Annotated.t(term, IdTagged.IdTag.t);

  let term_of = IdTagged.term_of;
  let temp = IdTagged.temp;

  let to_tpat = (sol: t): SolType.t => {
    let (unwrapped_sol, rewrap): (term, TPat.term => TPat.t) =
      IdTagged.unwrap(sol);
    (
      switch (unwrapped_sol) {
      | Unknown(p) => Unknown(p)
      | Var(v) => Var(v)
      | Multi(_) => Unknown(Hole(EmptyHole) |> Prov.anonymous)
      }
    )
    |> rewrap;
  };

  let rec all_provs_of = (sol: t): list(Prov.t) => {
    switch (sol |> term_of) {
    | Unknown(p) when Prov.is_identified(p) => [p]
    | Unknown(_) => []
    | Var(_) => []
    | Multi(xs) => List.concat_map(x => all_provs_of(x), xs)
    };
  };

  let solution_term = (sol: t): SolType.t => {
    let (unwrapped_sol, rewrap): (term, TPat.term => TPat.t) =
      IdTagged.unwrap(sol);
    switch (unwrapped_sol) {
    | Unknown(_) => Unknown(Hole(EmptyHole) |> Prov.anonymous) |> rewrap
    | Multi(_)
    | Var(_) => to_tpat(sol)
    };
  };

  let of_sol_term = (tpat: SolType.t): t => {
    let (unwrapped_tpat, rewrap) = IdTagged.unwrap(tpat);
    (
      switch (unwrapped_tpat) {
      | Unknown(p) => Unknown(p)
      | Var(v) => Var(v)
      }
    )
    |> rewrap;
  };

  let has_multiple_solutions = (sol: t): bool => {
    switch (sol |> term_of) {
    | Unknown(_) => false
    | Var(_) => false
    | Multi(_) => true
    };
  };

  let expand_solution = (sol: t): list(SolType.t) => {
    let typ = to_tpat(sol);
    [typ];
  };

  let replace_solution = (prov: StringProv.t, sol: t, new_sol: t): (t, bool) => {
    switch (sol.term) {
    | Unknown(p) when prov == StringProv.of_prov(p) => (new_sol, true)
    | _ => (sol, false)
    };
  };

  let refine_solution = (sol: t, tpat: SolType.t): t => {
    let (unwrapped_sol, rewrap_sol) = IdTagged.unwrap(sol);
    let (unwrapped_typ, rewrap_tpat) = IdTagged.unwrap(tpat);

    // TODO: I am not sure if just trashing the type's ID is the right
    // approach, but what are you going to do...
    switch (unwrapped_sol, unwrapped_typ) {
    | (s, Unknown({term: Hole(CycleHole), _}) as t)
    | (Unknown({term: Hole(CycleHole), _}) as s, t) =>
      Multi([s |> rewrap_sol, of_sol_term(t |> rewrap_tpat)]) |> temp
    | (Unknown(p), t) when !Prov.is_identified(p) =>
      of_sol_term(t |> rewrap_tpat)
    | (s, Unknown(p)) when !Prov.is_identified(p) => s |> rewrap_sol
    | (Unknown(_) as s, _) => s |> rewrap_sol
    | (_, Unknown(_) as t) => of_sol_term(t |> rewrap_tpat)
    | (Var(_), _) => Multi([sol, of_sol_term(tpat)]) |> temp
    | (Multi(ss), tpat) =>
      Multi(ss @ [of_sol_term(tpat |> rewrap_tpat)]) |> rewrap_sol // TODO: compress possibilities
    };
  };

  let anon_unknown = Unknown(Hole(EmptyHole) |> Prov.anonymous) |> temp;
};

module TypSolution: {
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
    | Rec(TPatSolution.t, t)
    | Poly(TPatSolution.t, t)
    | Var(string)
    | ExplicitNonlabel
    | ProofOf(Exp.t)
    | ProdProjection(t, t)
    | ProdExtension(t, t)
    | Multi(list(t))
  [@deriving (show({with_path: false}), sexp, yojson)]
  and t = Grammar.Annotated.t(term, IdTagged.IdTag.t);
  include
    SolutionBase with module SolType = Typ with type term := term with
      type t := t;

  let replace_tpat_solution: (StringProv.t, t, TPatSolution.t) => (t, bool);
} = {
  // computes the cartesian product of a list of lists
  let rec cartesian_product =
          (lists: list(list(Typ.t))): list(list(Typ.t)) =>
    switch (lists) {
    | [] => [[]]
    | [hd, ...tl] =>
      let tl_product = cartesian_product(tl);
      List.concat_map(h => List.map(t => [h, ...t], tl_product), hd);
    };
  module SolType = Typ;

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
    | Rec(TPatSolution.t, t)
    | Poly(TPatSolution.t, t)
    | Var(string)
    | ExplicitNonlabel
    | ProofOf(Exp.t)
    | ProdProjection(t, t)
    | ProdExtension(t, t)
    | Multi(list(t))
  and t = Grammar.Annotated.t(term, IdTagged.IdTag.t);

  module Set = PossibleSolutionSet.Make(Typ);

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
      | Rec(pat, ty) => Rec(TPatSolution.solution_term(pat), to_typ(ty))
      | Poly(pat, ty) => Poly(TPatSolution.solution_term(pat), to_typ(ty))
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

  let rec all_provs_of = (sol: t): list(Prov.t) => {
    switch (sol |> term_of) {
    | Unknown(p) when Prov.is_identified(p) => [p]
    | Unknown(_) => []
    | Atom(_) => []
    | ProdExtension(t1, t2) => all_provs_of(t1) @ all_provs_of(t2)
    | ProdProjection(t1, t2) => all_provs_of(t1) @ all_provs_of(t2)
    | Arrow(t1, t2) => all_provs_of(t1) @ all_provs_of(t2)
    | List(elt) => all_provs_of(elt)
    | Prod(args) => List.concat_map(all_provs_of, args)
    | Label(_) => []
    | Sum(_) => []
    | ExplicitNonlabel => []
    | TupLabel(l, r) => all_provs_of(l) @ all_provs_of(r)
    | Rec(_, ty) => all_provs_of(ty)
    | Poly(_, ty) => all_provs_of(ty)
    | Var(_) => []
    | Multi(ss) => List.concat_map(all_provs_of, ss)
    | ProofOf(_) => []
    };
  };
  ();

  let solution_term = (sol: t): Typ.t => {
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

  let rec of_sol_term = (typ: Typ.t): t => {
    let (unwrapped_typ, rewrap): (TermBase.typ_term, term => t) =
      IdTagged.unwrap(typ);
    switch (unwrapped_typ) {
    | Atom(t) => Atom(t) |> rewrap
    | Unknown(u) => Unknown(u) |> rewrap
    | Sum(s) => Sum(s) |> rewrap
    | Prod(elts) => Prod(List.map(e => of_sol_term(e), elts)) |> rewrap
    | Rec(pat, ty) =>
      Rec(TPatSolution.of_sol_term(pat), of_sol_term(ty)) |> rewrap
    | Poly(pat, ty) =>
      Poly(TPatSolution.of_sol_term(pat), of_sol_term(ty)) |> rewrap
    | List(elt) => List(of_sol_term(elt)) |> rewrap
    | ProofOf(exp) => ProofOf(exp) |> rewrap
    | ExplicitNonlabel => ExplicitNonlabel |> rewrap
    | Label(s) => Label(s) |> rewrap
    | TupLabel(l, t) => TupLabel(of_sol_term(l), of_sol_term(t)) |> rewrap
    | Var(v) => Var(v) |> rewrap
    | Parens(term) => of_sol_term(term)
    | Arrow(t1, t2) => Arrow(of_sol_term(t1), of_sol_term(t2)) |> rewrap
    | ProdProjection(t1, t2) =>
      ProdProjection(of_sol_term(t1), of_sol_term(t2)) |> rewrap
    | ProdExtension(t1, t2) =>
      ProdExtension(of_sol_term(t1), of_sol_term(t2)) |> rewrap
    };
  };

  /*
   * Is true if the solution expresses more than one possible type. In other words,
   * when the solution possesses a multi anywhere
   */
  let rec has_multiple_solutions = (sol: t): bool =>
    switch (sol |> term_of) {
    | ProofOf(_)
    | ExplicitNonlabel => false
    | Label(_) => false
    | TupLabel(ty1, ty2) =>
      has_multiple_solutions(ty1) || has_multiple_solutions(ty2)
    | ProdExtension(ty1, ty2)
    | ProdProjection(ty1, ty2)
    | Arrow(ty1, ty2) =>
      has_multiple_solutions(ty1) || has_multiple_solutions(ty2)
    | Atom(_) => false
    | Var(_) => false
    | Unknown(_) => false
    | Poly(_, ty) => has_multiple_solutions(ty)
    | Rec(_, ty) => has_multiple_solutions(ty)
    | List(ty) => has_multiple_solutions(ty)
    | Sum(_) => false
    | Prod(tys) => List.exists(has_multiple_solutions, tys)
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
  let rec expand_solution = (sol: t): list(Typ.t) => {
    // should IDs be preserved?
    switch (sol |> term_of) {
    | ProofOf(exp) => [ProofOf(exp) |> Typ.temp]
    | ExplicitNonlabel => [ExplicitNonlabel |> Typ.temp]
    | Label(l) => [Label(l) |> Typ.temp]
    | TupLabel(l, r) =>
      let t1_tys = expand_solution(l);
      let t2_tys = expand_solution(r);
      List.concat_map(
        t1 => List.map(t2 => {TupLabel(t1, t2) |> Typ.temp}, t2_tys),
        t1_tys,
      );
    | ProdProjection(t1, t2) =>
      let t1_tys = expand_solution(t1);
      let t2_tys = expand_solution(t2);
      List.concat_map(
        t1 => List.map(t2 => {ProdProjection(t1, t2) |> Typ.temp}, t2_tys),
        t1_tys,
      );
    | ProdExtension(t1, t2) =>
      let t1_tys = expand_solution(t1);
      let t2_tys = expand_solution(t2);
      List.concat_map(
        t1 => List.map(t2 => {ProdExtension(t1, t2) |> Typ.temp}, t2_tys),
        t1_tys,
      );
    | Arrow(t1, t2) =>
      let t1_tys = expand_solution(t1);
      let t2_tys = expand_solution(t2);
      List.concat_map(
        t1 => List.map(t2 => {Arrow(t1, t2) |> Typ.temp}, t2_tys),
        t1_tys,
      );
    | Atom(a) => [Atom(a) |> Typ.temp]
    | Var(v) => [Var(v) |> Typ.temp]
    | Unknown(p) => [Unknown(p) |> Typ.temp]
    | Poly(pat, ty) =>
      let pat_tys = TPatSolution.expand_solution(pat);
      let tys = expand_solution(ty);
      List.concat_map(
        pat => List.map(t2 => {Poly(pat, t2) |> Typ.temp}, tys),
        pat_tys,
      );
    | Rec(pat, ty) =>
      let pat_tys = TPatSolution.expand_solution(pat);
      let tys = expand_solution(ty);
      List.concat_map(
        pat => List.map(t2 => {Rec(pat, t2) |> Typ.temp}, tys),
        pat_tys,
      );
    | Sum(sm) => [Sum(sm) |> Typ.temp]
    | Multi(ss) => List.concat_map(expand_solution, ss)
    | List(sol) =>
      List.map(t => {List(t) |> Typ.temp}, expand_solution(sol))
    | Prod(args) =>
      let args_tys = List.map(expand_solution, args);

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

  let fold_solutions = (prov_to_replace, sol', f) =>
    List.fold_left(
      ((sols, changed), sol) => {
        let (sol', c) = f(prov_to_replace, sol, sol');
        ([sol', ...sols], c || changed);
      },
      ([], false),
    );

  let rec replace_solution =
          (prov_to_replace: StringProv.t, sol: t, sol': t): (t, bool) => {
    let fold_solutions =
      fold_solutions(prov_to_replace, sol', replace_solution);

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

  let rec replace_tpat_solution =
          (prov_to_replace: StringProv.t, sol: t, tpat_sol': TPatSolution.t)
          : (t, bool) => {
    let fold_solutions =
      fold_solutions(prov_to_replace, tpat_sol', replace_tpat_solution);

    let (unwrapped_sol, rewrap) = IdTagged.unwrap(sol);
    switch (unwrapped_sol) {
    | Unknown(_) => (sol, false)
    | Prod(ss) =>
      let (ss', changed) = fold_solutions(ss);
      (Prod(List.rev(ss')) |> rewrap, changed);
    | Multi(ss) =>
      let (ss', changed) = fold_solutions(ss);
      (Multi(List.rev(ss')) |> rewrap, changed);
    | Atom(_)
    | Sum(_)
    | Var(_)
    | ExplicitNonlabel
    | Label(_) => (sol, false)
    | TupLabel(label, body) =>
      let (label', changed1) =
        replace_tpat_solution(prov_to_replace, label, tpat_sol');
      let (body', changed2) =
        replace_tpat_solution(prov_to_replace, body, tpat_sol');
      (TupLabel(label', body') |> rewrap, changed1 || changed2);
    | Rec(pat, body) =>
      let (pat', pat_changed) =
        TPatSolution.replace_solution(prov_to_replace, pat, tpat_sol');
      let (body', body_changed) =
        replace_tpat_solution(prov_to_replace, body, tpat_sol');
      (Rec(pat', body') |> rewrap, pat_changed || body_changed);
    | Poly(pat, body) =>
      let (pat', pat_changed) =
        TPatSolution.replace_solution(prov_to_replace, pat, tpat_sol');
      let (body', body_changed) =
        replace_tpat_solution(prov_to_replace, body, tpat_sol');
      (Poly(pat', body') |> rewrap, pat_changed || body_changed);
    | List(t) =>
      let (t', changed) =
        replace_tpat_solution(prov_to_replace, t, tpat_sol');
      (List(t') |> rewrap, changed);
    | Arrow(s1, s2) =>
      let (s1', changed1) =
        replace_tpat_solution(prov_to_replace, s1, tpat_sol');
      let (s2', changed2) =
        replace_tpat_solution(prov_to_replace, s2, tpat_sol');
      (Arrow(s1', s2') |> rewrap, changed1 || changed2);
    | ProdProjection(s1, s2) =>
      let (s1', changed1) =
        replace_tpat_solution(prov_to_replace, s1, tpat_sol');
      let (s2', changed2) =
        replace_tpat_solution(prov_to_replace, s2, tpat_sol');
      (ProdProjection(s1', s2') |> rewrap, changed1 || changed2);
    | ProdExtension(s1, s2) =>
      let (s1', changed1) =
        replace_tpat_solution(prov_to_replace, s1, tpat_sol');
      let (s2', changed2) =
        replace_tpat_solution(prov_to_replace, s2, tpat_sol');
      (ProdExtension(s1', s2') |> rewrap, changed1 || changed2);
    | ProofOf(_) => (sol, false)
    };
  };

  // multiholes idk lol???
  let rec refine_solution = (sol: t, typ: Typ.t): t => {
    let (unwrapped_sol, rewrap_sol) = IdTagged.unwrap(sol);
    let (unwrapped_typ, rewrap_typ) = IdTagged.unwrap(typ);

    // TODO: I am not sure if just trashing the type's ID is the right
    // approach, but what are you going to do...
    switch (unwrapped_sol, unwrapped_typ) {
    | (s, Unknown({term: Hole(CycleHole), _}) as t)
    | (Unknown({term: Hole(CycleHole), _}) as s, t) =>
      Multi([s |> rewrap_sol, of_sol_term(t |> rewrap_typ)]) |> temp
    | (Unknown(p), t) when !Prov.is_identified(p) =>
      of_sol_term(t |> rewrap_typ)
    | (s, Unknown(p)) when !Prov.is_identified(p) => s |> rewrap_sol
    | (Unknown(_) as s, _) => s |> rewrap_sol
    | (_, Unknown(_) as t) => of_sol_term(t |> rewrap_typ)

    | (Atom(a1), Atom(a2)) when a1 == a2 => Atom(a1) |> rewrap_sol
    | (Atom(_) as s, Atom(_) as t) =>
      Multi([s |> rewrap_sol, of_sol_term(t |> rewrap_typ)]) |> temp
    | (List(l1), List(l2)) => List(refine_solution(l1, l2)) |> rewrap_sol
    | (Sum(s1), Sum(s2)) when s1 == s2 => Sum(s1) |> rewrap_sol
    | (Prod(p1), Prod(p2) as t) =>
      if (List.length(p1) == List.length(p2)) {
        Prod(List.map2(refine_solution, p1, p2)) |> rewrap_sol;
      } else {
        Multi([Prod(p1) |> rewrap_sol, of_sol_term(t |> rewrap_typ)])
        |> temp;
      }
    | (ProofOf(exp1), ProofOf(exp2)) when Exp.equal(exp1, exp2) =>
      ProofOf(exp1) |> rewrap_sol
    | (ExplicitNonlabel, ExplicitNonlabel) => ExplicitNonlabel |> rewrap_sol
    | (Label(s1), Label(s2) as t) =>
      if (s1 == s2) {
        Label(s1) |> rewrap_sol;
      } else {
        Multi([Label(s1) |> rewrap_sol, of_sol_term(t |> rewrap_typ)])
        |> temp;
      }
    | (TupLabel(l1, r1), TupLabel(l2, r2)) =>
      TupLabel(refine_solution(l1, l2), refine_solution(r1, r2))
      |> rewrap_sol
    | (Rec(pat1, ty1), Rec(pat2, ty2)) =>
      Rec(
        TPatSolution.refine_solution(pat1, pat2),
        refine_solution(ty1, ty2),
      )
      |> rewrap_sol
    | (Poly(pat1, ty1), Poly(pat2, ty2)) =>
      Poly(
        TPatSolution.refine_solution(pat1, pat2),
        refine_solution(ty1, ty2),
      )
      |> rewrap_sol
    | (Arrow(s1, s2), Arrow(t1, t2)) =>
      Arrow(refine_solution(s1, t1), refine_solution(s2, t2)) |> rewrap_sol
    | (ProdProjection(s1, s2), ProdProjection(t1, t2)) =>
      ProdProjection(refine_solution(s1, t1), refine_solution(s2, t2))
      |> rewrap_sol
    | (ProdExtension(s1, s2), ProdExtension(t1, t2)) =>
      ProdExtension(refine_solution(s1, t1), refine_solution(s2, t2))
      |> rewrap_sol
    | (Multi(ss), t) =>
      Multi(ss @ [of_sol_term(t |> rewrap_typ)]) |> rewrap_sol // TODO: compress possibilities
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
      Multi([s |> rewrap_sol, of_sol_term(t |> rewrap_typ)]) |> temp
    };
  };

  let anon_unknown = Unknown(Hole(EmptyHole) |> Prov.anonymous) |> temp;

  let temp_cyclic: t = Unknown(Hole(CycleHole) |> Prov.anonymous) |> temp /*   | Cyclic => "{Cyclic}"*/;
  // let rec to_string =
  //   fun
  //   | EHole => "?"
  //   | Hole(p) => "?{" ++ string_of_prov(p) ++ "}"
  //   | Num => "Num"
  //   | Bool => "Bool"
  //   | Arrow(s1, s2) =>
  //     "(" ++ string_of_solution(s1) ++ "->" ++ string_of_solution(s2) ++ ")"
  //   | Multi(ss) =>
  //     "{" ++ String.concat("|", List.map(string_of_solution, ss)) ++ "}";
};
