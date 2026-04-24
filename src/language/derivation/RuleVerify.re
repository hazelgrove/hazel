/**
  Verification of a derivation step against a rule spec.

  Given a [RuleSpec.t] (produced by [RuleSpec.of_spec]) and a concrete
  conclusion/premises drawn from the user's derivation, [verify] returns a list
  of [failure]s describing why the step does not match, or the empty list if it
  checks. [go_spec] walks the spec and the syntax in lockstep, binding
  spec-side variables to their actual matches in a [map]; [go_test] then runs
  the rule's side-conditions (e.g. membership, equality) over those bindings.
 */
open Util;
open RuleSpec;

module Map = Map.Make(String);

/* A [specced] is a spec term paired with the concrete term it was matched
   against during verification. */
[@deriving (show({with_path: false}), sexp, yojson)]
type specced = (Drv.Any.t, Drv.Any.t);

let show_linked = ((spec, syntax): specced): string =>
  Printf.sprintf(
    "[*%s*](%s)",
    switch (spec) {
    | Exp({term: Var(s), _})
    | Pat({term: Var(s), _})
    | Typ({term: Var(s), _})
    | TPat({term: Var(s), _}) => s
    | _ => "term"
    },
    syntax |> Drv.Any.rep_id |> Id.to_string,
  );

/* A [map] binds spec-side variable names (e.g. "gamma", "a") to the
   [specced] pair they were first matched against. Subsequent occurrences of
   the same name must match equal syntax or verification fails. */
[@deriving (show({with_path: false}), sexp, yojson)]
type map = [@opaque] Map.t(specced);

[@deriving (show({with_path: false}), sexp, yojson)]
type failure =
  | Mismatch(int, int) // expected, actual
  | FailMatch(specced)
  | NotEqual(specced, specced)
  | FailUnbox(specced, Drv.Any.cls)
  | FailTest(map, test);

exception Unreachable;

/* Walk a spec term and the concrete syntax in lockstep:
   - atomic specs (e.g. [Truth]) must match the syntax exactly;
   - spec-side [Var(name)]s are looked up in [map] and either bound, or
     required to match their previous binding (yielding [NotEqual] on a
     mismatch);
   - shape mismatches produce [FailMatch]. */
let rec go_spec: ((map, list(failure)), specced) => (map, list(failure)) =
  ((map, res) as info, (spec, syntax) as specced) => {
    let go_exp = (spec, syntax) =>
      go_spec(_, (Exp(spec), Exp(syntax)): specced);
    let go_pat = (spec, syntax) =>
      go_spec(_, (Pat(spec), Pat(syntax)): specced);
    let go_typ = (spec, syntax) =>
      go_spec(_, (Typ(spec), Typ(syntax)): specced);
    let go_tpat = (spec, syntax) =>
      go_spec(_, (TPat(spec), TPat(syntax)): specced);
    let failunbox = (map, [FailMatch(specced), ...res]);
    let register = s => {
      switch (Map.find_opt(s, map)) {
      | Some((_, syntax') as specced') => (
          map,
          Drv.Any.eq(syntax, syntax', ~skip_hole=false)
            ? res : [NotEqual(specced, specced'), ...res],
        )
      | None => (Map.add(s, specced, map), res)
      };
    };
    switch (spec, syntax) {
    | (Exp(spec), Exp(syntax)) =>
      switch (Drv.Exp.term_of(spec), Drv.Exp.term_of(syntax)) {
      | (Hole(_), _) => raise(Unreachable)
      | (Quote(_), _) => raise(Unreachable)
      | (Var(s), _) => register(s)
      | (Parens(_), _) => raise(Unreachable)
      | (Tuple(_), _) => raise(Unreachable)
      | (Val(sa), Val(a)) => info |> go_exp(sa, a)
      | (Val(_), _) => failunbox
      | (Eval(sa, sb), Eval(a, b)) =>
        info |> go_exp(sa, a) |> go_exp(sb, b)
      | (Eval(_), _) => failunbox
      | (Entail(sa, sb), Entail(a, b)) =>
        info |> go_exp(sa, a) |> go_exp(sb, b)
      | (Entail(_), _) => failunbox
      | (Consistent(sa, sb), Consistent(a, b)) =>
        info |> go_typ(sa, a) |> go_typ(sb, b)
      | (Consistent(_), _) => failunbox
      | (MatchedArrow(sa, sb), MatchedArrow(a, b)) =>
        info |> go_typ(sa, a) |> go_typ(sb, b)
      | (MatchedArrow(_), _) => failunbox
      | (MatchedProd(sa, sb), MatchedProd(a, b)) =>
        info |> go_typ(sa, a) |> go_typ(sb, b)
      | (MatchedProd(_), _) => failunbox
      | (MatchedSum(sa, sb), MatchedSum(a, b)) =>
        info |> go_typ(sa, a) |> go_typ(sb, b)
      | (MatchedSum(_), _) => failunbox
      | (Ctx(_), _) => raise(Unreachable)
      | (Cons(_), _) => raise(Unreachable)
      | (Concat(_), _) => raise(Unreachable)
      | (Type(sa), Type(a)) => info |> go_typ(sa, a)
      | (Type(_), _) => failunbox
      | (HasType(sa, sb), HasType(a, b)) =>
        info |> go_exp(sa, a) |> go_typ(sb, b)
      | (HasType(_), _) => failunbox
      | (Syn(sa, sb), Syn(a, b)) => info |> go_exp(sa, a) |> go_typ(sb, b)
      | (Syn(_), _) => failunbox
      | (Ana(sa, sb), Ana(a, b)) => info |> go_exp(sa, a) |> go_typ(sb, b)
      | (Ana(_), _) => failunbox
      | (And(sa, sb), And(a, b)) => info |> go_exp(sa, a) |> go_exp(sb, b)
      | (And(_), _) => failunbox
      | (Or(sa, sb), Or(a, b)) => info |> go_exp(sa, a) |> go_exp(sb, b)
      | (Or(_), _) => failunbox
      | (Impl(sa, sb), Impl(a, b)) =>
        info |> go_exp(sa, a) |> go_exp(sb, b)
      | (Impl(_), _) => failunbox
      | (Truth, Truth) => info
      | (Truth, _) => failunbox
      | (Falsity, Falsity) => info
      | (Falsity, _) => failunbox
      | (NumLit(_), _) => raise(Unreachable)
      | (Neg(sa), Neg(a)) => info |> go_exp(sa, a)
      | (Neg(_), _) => failunbox
      | (BinOp(sop, sa, sb), BinOp(op, a, b)) when sop == op =>
        info |> go_exp(sa, a) |> go_exp(sb, b)
      | (BinOp(_), _) => failunbox
      | (True, True) => info
      | (True, _) => failunbox
      | (False, False) => info
      | (False, _) => failunbox
      | (If(sa, sb, sc), If(a, b, c)) =>
        info |> go_exp(sa, a) |> go_exp(sb, b) |> go_exp(sc, c)
      | (If(_), _) => failunbox
      | (Let(sp, sa, sb), Let(p, a, b)) =>
        info |> go_pat(sp, p) |> go_exp(sa, a) |> go_exp(sb, b)
      | (Let(_), _) => failunbox
      | (Fix(sp, sa), Fix(p, a)) => info |> go_pat(sp, p) |> go_exp(sa, a)
      | (Fix(_), _) => failunbox
      | (Fun(sp, sa), Fun(p, a)) => info |> go_pat(sp, p) |> go_exp(sa, a)
      | (Fun(_), _) => failunbox
      | (Ap(sa, sb), Ap(a, b)) => info |> go_exp(sa, a) |> go_exp(sb, b)
      | (Ap(_), _) => failunbox
      | (Pair(sa, sb), Pair(a, b)) =>
        info |> go_exp(sa, a) |> go_exp(sb, b)
      | (Pair(_), _) => failunbox
      | (Triv, Triv) => info
      | (Triv, _) => failunbox
      | (PrjL(sa), PrjL(a)) => info |> go_exp(sa, a)
      | (PrjL(_), _) => failunbox
      | (PrjR(sa), PrjR(a)) => info |> go_exp(sa, a)
      | (PrjR(_), _) => failunbox
      | (InjL(sa), InjL(a)) => info |> go_exp(sa, a)
      | (InjL(_), _) => failunbox
      | (InjR(sa), InjR(a)) => info |> go_exp(sa, a)
      | (InjR(_), _) => failunbox
      | (Case(sa, sb, sc, sd, se), Case(a, b, c, d, e)) =>
        info
        |> go_exp(sa, a)
        |> go_pat(sb, b)
        |> go_exp(sc, c)
        |> go_pat(sd, d)
        |> go_exp(se, e)
      | (Case(_), _) => failunbox
      | (Roll(sa), Roll(a)) => info |> go_exp(sa, a)
      | (Roll(_), _) => failunbox
      | (Unroll(sa), Unroll(a)) => info |> go_exp(sa, a)
      | (Unroll(_), _) => failunbox
      | (ExpHole, ExpHole) => info
      | (ExpHole, _) => failunbox
      }
    | (Exp(_), _) => raise(Unreachable)
    | (Pat(spec), Pat(syntax)) =>
      switch (Drv.Pat.term_of(spec), Drv.Pat.term_of(syntax)) {
      | (Hole(_), _) => raise(Unreachable)
      | (Quote(_), _) => raise(Unreachable)
      | (Var(s), _) => register(s)
      | (Parens(_), _) => raise(Unreachable)
      | (Cast(sp, sa), Cast(p, a)) =>
        info |> go_pat(sp, p) |> go_typ(sa, a)
      | (Cast(_), _) => failunbox
      | (InjL(sp), InjL(p)) => info |> go_pat(sp, p)
      | (InjL(_), _) => failunbox
      | (InjR(sp), InjR(p)) => info |> go_pat(sp, p)
      | (InjR(_), _) => failunbox
      | (Pair(sp, sq), Pair(p, q)) =>
        info |> go_pat(sp, p) |> go_pat(sq, q)
      | (Pair(_), _) => failunbox
      }
    | (Pat(_), _) => raise(Unreachable)
    | (Typ(spec), Typ(syntax)) =>
      switch (Drv.Typ.term_of(spec), Drv.Typ.term_of(syntax)) {
      | (Hole(_), _) => raise(Unreachable)
      | (Quote(_), _) => raise(Unreachable)
      | (Var(s), _) => register(s)
      | (Parens(_), _) => raise(Unreachable)
      | (Num, Num) => info
      | (Num, _) => failunbox
      | (Bool, Bool) => info
      | (Bool, _) => failunbox
      | (Arrow(sa, sb), Arrow(a, b)) =>
        info |> go_typ(sa, a) |> go_typ(sb, b)
      | (Arrow(_), _) => failunbox
      | (Prod(sa, sb), Prod(a, b)) =>
        info |> go_typ(sa, a) |> go_typ(sb, b)
      | (Prod(_), _) => failunbox
      | (Unit, Unit) => info
      | (Unit, _) => failunbox
      | (Sum(sa, sb), Sum(a, b)) => info |> go_typ(sa, a) |> go_typ(sb, b)
      | (Sum(_), _) => failunbox
      | (Rec(sp, sa), Rec(p, a)) =>
        info |> go_tpat(sp, p) |> go_typ(sa, a)
      | (Rec(_), _) => failunbox
      | (TypHole, TypHole) => info
      | (TypHole, _) => failunbox
      }
    | (Typ(_), _) => raise(Unreachable)
    | (TPat(spec), TPat(syntax)) =>
      switch (Drv.TPat.term_of(spec), Drv.TPat.term_of(syntax)) {
      | (Hole(_), _) => raise(Unreachable)
      | (Quote(_), _) => raise(Unreachable)
      | (Var(s), _) => register(s)
      }
    | (TPat(_), _) => raise(Unreachable)
    };
  };

/* Evaluate a side-condition [test] against a binding [map] produced by
   [go_spec]. Returns [None] if the test passes, [Some(failure)] otherwise. */
let go_test = (map: map, test: test): option(failure) => {
  exception Failure(failure);
  let lookup = s =>
    switch (Map.find_opt(s, map)) {
    | Some(specced) => specced
    | None => raise(Unreachable)
    };
  let exp_of_specced: specced => Drv.Exp.t =
    fun
    | (_, Exp(syntax)) => syntax
    | _ => raise(Unreachable);
  let pat_of_specced: specced => Drv.Pat.t =
    fun
    | (_, Pat(syntax)) => syntax
    | _ => raise(Unreachable);
  let typ_of_specced: specced => Drv.Typ.t =
    fun
    | (_, Typ(syntax)) => syntax
    | _ => raise(Unreachable);
  let tpat_of_specced: specced => Drv.TPat.t =
    fun
    | (_, TPat(syntax)) => syntax
    | _ => raise(Unreachable);
  let rec go: type a. RuleFormula.t(a) => a =
    formula =>
      switch (formula.term) {
      | LookUpExp(s) => s |> lookup |> exp_of_specced
      | LookUpPat(s) => s |> lookup |> pat_of_specced
      | LookUpTyp(s) => s |> lookup |> typ_of_specced
      | LookUpTPat(s) => s |> lookup |> tpat_of_specced
      | UnboxCtx({term: LookUpExp(s), _}) =>
        let specced = lookup(s);
        switch (Drv.Exp.term_of(exp_of_specced(specced))) {
        | Ctx(syntax) => syntax
        | _ => raise(Failure(FailUnbox(specced, Exp(Ctx))))
        };
      | UnboxCtx(_) => raise(Unreachable)
      | UnboxNumLit({term: LookUpExp(s), _}) =>
        let specced = lookup(s);
        switch (Drv.Exp.term_of(exp_of_specced(specced))) {
        | NumLit(i) => i
        | Neg({term: NumLit(i), _}) => - i
        | _ => raise(Failure(FailUnbox(specced, Exp(NumLit))))
        };
      | UnboxNumLit(_) => raise(Unreachable)
      | UnboxExpVar({term: LookUpExp(s), _}) =>
        let specced = lookup(s);
        switch (Drv.Exp.term_of(exp_of_specced(specced))) {
        | Var(s) => s
        | _ => raise(Failure(FailUnbox(specced, Exp(Var))))
        };
      | UnboxExpVar(_) => raise(Unreachable)
      | UnboxPatVar({term: LookUpPat(s), _}) =>
        let specced = lookup(s);
        let rec f = p =>
          switch (Drv.Pat.term_of(p)) {
          | Var(s) => s
          | Cast(p, _) => f(p)
          | _ => raise(Failure(FailUnbox(specced, Pat(Var))))
          };
        f(pat_of_specced(specced));
      | UnboxTypVar({term: LookUpTyp(s), _}) =>
        let specced = lookup(s);
        switch (Drv.Typ.term_of(typ_of_specced(specced))) {
        | Var(s) => s
        | _ => raise(Failure(FailUnbox(specced, Typ(Var))))
        };
      | UnboxTypVar(_) => raise(Unreachable)
      | UnboxTPatVar({term: LookUpTPat(s), _}) =>
        let specced = lookup(s);
        switch (Drv.TPat.term_of(tpat_of_specced(specced))) {
        | Var(s) => s
        | _ => raise(Failure(FailUnbox(specced, TPat(Var))))
        };
      | ExpVar(s) => Var(go(s)) |> Drv.Exp.fresh
      | HasType(e, t) => HasType(go(e), go(t)) |> Drv.Exp.fresh
      | Type(t) => Type(go(t)) |> Drv.Exp.fresh
      | Fix(p, e) => Fix(go(p), go(e)) |> Drv.Exp.fresh
      | Subst(e, p, e') => Drv.Exp.subst(go(e), go(p), go(e'))
      | Ctx(e) => Ctx(go(e)) |> Drv.Exp.fresh
      | Cons(e, ctx) => Drv.Exp.cons_ctx(go(ctx), go(e))
      | Neg(e) => - go(e)
      | Plus(e1, e2) => go(e1) + go(e2)
      | Minus(e1, e2) => go(e1) - go(e2)
      | Times(e1, e2) => go(e1) * go(e2)
      | TypVar(s) => Var(go(s)) |> Drv.Typ.fresh
      | Rec(t, a) => Rec(go(t), go(a)) |> Drv.Typ.fresh
      | Glb(a, b) => Drv.Typ.glb(go(a), go(b))
      | SubstTy(v, x, e) => Drv.Typ.subst(go(v), go(x), go(e))
      | Ignore(a) => go(a) |> (_ => true)
      | Gt(a, b) => go(a) > go(b)
      | Lt(a, b) => go(a) < go(b)
      | Eq(a, b) => go(a) == go(b)
      | NotGt(a, b) => go(a) <= go(b)
      | NotLt(a, b) => go(a) >= go(b)
      | NotEq(a, b) => go(a) != go(b)
      | EqExp(a, b) => Drv.Exp.eq(go(a), go(b), ~skip_hole=false)
      | EqCtx(a, b) =>
        List.equal(Drv.Exp.eq(~skip_hole=false), go(a), go(b))
      | EqTyp(a, b) => Drv.Typ.eq(go(a), go(b), ~skip_hole=false)
      | Mem(a, b) => Drv.Exp.mem_ctx(go(a), go(b))
      | Subset(a, b) => Drv.Exp.subset_ctx(go(a), go(b))
      };
  try(go(test) ? None : Some(FailTest(map, test))) {
  | Failure(failure) => Some(failure)
  };
};

type res = list(failure);

/* If this failure corresponds to a "partially correct" outcome — i.e. the
   user's syntax contains a hole in the position we were trying to match —
   return the [specced] that locates the hole. Otherwise return [None]. */
let partial_correct_specced: failure => option(specced) =
  fun
  | FailMatch((_, syntax) as specced)
  | FailUnbox((_, syntax) as specced, _) =>
    DrvTerm.Any.is_hole(syntax) ? Some(specced) : None
  | NotEqual((_, syntax1) as specced1, (_, syntax2) as specced2) =>
    DrvTermBase.Any.eq(syntax1, syntax2, ~skip_hole=true)
      ? DrvTerm.Any.contains_hole(syntax1)
          ? Some(specced1) : Some(specced2)
      : None
  | Mismatch(_, _)
  | FailTest(_, _) => None;

/* If every failure in [res] is "partially correct", return one of the hole
   [specced]s; otherwise [None]. Requires [res] to be non-empty. */
let all_partial_correct: res => option(specced) =
  res => {
    let ss = List.map(partial_correct_specced, res);
    List.exists(Option.is_none, ss) ? None : List.hd(ss);
  };

let verify: (t, (Drv.Exp.t, list(Drv.Exp.t))) => res =
  ({concl, prems, tests}, (concl_syntax, prems_syntax)) => {
    let rec go_specs = (xs, ys, acc) =>
      switch (xs, ys) {
      | ([x, ...xs], [y, ...ys]) =>
        go_specs(xs, ys, go_spec(acc, (Exp(x), Exp(y))))
      | _ => acc
      };
    let (map, res) =
      go_specs(
        [concl, ...prems],
        [concl_syntax, ...prems_syntax],
        (Map.empty, []),
      );
    let (m, n) = (List.length(prems), List.length(prems_syntax));
    /* If the premise count doesn't match, or we already found match errors,
       skip the side-condition tests — they'd only produce noisy failures. */
    let res = res @ (m != n ? [Mismatch(m, n)] : []);

    let go_tests: (map, list(failure), list(test)) => list(failure) =
      map =>
        List.fold_left((res, test) => {
          switch (go_test(map, test)) {
          | None => res
          | Some(failure) => [failure, ...res]
          }
        });
    List.is_empty(res) ? go_tests(map, res, tests) : res;
  };

/* Debug helper: print every rule's spec (premises, conclusion, test count)
   to stdout. Handy when adding or tweaking a rule. */
let __print_all_specs_and_tests = () => {
  Rule.all
  |> List.iter(rule => {
       let Spec.{concl, prems, tests} = of_spec(rule);
       List.iter(prem => print_endline("  " ++ Drv.Exp.show(prem)), prems);
       List.iter(_test => print_endline("  {Test} "), tests);
       print_endline(
         "——————————————————————["
         ++ Rule.show(rule)
         ++ "]\n  "
         ++ Drv.Exp.show(concl)
         ++ "\n",
       );
     });
};
