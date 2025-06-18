open Util;
open RuleSpec;

// No repr for `t` because we will use `of_syntax` to convert `t` to `DrvSyntax.t`
// and use `DrvSyntax.repr`.
module Map = Map.Make(String);

// A `specced` is a pair of a rule spec the real derivation
// syntax that is checked against.
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

// A `map` maps a name in Reg to a `specced`.
[@deriving (show({with_path: false}), sexp, yojson)]
type map = [@opaque] Map.t(specced);

module Formula = {
  include Formula;
  // let rec show_linked = (p, map: map, op) =>
  //   switch (op) {
  //   | Get(s) =>
  //     switch (RuleSpec.Map.find_opt(s, map)) {
  //     | Some(specced) => RuleSpec.show_linked(specced)
  //     | None => s
  //     }
  //   | _ =>
  //     op
  //     |> repr(p)
  //     |> Aba.join(Fun.id, show_linked(precedence(op), map))
  //     |> String.concat("")
  //   };
  // let show_linked = (map: map, test: t) =>
  //   test
  //   |> repr
  //   |> (
  //     ((labels, ops) as aba) =>
  //       switch (test, ops) {
  //       | (Eq(Get(_), _), [a, b]) => (labels, [a, b])
  //       | _ => aba
  //       }
  //   )
  //   |> Aba.join(Fun.id, Operation.show_linked(map))
  //   |> String.concat("");
};

[@deriving (show({with_path: false}), sexp, yojson)]
type failure =
  | Mismatch(int, int) // expected, actual
  | FailMatch(specced)
  | NotEqual(specced, specced)
  | FailUnbox(specced, Drv.Any.cls)
  | FailTest(map, test);

let failure_msg_vague = (failure: failure): string =>
  switch (failure) {
  | Mismatch(expected, actual) =>
    Printf.sprintf("Expected %d premises, but found %d", expected, actual)
  | FailMatch(_) => "Could not match a term against a expected form"
  | NotEqual(_, _) => "Two matched terms that should be equal were different"
  | FailUnbox(_, _) => "Could not extract an atom form from a term"
  | FailTest(_, _) => "Matched terms failed a test (hidden premise)"
  };

let failure_msg_vague = e =>
  e |> failure_msg_vague |> Printf.sprintf("❌ %s");

/**
  This module describles the speculation of rules for checking
  involving calculations. Refer to `RuleSpec.re` For speculations
  on unboxing and unification,
 */

exception Unreachable;

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
  let rec go: type a. Formula.t(a) => a =
    formula =>
      switch (formula) {
      | LookUpExp(s) => s |> lookup |> exp_of_specced
      | LookUpPat(s) => s |> lookup |> pat_of_specced
      | LookUpTyp(s) => s |> lookup |> typ_of_specced
      | LookUpTPat(s) => s |> lookup |> tpat_of_specced
      | UnboxCtx(LookUpExp(s)) =>
        let specced = lookup(s);
        switch (Drv.Exp.term_of(exp_of_specced(specced))) {
        | Ctx(syntax) => syntax
        | _ => raise(Failure(FailUnbox(specced, Exp(Ctx))))
        };
      | UnboxCtx(_) => raise(Unreachable)
      | UnboxNumLit(LookUpExp(s)) =>
        let specced = lookup(s);
        switch (Drv.Exp.term_of(exp_of_specced(specced))) {
        | NumLit(i) => i
        | Neg({term: NumLit(i), _}) => - i
        | _ => raise(Failure(FailUnbox(specced, Exp(NumLit))))
        };
      | UnboxNumLit(_) => raise(Unreachable)
      | UnboxExpVar(LookUpExp(s)) =>
        let specced = lookup(s);
        switch (Drv.Exp.term_of(exp_of_specced(specced))) {
        | Var(s) => s
        | _ => raise(Failure(FailUnbox(specced, Exp(Var))))
        };
      | UnboxExpVar(_) => raise(Unreachable)
      | UnboxPatVar(LookUpPat(s)) =>
        let specced = lookup(s);
        let rec f = p =>
          switch (Drv.Pat.term_of(p)) {
          | Var(s) => s
          | Cast(p, _) => f(p)
          | _ => raise(Failure(FailUnbox(specced, Pat(Var))))
          };
        f(pat_of_specced(specced));
      | UnboxTypVar(LookUpTyp(s)) =>
        let specced = lookup(s);
        switch (Drv.Typ.term_of(typ_of_specced(specced))) {
        | Var(s) => s
        | _ => raise(Failure(FailUnbox(specced, Typ(Var))))
        };
      | UnboxTypVar(_) => raise(Unreachable)
      | UnboxTPatVar(LookUpTPat(s)) =>
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

//   let repr = (~sp: string=" ", p: int, operation: t): Aba.t(string, t) => {
//     let p' = precedence(operation);
//     let tight_start = s =>
//       s == ""
//       || List.exists(
//            String.ends_with(s, ~suffix=_),
//            ["/", "「", "」", "("],
//          );
//     let tight_end = s =>
//       s == ""
//       || List.exists(
//            String.starts_with(s, ~prefix=_),
//            ["/", "」", ",", ")"],
//          );
//     let mk_parens = labels =>
//       labels
//       |> ListUtil.map_first(s => p < p' ? "(" ++ s : s)
//       |> ListUtil.map_last(s => p < p' ? s ++ ")" : s);
//     let op = labels =>
//       labels
//       |> List.map(s =>
//            (tight_end(s) ? "" : sp) ++ s ++ (tight_start(s) ? "" : sp)
//          )
//       |> ListUtil.map_first(s =>
//            String.trim(s) ++ (tight_start(s) ? "" : sp)
//          )
//       |> ListUtil.map_last(s => (tight_end(s) ? "" : sp) ++ String.trim(s))
//       |> mk_parens;
//     let bin = (labels: list(string)) => op([""] @ labels @ [""]);
//     let pre = (labels: list(string)) => op(labels @ [""]);
//     let post = (labels: list(string)) => op([""] @ labels);
//     let op_sg = (label: string) => [label];
//     let bin_sg = (label: string) => bin([label]);
//     let pre_sg = (label: string) => pre([label]);
//     let post_sg = (label: string) => post([label]);
//     switch (operation) {
//     | Get(s) => (s |> op_sg, [])
//     | VarOfPat(p) => ([] |> bin, [p])
//     | TVarOfTPat(t) => ([] |> bin, [t])
//     | HasType(x, t) => (":" |> bin_sg, [x, t])
//     | Type(a) => ("type" |> post_sg, [a])
//     | Fix(p, e) => (["fix", "→"] |> pre, [p, e])
//     | Rec(t, a) => (["rec", "is"] |> pre, [t, a])
//     | Glb(a, b) => (["glb(", ",", ")"] |> op, [a, b])
//     | Subst((v, x), e) => (["「", "/", "」"] |> pre, [v, x, e])
//     | SubstTy((t, a), e) => (["「", "/", "」"] |> pre, [t, a, e])
//     | Cons(e, l) => ("," |> bin_sg, [l, e])
//     | Neg(n) => ("-" |> pre_sg, [n])
//     | Plus(n1, n2) => ("+" |> bin_sg, [n1, n2])
//     | Minus(n1, n2) => ("-" |> bin_sg, [n1, n2])
//     | Times(n1, n2) => ("×" |> bin_sg, [n1, n2])
//     };
//   };

//   let rec show = (p, syntax) =>
//     syntax
//     |> repr(p)
//     |> Aba.join(Fun.id, show(precedence(syntax)))
//     |> String.concat("");

//   let show = show(Precedence.min);

//   let show_linked = show_linked(Precedence.min);

// let repr = (~sp: string=" ", test: t): Aba.t(string, Operation.t) => {
//   let op = labels =>
//     labels
//     |> List.map(s => sp ++ s ++ sp)
//     |> ListUtil.map_first(s => String.trim(s))
//     |> ListUtil.map_last(s => String.trim(s));
//   let bin = (labels: list(string)) => op([""] @ labels @ [""]);
//   let bin_sg = (label: string) => bin([label]);
//   switch (test) {
//   | Eq(a, b) => ("=" |> bin_sg, [a, b])
//   | NotEq(a, b) => ("≠" |> bin_sg, [a, b])
//   | Lt(a, b) => ("<" |> bin_sg, [a, b])
//   | NotLt(a, b) => ("≥" |> bin_sg, [a, b])
//   | Gt(a, b) => (">" |> bin_sg, [a, b])
//   | NotGt(a, b) => ("≤" |> bin_sg, [a, b])
//   | Mem(p, ctx) => ("∈" |> bin_sg, [p, ctx])
//   | Subset(a, b) => ("⊆" |> bin_sg, [a, b])
//   };
// };

// let show = syntax =>
//   syntax |> repr |> Aba.join(Fun.id, Operation.show) |> String.concat("");

type res = list(failure);

let is_partial_correct: failure => option(specced) =
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

// require: res is not empty
let all_partial_correct: res => option(specced) =
  res => {
    let ss = List.map(is_partial_correct, res);
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
    // If premises number mismatch or there is any previous error, we don't run tests
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

// Debugging function
let __print_all_specs_and_tests = () => {
  Rule.all
  |> List.iter(rule => {
       let Spec.{concl, prems, tests} = of_spec(rule);
       List.iter(prem => print_endline("  " ++ Drv.Exp.show(prem)), prems);
       List.iter(
         test => print_endline("  {Test} " ++ show_test(test)),
         tests,
       );
       print_endline(
         "——————————————————————["
         ++ Rule.show(rule)
         ++ "]\n  "
         ++ Drv.Exp.show(concl)
         ++ "\n",
       );
     });
};

// Note(zhiyao): never mind

/**
  The following functions are utilized in the frontend to address the problem
  of representing a specific type of checking. For example, in the case
  of `E_Let`, the initial structure is as follows:

  Premises := [ e_def ⇓ v_def , e_body' ⇓ v' ]
  Conclusion := let x = e_def in e_body ⇓ v ]
  Tests := [ e_body' = [v_def/x]e_body ]

  To simplify definitions, we can convert the `Tests` into `Premises` by
  substituting `e_body'` with `[v_def/x]e_body`. The updated structure becomes:

  Premises := [ e_def ⇓ v_def , [v_def/x]e_body ⇓ v' ]
  Conclusion := let x = e_def in e_body ⇓ v
  Tests: []
 */;

// let spec_fill_eq_test: (RuleTest.test, Drv.Exp.t) => Drv.Exp.t =
//   fun
//   | Eq(Get(s'), op) =>
//     RuleSpec.map_reg(s => s == s' ? RuleTest.Operation.show(op) : s)
//   | _ => Fun.id;

// let spec_fill_eq_tests: (spec, tests) => spec =
//   List.fold_left(((concl, prems), test) =>
//     (
//       concl |> spec_fill_eq_test(test),
//       prems |> List.map(spec_fill_eq_test(test)),
//     )
//   );

// let tests_fill_eq_tests: tests => tests =
//   List.map(
//     fun
//     | RuleTest.Eq(Get(_), op) =>
//       RuleTest.Eq(Get(RuleTest.Operation.show(op)), op)
//     | _ as test => test,
//   );

// let fill_eq_tests: (spec, tests) => (spec, tests) =
//   (spec, tests) => (
//     spec_fill_eq_tests(spec, tests),
//     tests_fill_eq_tests(tests),
//   );

// let test_remove_eq_test: tests => tests =
//   List.filter(
//     fun
//     | RuleTest.Eq(Get(_), _) => false
//     | _ => true,
//   );
