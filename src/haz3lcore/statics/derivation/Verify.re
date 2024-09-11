open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  self: Prop.t,
  ghost: option(Prop.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type operation =
  | Neg(t)
  | Plus(t, t)
  | Minus(t, t)
  | Times(t, t)
  | Lt(t)
  | NotLt(t)
  | Gt(t)
  | NotGt(t)
  | Eq(t)
  | NotEq(t)
  | Subst((t, t), t)
  | Subst2((t, t), (t, t), t)
  | SubstTy((t, t), t)
  | Cons(t, t)
  | ConsHasTy((t, t), t)
  | ConsHasTy2((t, t), (t, t), t)
  | Mem(t)
  | MemHasTy(t, t);

let repr_operation: (t, operation) => string =
  (p, op) => {
    let repr = p => Prop.repr(p.self);
    switch (op) {
    | Neg(p') =>
      Printf.sprintf("Expect %s equal to -%s.", repr(p), repr(p'))
    | Plus(p1, p2) =>
      Printf.sprintf(
        "Expect %s equal to %s + %s.",
        repr(p),
        repr(p1),
        repr(p2),
      )
    | Minus(p1, p2) =>
      Printf.sprintf(
        "Expect %s equal to %s - %s.",
        repr(p),
        repr(p1),
        repr(p2),
      )
    | Times(p1, p2) =>
      Printf.sprintf(
        "Expect %s equal to %s * %s.",
        repr(p),
        repr(p1),
        repr(p2),
      )
    | Lt(p') => Printf.sprintf("Expect %s less than %s.", repr(p), repr(p'))
    | NotLt(p') =>
      Printf.sprintf("Expect %s not less than %s.", repr(p), repr(p'))
    | Gt(p') =>
      Printf.sprintf("Expect %s greater than %s.", repr(p), repr(p'))
    | NotGt(p') =>
      Printf.sprintf("Expect %s not greater than %s.", repr(p), repr(p'))
    | Eq(p') => Printf.sprintf("Expect %s equal to %s.", repr(p), repr(p'))
    | NotEq(p') =>
      Printf.sprintf("Expect %s not equal to %s.", repr(p), repr(p'))
    | Subst((v, x), p') =>
      Printf.sprintf(
        "Expect %s equal to %s with %s replaced by %s.",
        repr(p),
        repr(p'),
        repr(x),
        repr(v),
      )
    | Subst2((v1, x1), (v2, x2), p') =>
      Printf.sprintf(
        "Expect %s equal to %s with %s replaced by %s and %s replaced by %s.",
        repr(p),
        repr(p'),
        repr(x1),
        repr(v1),
        repr(x2),
        repr(v2),
      )
    | SubstTy((v, x), p') =>
      Printf.sprintf(
        "Expect %s equal to %s with %s replaced by %s.",
        repr(p),
        repr(p'),
        repr(x),
        repr(v),
      )
    | Cons(p1, p2) =>
      Printf.sprintf(
        "Expect %s equal to %s extended by %s.",
        repr(p),
        repr(p1),
        repr(p2),
      )
    | ConsHasTy((x, t), p') =>
      Printf.sprintf(
        "Expect %s equal to %s extended by %s : %s.",
        repr(p),
        repr(p'),
        repr(x),
        repr(t),
      )
    | ConsHasTy2((x1, t1), (x2, t2), p') =>
      Printf.sprintf(
        "Expect %s equal to %s extended by %s : %s, %s : %s.",
        repr(p),
        repr(p'),
        repr(x1),
        repr(t1),
        repr(x2),
        repr(t2),
      )
    | Mem(p') =>
      Printf.sprintf("Expect %s to be a member of %s.", repr(p'), repr(p))
    | MemHasTy(p', t) =>
      Printf.sprintf(
        "Expect %s : %s to be a member of %s.",
        repr(p'),
        repr(t),
        repr(p),
      )
    };
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type failure =
  | PremiseMismatch(int, int) /* expected, actual */
  | FailUnbox(Prop.cls, t)
  | NotAList(t) // Should be caught by the typechecker. Not expected to happen
  | NotEqual(t, t)
  | FailTest(t, operation); // expect {1} equal to {2}

let repr_failure: failure => string =
  fun
  | PremiseMismatch(e, a) =>
    Printf.sprintf("Expected %d premise(s), but got %d.", e, a)
  | FailUnbox(cls, p) =>
    Printf.sprintf(
      "Failed to unbox %s from %s.",
      Prop.show_cls(cls),
      Prop.repr(p.self),
    )
  | NotAList(p) =>
    Printf.sprintf("Expected a list, but got %s.", Prop.repr(p.self))
  | NotEqual(p1, p2) =>
    Printf.sprintf(
      "Expected %s equal to %s.",
      Prop.repr(p1.self),
      Prop.repr(p2.self),
    )
  | FailTest(p, op) => repr_operation(p, op);

type req('a) =
  // Top-level
  | Get: req(t) // get term
  | TestEq(t): req(unit) // test term equality
  | Test(operation): req(unit) // check term
  // ALFA Typ
  | Num: req(unit)
  | Bool: req(unit)
  | Arrow(req('a), req('b)): req(('a, 'b))
  | Prod(req('a), req('b)): req(('a, 'b))
  | Unit: req(unit)
  | Sum(req('a), req('b)): req(('a, 'b))
  | TVar: req(string)
  | Rec(req('a), req('b)): req(('a, 'b))
  // ALFA Exp
  | NumLit: req(int)
  | UnOp(req('a), req('b)): req(('a, 'b))
  | BinOp(req('a), req('b), req('c)): req(('a, 'b, 'c))
  | True: req(unit)
  | False: req(unit)
  | If(req('a), req('b), req('c)): req(('a, 'b, 'c))
  | Var: req(string)
  | Let(req('a), req('b), req('c)): req(('a, 'b, 'c))
  | LetAnn(req('a), req('b), req('c), req('d)): req(('a, 'b, 'c, 'd))
  | Fix(req('a), req('b)): req(('a, 'b))
  | FixAnn(req('a), req('b), req('c)): req(('a, 'b, 'c))
  | Fun(req('a), req('b)): req(('a, 'b))
  | FunAnn(req('a), req('b), req('c)): req(('a, 'b, 'c))
  | Ap(req('a), req('b)): req(('a, 'b))
  | Pair(req('a), req('b)): req(('a, 'b))
  | Triv: req(unit)
  | PrjL(req('a)): req('a)
  | PrjR(req('a)): req('a)
  | LetPair(req('a), req('b), req('c), req('d)): req(('a, 'b, 'c, 'd))
  | InjL(req('a)): req('a)
  | InjR(req('a)): req('a)
  | Case(req('a), req('b), req('c), req('d), req('e))
    : req(('a, 'b, 'c, 'd, 'e))
  | Roll(req('a)): req('a)
  | Unroll(req('a)): req('a)
  // ALFA UnOp
  | OpNeg: req(unit)
  // ALFA BinOp
  | OpPlus: req(unit)
  | OpMinus: req(unit)
  | OpTimes: req(unit)
  | OpLt: req(unit)
  | OpGt: req(unit)
  | OpEq: req(unit)
  // ALFA Meta
  | TPat: req(string)
  | Pat: req(string)
  // ALFA Proposition
  | HasTy(req('a), req('b)): req(('a, 'b))
  | Syn(req('a), req('b)): req(('a, 'b))
  | Ana(req('a), req('b)): req(('a, 'b))
  // Logical Proposition
  | And(req('a), req('b)): req(('a, 'b))
  | Or(req('a), req('b)): req(('a, 'b))
  | Impl(req('a), req('b)): req(('a, 'b))
  | Truth: req(unit)
  | Falsity: req(unit)
  // Judgments
  | Val(req('a)): req('a)
  | Eval(req('a), req('b)): req(('a, 'b))
  | Entail(req('a), req('b)): req(('a, 'b));

let (let$) = (x, f) =>
  switch (x) {
  | Ok(x) => f(x)
  | Error(e) => Error(e)
  };


let rec unbox: type a. (t, req(a)) => result(a, failure) =
  ({self, ghost}, req) => {
    switch (ghost) {
    | None => unbox_self(self, req)
    | Some(ghost) => unbox_with_ghost(self, ghost, req)
    };
  }

and unbox_self: type a. (Prop.t, req(a)) => result(a, failure) =
  (self, req) => {
    let p = {self, ghost: None};
    let unbox = self => unbox({self, ghost: None});
    switch (req, IdTagged.term_of(p.self)) {
    // Top-level
    | (Get, _) => Ok(p)
    | (TestEq(p'), _) =>
      Prop.eq(p'.self, p.self) ? Ok() : Error(NotEqual(p, p'))
    | (Test(op), _) =>
      let$ result = check(p, op);
      result ? Ok() : Error(FailTest(p, op));
    | (Sum(ra, rb), Sum(a, b)) =>
      let$ a = unbox(a, ra);
      let$ b = unbox(b, rb);
      Ok((a, b));
    | (Sum(_), _) => Error(FailUnbox(Sum, p))
    | (TVar, TVar(s)) => Ok(s)
    | (TVar, _) => Error(FailUnbox(TVar, p))
    | (Rec(ra, rb), Rec(a, b)) =>
      let$ a = unbox(a, ra);
      let$ b = unbox(b, rb);
      Ok((a, b));
    | (Rec(_), _) => Error(FailUnbox(Rec, p))
    | (TPat, TPat(s)) => Ok(s)
    | (TPat, _) => Error(FailUnbox(TPat, p))
    | (Fix(ra, rb), Fix(a, b)) =>
      let$ a = unbox(a, ra);
      let$ b = unbox(b, rb);
      Ok((a, b));
    | (Fix(_), _) => Error(FailUnbox(Fix, p))
    | (InjL(ra), InjL(a)) =>
      let$ a = unbox(a, ra);
      Ok(a);
    | (FixAnn(ra, rb, rc), FixAnn(a, b, c)) =>
      let$ a = unbox(a, ra);
      let$ b = unbox(b, rb);
      let$ c = unbox(c, rc);
      Ok((a, b, c));
    | (FixAnn(_), _) => Error(FailUnbox(FixAnn, p))
    | (InjL(_), _) => Error(FailUnbox(InjL, p))
    | (InjR(ra), InjR(a)) =>
      let$ a = unbox(a, ra);
      Ok(a);
    | (InjR(_), _) => Error(FailUnbox(InjR, p))
    | (Case(ra, rb, rc, rd, re), Case(a, b, c, d, e)) =>
      let$ a = unbox(a, ra);
      let$ b = unbox(b, rb);
      let$ c = unbox(c, rc);
      let$ d = unbox(d, rd);
      let$ e = unbox(e, re);
      Ok((a, b, c, d, e));
    | (Case(_), _) => Error(FailUnbox(Case, p))
    | (Roll(ra), Roll(a)) =>
      let$ a = unbox(a, ra);
      Ok(a);
    | (Roll(_), _) => Error(FailUnbox(Roll, p))
    | (Unroll(ra), Unroll(a)) =>
      let$ a = unbox(a, ra);
      Ok(a);
    | (Unroll(_), _) => Error(FailUnbox(Unroll, p))
    // ALFp
    | (Num, Num) => Ok()
    | (Num, _) => Error(FailUnbox(Num, p))
    | (Bool, Bool) => Ok()
    | (Bool, _) => Error(FailUnbox(Bool, p))
    | (Arrow(ra, rb), Arrow(a, b)) =>
      let$ a = unbox(a, ra);
      let$ b = unbox(b, rb);
      Ok((a, b));
    | (Arrow(_), _) => Error(FailUnbox(Arrow, p))
    | (Prod(ra, rb), Prod(a, b)) =>
      let$ a = unbox(a, ra);
      let$ b = unbox(b, rb);
      Ok((a, b));
    | (Prod(_), _) => Error(FailUnbox(Prod, p))
    | (Unit, Unit) => Ok()
    | (Unit, _) => Error(FailUnbox(Unit, p))
    | (Pair(ra, rb), Pair(a, b)) =>
      let$ a = unbox(a, ra);
      let$ b = unbox(b, rb);
      Ok((a, b));
    | (Pair(_), _) => Error(FailUnbox(Pair, p))
    | (LetPair(ra, rb, rc, rd), LetPair(a, b, c, d)) =>
      let$ a = unbox(a, ra);
      let$ b = unbox(b, rb);
      let$ c = unbox(c, rc);
      let$ d = unbox(d, rd);
      Ok((a, b, c, d));
    | (LetPair(_), _) => Error(FailUnbox(LetPair, p))
    | (PrjL(ra), PrjL(a)) =>
      let$ a = unbox(a, ra);
      Ok(a);
    | (PrjL(_), _) => Error(FailUnbox(PrjL, p))
    | (PrjR(ra), PrjR(a)) =>
      let$ a = unbox(a, ra);
      Ok(a);
    | (PrjR(_), _) => Error(FailUnbox(PrjR, p))
    | (Triv, Triv) => Ok()
    | (Triv, _) => Error(FailUnbox(Triv, p))
    | (Pat, Pat(s)) => Ok(s)
    | (Pat, _) => Error(FailUnbox(Pat, p))
    // ALF
    | (True, True) => Ok()
    | (True, _) => Error(FailUnbox(True, p))
    | (False, False) => Ok()
    | (False, _) => Error(FailUnbox(False, p))
    | (If(ra, rb, rc), If(a, b, c)) =>
      let$ a = unbox(a, ra);
      let$ b = unbox(b, rb);
      let$ c = unbox(c, rc);
      Ok((a, b, c));
    | (If(_), _) => Error(FailUnbox(If, p))
    | (Var, Var(s)) => Ok(s)
    | (Var, _) => Error(FailUnbox(Var, p))
    | (Let(ra, rb, rc), Let(a, b, c)) =>
      let$ a = unbox(a, ra);
      let$ b = unbox(b, rb);
      let$ c = unbox(c, rc);
      Ok((a, b, c));
    | (Let(_), _) => Error(FailUnbox(Let, p))
    | (LetAnn(ra, rb, rc, rd), LetAnn(a, b, c, d)) =>
      let$ a = unbox(a, ra);
      let$ b = unbox(b, rb);
      let$ c = unbox(c, rc);
      let$ d = unbox(d, rd);
      Ok((a, b, c, d));
    | (LetAnn(_), _) => Error(FailUnbox(LetAnn, p))
    | (Fun(ra, rb), Fun(a, b)) =>
      let$ a = unbox(a, ra);
      let$ b = unbox(b, rb);
      Ok((a, b));
    | (Fun(_), _) => Error(FailUnbox(Fun, p))
    | (FunAnn(ra, rb, rc), FunAnn(a, b, c)) =>
      let$ a = unbox(a, ra);
      let$ b = unbox(b, rb);
      let$ c = unbox(c, rc);
      Ok((a, b, c));
    | (FunAnn(_), _) => Error(FailUnbox(FunAnn, p))
    | (Ap(ra, rb), Ap(a, b)) =>
      let$ a = unbox(a, ra);
      let$ b = unbox(b, rb);
      Ok((a, b));
    | (Ap(_), _) => Error(FailUnbox(Ap, p))
    | (OpLt, OpLt) => Ok()
    | (OpLt, _) => Error(FailUnbox(OpLt, p))
    | (OpGt, OpGt) => Ok()
    | (OpGt, _) => Error(FailUnbox(OpGt, p))
    | (OpEq, OpEq) => Ok()
    | (OpEq, _) => Error(FailUnbox(OpEq, p))
    // AL
    | (NumLit, NumLit(n)) => Ok(n)
    | (NumLit, _) => Error(FailUnbox(NumLit, p))
    | (UnOp(ra, rb), UnOp(a, b)) =>
      let$ a = unbox(a, ra);
      let$ b = unbox(b, rb);
      Ok((a, b));
    | (UnOp(_), _) => Error(FailUnbox(UnOp, p))
    | (BinOp(ra, rb, rc), BinOp(a, b, c)) =>
      let$ a = unbox(a, ra);
      let$ b = unbox(b, rb);
      let$ c = unbox(c, rc);
      Ok((a, b, c));
    | (BinOp(_), _) => Error(FailUnbox(BinOp, p))
    | (OpNeg, OpNeg) => Ok()
    | (OpNeg, _) => Error(FailUnbox(OpNeg, p))
    | (OpPlus, OpPlus) => Ok()
    | (OpPlus, _) => Error(FailUnbox(OpPlus, p))
    | (OpMinus, OpMinus) => Ok()
    | (OpMinus, _) => Error(FailUnbox(OpMinus, p))
    | (OpTimes, OpTimes) => Ok()
    | (OpTimes, _) => Error(FailUnbox(OpTimes, p))
    // ALFA proposition
    | (HasTy(ra, rb), HasTy(a, b)) =>
      let$ a = unbox(a, ra);
      let$ b = unbox(b, rb);
      Ok((a, b));
    | (HasTy(_), _) => Error(FailUnbox(HasTy, p))
    | (Syn(ra, rb), Syn(a, b)) =>
      let$ a = unbox(a, ra);
      let$ b = unbox(b, rb);
      Ok((a, b));
    | (Syn(_), _) => Error(FailUnbox(Syn, p))
    | (Ana(ra, rb), Ana(a, b)) =>
      let$ a = unbox(a, ra);
      let$ b = unbox(b, rb);
      Ok((a, b));
    | (Ana(_), _) => Error(FailUnbox(Ana, p))
    // Propositional logic
    | (And(ra, rb), And(a, b)) =>
      let$ a = unbox(a, ra);
      let$ b = unbox(b, rb);
      Ok((a, b));
    | (And(_), _) => Error(FailUnbox(And, p))
    | (Or(ra, rb), Or(a, b)) =>
      let$ a = unbox(a, ra);
      let$ b = unbox(b, rb);
      Ok((a, b));
    | (Or(_), _) => Error(FailUnbox(Or, p))
    | (Impl(ra, rb), Impl(a, b)) =>
      let$ a = unbox(a, ra);
      let$ b = unbox(b, rb);
      Ok((a, b));
    | (Impl(_), _) => Error(FailUnbox(Impl, p))
    | (Truth, Truth) => Ok()
    | (Truth, _) => Error(FailUnbox(Truth, p))
    | (Falsity, Falsity) => Ok()
    | (Falsity, _) => Error(FailUnbox(Falsity, p))
    // Judgments
    | (Val(ra), Val(a)) =>
      let$ a = unbox(a, ra);
      Ok(a);
    | (Val(_), _) => Error(FailUnbox(Val, p))
    | (Eval(ra, rb), Eval(a, b)) =>
      let$ a = unbox(a, ra);
      let$ b = unbox(b, rb);
      Ok((a, b));
    | (Eval(_), _) => Error(FailUnbox(Eval, p))
    | (Entail(ra, rb), Entail(a, b)) =>
      let$ a = unbox(a, ra);
      let$ b = unbox(b, rb);
      Ok((a, b));
    | (Entail(_), _) => Error(FailUnbox(Entail, p))
    };
  }

and unbox_with_ghost: type a. (Prop.t, Prop.t, req(a)) => result(a, failure) =
  (self, ghost, req) => {
    let p = {self, ghost: Some(ghost)};
    let unbox = (self, ghost) => unbox({self, ghost: Some(ghost)});
    switch (req, IdTagged.term_of(self), IdTagged.term_of(ghost)) {
    // Top-level
    | (Get, _, _) => Ok(p)
    | (TestEq(p'), _, _) =>
      // p' should also have ghost
      Prop.eq(p'.self, p.self) && Prop.eq(p'.ghost |> Option.get, ghost)
        ? Ok() : Error(NotEqual(p, p'))
    | (Test(op), _, _) =>
      let$ result = check(p, op);
      result ? Ok() : Error(FailTest(p, op));
    | (Sum(ra, rb), Sum(a, b), Sum(a', b')) =>
      let$ a = unbox(a, a', ra);
      let$ b = unbox(b, b', rb);
      Ok((a, b));
    | (Sum(_), _, _) => Error(FailUnbox(Sum, p))
    | (TVar, TVar(s), TVar(_)) => Ok(s)
    | (TVar, _, _) => Error(FailUnbox(TVar, p))
    | (Rec(ra, rb), Rec(a, b), Rec(a', b')) =>
      let$ a = unbox(a, a', ra);
      let$ b = unbox(b, b', rb);
      Ok((a, b));
    | (Rec(_), _, _) => Error(FailUnbox(Rec, p))
    | (TPat, TPat(s), TPat(_)) => Ok(s)
    | (TPat, _, _) => Error(FailUnbox(TPat, p))
    | (Fix(ra, rb), Fix(a, b), Fix(a', b')) =>
      let$ a = unbox(a, a', ra);
      let$ b = unbox(b, b', rb);
      Ok((a, b));
    | (Fix(_), _, _) => Error(FailUnbox(Fix, p))
    | (FixAnn(ra, rb, rc), FixAnn(a, b, c), FixAnn(a', b', c')) =>
      let$ a = unbox(a, a', ra);
      let$ b = unbox(b, b', rb);
      let$ c = unbox(c, c', rc);
      Ok((a, b, c));
    | (FixAnn(_), _, _) => Error(FailUnbox(FixAnn, p))
    | (InjL(ra), InjL(a), InjL(a')) =>
      let$ a = unbox(a, a', ra);
      Ok(a);
    | (InjL(_), _, _) => Error(FailUnbox(InjL, p))
    | (InjR(ra), InjR(a), InjR(a')) =>
      let$ a = unbox(a, a', ra);
      Ok(a);
    | (InjR(_), _, _) => Error(FailUnbox(InjR, p))
    | (
        Case(ra, rb, rc, rd, re),
        Case(a, b, c, d, e),
        Case(a', b', c', d', e'),
      ) =>
      let$ a = unbox(a, a', ra);
      let$ b = unbox(b, b', rb);
      let$ c = unbox(c, c', rc);
      let$ d = unbox(d, d', rd);
      let$ e = unbox(e, e', re);
      Ok((a, b, c, d, e));
    | (Case(_), _, _) => Error(FailUnbox(Case, p))
    | (Roll(ra), Roll(a), Roll(a')) =>
      let$ a = unbox(a, a', ra);
      Ok(a);
    | (Roll(_), _, _) => Error(FailUnbox(Roll, p))
    | (Unroll(ra), Unroll(a), Unroll(a')) =>
      let$ a = unbox(a, a', ra);
      Ok(a);
    | (Unroll(_), _, _) => Error(FailUnbox(Unroll, p))
    // ALFp
    | (Num, Num, Num) => Ok()
    | (Num, _, _) => Error(FailUnbox(Num, p))
    | (Bool, Bool, Bool) => Ok()
    | (Bool, _, _) => Error(FailUnbox(Bool, p))
    | (Arrow(ra, rb), Arrow(a, b), Arrow(a', b')) =>
      let$ a = unbox(a, a', ra);
      let$ b = unbox(b, b', rb);
      Ok((a, b));
    | (Arrow(_), _, _) => Error(FailUnbox(Arrow, p))
    | (Prod(ra, rb), Prod(a, b), Prod(a', b')) =>
      let$ a = unbox(a, a', ra);
      let$ b = unbox(b, b', rb);
      Ok((a, b));
    | (Prod(_), _, _) => Error(FailUnbox(Prod, p))
    | (Unit, Unit, Unit) => Ok()
    | (Unit, _, _) => Error(FailUnbox(Unit, p))
    | (Pair(ra, rb), Pair(a, b), Pair(a', b')) =>
      let$ a = unbox(a, a', ra);
      let$ b = unbox(b, b', rb);
      Ok((a, b));
    | (Pair(_), _, _) => Error(FailUnbox(Pair, p))
    | (
        LetPair(ra, rb, rc, rd),
        LetPair(a, b, c, d),
        LetPair(a', b', c', d'),
      ) =>
      let$ a = unbox(a, a', ra);
      let$ b = unbox(b, b', rb);
      let$ c = unbox(c, c', rc);
      let$ d = unbox(d, d', rd);
      Ok((a, b, c, d));
    | (LetPair(_), _, _) => Error(FailUnbox(LetPair, p))
    | (PrjL(ra), PrjL(a), PrjL(a')) =>
      let$ a = unbox(a, a', ra);
      Ok(a);
    | (PrjL(_), _, _) => Error(FailUnbox(PrjL, p))
    | (PrjR(ra), PrjR(a), PrjR(a')) =>
      let$ a = unbox(a, a', ra);
      Ok(a);
    | (PrjR(_), _, _) => Error(FailUnbox(PrjR, p))
    | (Triv, Triv, Triv) => Ok()
    | (Triv, _, _) => Error(FailUnbox(Triv, p))
    | (Pat, Pat(s), Pat(_)) => Ok(s)
    | (Pat, _, _) => Error(FailUnbox(Pat, p))
    // ALF
    | (True, True, True) => Ok()
    | (True, _, _) => Error(FailUnbox(True, p))
    | (False, False, False) => Ok()
    | (False, _, _) => Error(FailUnbox(False, p))
    | (If(ra, rb, rc), If(a, b, c), If(a', b', c')) =>
      let$ a = unbox(a, a', ra);
      let$ b = unbox(b, b', rb);
      let$ c = unbox(c, c', rc);
      Ok((a, b, c));
    | (If(_), _, _) => Error(FailUnbox(If, p))
    | (Var, Var(s), Var(_)) => Ok(s)
    | (Var, _, _) => Error(FailUnbox(Var, p))
    | (Let(ra, rb, rc), Let(a, b, c), Let(a', b', c')) =>
      let$ a = unbox(a, a', ra);
      let$ b = unbox(b, b', rb);
      let$ c = unbox(c, c', rc);
      Ok((a, b, c));
    | (Let(_), _, _) => Error(FailUnbox(Let, p))
    | (LetAnn(ra, rb, rc, rd), LetAnn(a, b, c, d), LetAnn(a', b', c', d')) =>
      let$ a = unbox(a, a', ra);
      let$ b = unbox(b, b', rb);
      let$ c = unbox(c, c', rc);
      let$ d = unbox(d, d', rd);
      Ok((a, b, c, d));
    | (LetAnn(_), _, _) => Error(FailUnbox(LetAnn, p))
    | (Fun(ra, rb), Fun(a, b), Fun(a', b')) =>
      let$ a = unbox(a, a', ra);
      let$ b = unbox(b, b', rb);
      Ok((a, b));
    | (Fun(_), _, _) => Error(FailUnbox(Fun, p))
    | (FunAnn(ra, rb, rc), FunAnn(a, b, c), FunAnn(a', b', c')) =>
      let$ a = unbox(a, a', ra);
      let$ b = unbox(b, b', rb);
      let$ c = unbox(c, c', rc);
      Ok((a, b, c));
    | (FunAnn(_), _, _) => Error(FailUnbox(FunAnn, p))
    | (Ap(ra, rb), Ap(a, b), Ap(a', b')) =>
      let$ a = unbox(a, a', ra);
      let$ b = unbox(b, b', rb);
      Ok((a, b));
    | (Ap(_), _, _) => Error(FailUnbox(Ap, p))
    | (OpLt, OpLt, OpLt) => Ok()
    | (OpLt, _, _) => Error(FailUnbox(OpLt, p))
    | (OpGt, OpGt, OpGt) => Ok()
    | (OpGt, _, _) => Error(FailUnbox(OpGt, p))
    | (OpEq, OpEq, OpEq) => Ok()
    | (OpEq, _, _) => Error(FailUnbox(OpEq, p))
    // AL
    | (NumLit, NumLit(n), NumLit(_)) => Ok(n)
    | (NumLit, _, _) => Error(FailUnbox(NumLit, p))
    | (UnOp(ra, rb), UnOp(a, b), UnOp(a', b')) =>
      let$ a = unbox(a, a', ra);
      let$ b = unbox(b, b', rb);
      Ok((a, b));
    | (UnOp(_), _, _) => Error(FailUnbox(UnOp, p))
    | (BinOp(ra, rb, rc), BinOp(a, b, c), BinOp(a', b', c')) =>
      let$ a = unbox(a, a', ra);
      let$ b = unbox(b, b', rb);
      let$ c = unbox(c, c', rc);
      Ok((a, b, c));
    | (BinOp(_), _, _) => Error(FailUnbox(BinOp, p))
    | (OpNeg, OpNeg, OpNeg) => Ok()
    | (OpNeg, _, _) => Error(FailUnbox(OpNeg, p))
    | (OpPlus, OpPlus, OpPlus) => Ok()
    | (OpPlus, _, _) => Error(FailUnbox(OpPlus, p))
    | (OpMinus, OpMinus, OpMinus) => Ok()
    | (OpMinus, _, _) => Error(FailUnbox(OpMinus, p))
    | (OpTimes, OpTimes, OpTimes) => Ok()
    | (OpTimes, _, _) => Error(FailUnbox(OpTimes, p))
    // ALFA proposition
    | (HasTy(ra, rb), HasTy(a, b), HasTy(a', b')) =>
      let$ a = unbox(a, a', ra);
      let$ b = unbox(b, b', rb);
      Ok((a, b));
    | (HasTy(_), _, _) => Error(FailUnbox(HasTy, p))
    | (Syn(ra, rb), Syn(a, b), Syn(a', b')) =>
      let$ a = unbox(a, a', ra);
      let$ b = unbox(b, b', rb);
      Ok((a, b));
    | (Syn(_), _, _) => Error(FailUnbox(Syn, p))
    | (Ana(ra, rb), Ana(a, b), Ana(a', b')) =>
      let$ a = unbox(a, a', ra);
      let$ b = unbox(b, b', rb);
      Ok((a, b));
    | (Ana(_), _, _) => Error(FailUnbox(Ana, p))
    // Propositional logic
    | (And(ra, rb), And(a, b), And(a', b')) =>
      let$ a = unbox(a, a', ra);
      let$ b = unbox(b, b', rb);
      Ok((a, b));
    | (And(_), _, _) => Error(FailUnbox(And, p))
    | (Or(ra, rb), Or(a, b), Or(a', b')) =>
      let$ a = unbox(a, a', ra);
      let$ b = unbox(b, b', rb);
      Ok((a, b));
    | (Or(_), _, _) => Error(FailUnbox(Or, p))
    | (Impl(ra, rb), Impl(a, b), Impl(a', b')) =>
      let$ a = unbox(a, a', ra);
      let$ b = unbox(b, b', rb);
      Ok((a, b));
    | (Impl(_), _, _) => Error(FailUnbox(Impl, p))
    | (Truth, Truth, Truth) => Ok()
    | (Truth, _, _) => Error(FailUnbox(Truth, p))
    | (Falsity, Falsity, Falsity) => Ok()
    | (Falsity, _, _) => Error(FailUnbox(Falsity, p))
    // Judgments
    | (Val(ra), Val(a), Val(a')) =>
      let$ a = unbox(a, a', ra);
      Ok(a);
    | (Val(_), _, _) => Error(FailUnbox(Val, p))
    | (Eval(ra, rb), Eval(a, b), Eval(a', b')) =>
      let$ a = unbox(a, a', ra);
      let$ b = unbox(b, b', rb);
      Ok((a, b));
    | (Eval(_), _, _) => Error(FailUnbox(Eval, p))
    | (Entail(ra, rb), Entail(a, b), Entail(a', b')) =>
      let$ a = unbox(a, a', ra);
      let$ b = unbox(b, b', rb);
      Ok((a, b));
    | (Entail(_), _, _) => Error(FailUnbox(Entail, p))
    };
  }

and check: (t, operation) => result(bool, failure) =
  ({self, _} as p, op) => {
    let unbox_list = p => {
      switch (IdTagged.term_of(p.self)) {
      | Ctx(l) => Ok(l)
      | _ => Error(NotAList(p))
      };
    };
    let unbox_num2 = (p1, p2) => {
      let$ n1 = unbox(p1, NumLit);
      let$ n2 = unbox(p2, NumLit);
      Ok((n1, n2));
    };
    let unbox_num3 = (p1, p2, p3) => {
      let$ n1 = unbox(p1, NumLit);
      let$ n2 = unbox(p2, NumLit);
      let$ n3 = unbox(p3, NumLit);
      Ok((n1, n2, n3));
    };
    let unbox_pat = x => {
      let$ x = unbox(x, Pat);
      Ok(Var(x) |> Prop.temp);
    };
    switch (op) {
    | Neg(p') =>
      let$ (n, n') = unbox_num2(p, p');
      Ok(n == - n');
    | Plus(p1, p2) =>
      let$ (n, n1, n2) = unbox_num3(p, p1, p2);
      Ok(n == n1 + n2);
    | Minus(p1, p2) =>
      let$ (n, n1, n2) = unbox_num3(p, p1, p2);
      Ok(n == n1 - n2);
    | Times(p1, p2) =>
      let$ (n, n1, n2) = unbox_num3(p, p1, p2);
      Ok(n == n1 * n2);
    | Lt(p') =>
      let$ (n, n') = unbox_num2(p, p');
      Ok(n > n');
    | NotLt(p') =>
      let$ (n, n') = unbox_num2(p, p');
      Ok(n <= n');
    | Gt(p') =>
      let$ (n, n') = unbox_num2(p, p');
      Ok(n < n');
    | NotGt(p') =>
      let$ (n, n') = unbox_num2(p, p');
      Ok(n >= n');
    | Eq(p') =>
      let$ (n, n') = unbox_num2(p, p');
      Ok(n == n');
    | NotEq(p') =>
      let$ (n, n') = unbox_num2(p, p');
      Ok(n != n');
    | Subst((v, x), e) =>
      let$ x = unbox(x, Pat);
      Ok(Prop.eq(self, Prop.subst(v.self, x, e.self)));
    | Subst2((v1, x1), (v2, x2), e) =>
      let$ x1 = unbox(x1, Pat);
      let$ x2 = unbox(x2, Pat);
      Ok(
        Prop.eq(
          self,
          Prop.subst(v2.self, x2, Prop.subst(v1.self, x1, e.self)),
        ),
      );
    | SubstTy((t, a), e) =>
      let$ a = unbox(a, TVar);
      Ok(Prop.eq(self, Prop.subst_ty(t.self, a, e.self)));
    | Cons(e, l) =>
      let$ l = unbox_list(l);
      let l = Ctx(Prop.cons_ctx(e.self, l)) |> Prop.temp;
      Ok(Prop.eq(self, l));
    | ConsHasTy((pat, t), l) =>
      let$ l = unbox_list(l);
      let$ x = unbox_pat(pat);
      let l =
        Ctx(Prop.cons_ctx(HasTy(x, t.self) |> Prop.temp, l)) |> Prop.temp;
      if (!Prop.eq(self, l)) {
        print_endline(Prop.show(self));
        print_endline(Prop.show(l));
      };
      Ok(Prop.eq(self, l));
    | ConsHasTy2((pat1, t1), (pat2, t2), l) =>
      let$ l = unbox_list(l);
      let$ x1 = unbox_pat(pat1);
      let$ x2 = unbox_pat(pat2);
      let l = Prop.cons_ctx(HasTy(x1, t1.self) |> Prop.temp, l);
      let l =
        Ctx(Prop.cons_ctx(HasTy(x2, t2.self) |> Prop.temp, l)) |> Prop.temp;
      if (!Prop.eq(self, l)) {
        print_endline(Prop.show(self));
        print_endline(Prop.show(l));
      };
      Ok(Prop.eq(self, l));
    | Mem(e) =>
      let$ l = unbox_list(p);
      Ok(Prop.mem_ctx(e.self, l));
    | MemHasTy(x, t) =>
      let$ l = unbox_list(p);
      Ok(Prop.mem_ctx(HasTy(x.self, t.self) |> Prop.temp, l));
    };
  };

let expect_prems_num: (Rule.t, list(t)) => result(int => t, failure) =
  (rule, prems) => {
    let got = List.length(prems);
    let expect = Rule.prems_num(rule);
    expect == got
      ? Ok(x => List.nth(prems, x)) : Error(PremiseMismatch(expect, got));
  };

let verify = (rule: Rule.t, prems: list(t), concl: t): result(unit, failure) => {
  let$ prems = expect_prems_num(rule, prems);
  // The following symbols / operators are defined for convenience just
  // under this function.
  let __ = Get;
  let (!) = x => TestEq(x);
  let (!!) = x => Test(x);
  let (>>) = unbox;
  switch (rule) {
  // ALFA
  | A_Subsumption =>
    let$ (ctx, (e, t)) = prems(0) >> Entail(__, Syn(__, __));
    let$ _ = concl >> Entail(!ctx, Ana(!e, !t));
    Ok();
  | E_Val =>
    let$ v = prems(0) >> Val(__);
    let$ _ = concl >> Eval(!v, !v);
    Ok();
  | S_Num =>
    let$ _ = concl >> Entail(__, Syn(NumLit, Num));
    Ok();
  | T_Num =>
    let$ _ = concl >> Entail(__, HasTy(NumLit, Num));
    Ok();
  | V_Num =>
    let$ _ = concl >> Val(NumLit);
    Ok();
  | S_True =>
    let$ _ = concl >> Entail(__, Syn(True, Bool));
    Ok();
  | T_True =>
    let$ _ = concl >> Entail(__, HasTy(True, Bool));
    Ok();
  | V_True =>
    let$ _ = concl >> Val(True);
    Ok();
  | S_False =>
    let$ _ = concl >> Entail(__, Syn(False, Bool));
    Ok();
  | T_False =>
    let$ _ = concl >> Entail(__, HasTy(False, Bool));
    Ok();
  | V_False =>
    let$ _ = concl >> Val(False);
    Ok();
  | S_Triv =>
    let$ _ = concl >> Entail(__, Syn(Triv, Unit));
    Ok();
  | T_Triv =>
    let$ _ = concl >> Entail(__, HasTy(Triv, Unit));
    Ok();
  | V_Triv =>
    let$ _ = concl >> Val(Triv);
    Ok();
  | S_Neg =>
    let$ (ctx, (e, _)) = prems(0) >> Entail(__, Syn(__, Num));
    let$ _ = concl >> Entail(!ctx, Syn(UnOp(OpNeg, !e), Num));
    Ok();
  | T_Neg =>
    let$ (ctx, (e, _)) = prems(0) >> Entail(__, HasTy(__, Num));
    let$ _ = concl >> Entail(!ctx, HasTy(UnOp(OpNeg, !e), Num));
    Ok();
  | E_Neg =>
    let$ (e, v) = prems(0) >> Eval(__, __);
    let$ _ = concl >> Eval(UnOp(OpNeg, !e), !!Neg(v));
    Ok();
  | S_Plus =>
    let$ (ctx, (e1, _)) = prems(0) >> Entail(__, Ana(__, Num));
    let$ (_, (e2, _)) = prems(1) >> Entail(!ctx, Ana(__, Num));
    let$ _ = concl >> Entail(!ctx, Syn(BinOp(OpPlus, !e1, !e2), Num));
    Ok();
  | T_Plus =>
    let$ (ctx, (e1, _)) = prems(0) >> Entail(__, HasTy(__, Num));
    let$ (_, (e2, _)) = prems(1) >> Entail(!ctx, HasTy(__, Num));
    let$ _ = concl >> Entail(!ctx, HasTy(BinOp(OpPlus, !e1, !e2), Num));
    Ok();
  | E_Plus =>
    let$ (e1, v1) = prems(0) >> Eval(__, __);
    let$ (e2, v2) = prems(1) >> Eval(__, __);
    let$ _ = concl >> Eval(BinOp(OpPlus, !e1, !e2), !!Plus(v1, v2));
    Ok();
  | S_Minus =>
    let$ (ctx, (e1, _)) = prems(0) >> Entail(__, Ana(__, Num));
    let$ (_, (e2, _)) = prems(1) >> Entail(!ctx, Ana(__, Num));
    let$ _ = concl >> Entail(!ctx, Syn(BinOp(OpMinus, !e1, !e2), Num));
    Ok();
  | T_Minus =>
    let$ (ctx, (e1, _)) = prems(0) >> Entail(__, HasTy(__, Num));
    let$ (_, (e2, _)) = prems(1) >> Entail(!ctx, HasTy(__, Num));
    let$ _ = concl >> Entail(!ctx, HasTy(BinOp(OpMinus, !e1, !e2), Num));
    Ok();
  | E_Minus =>
    let$ (e1, v1) = prems(0) >> Eval(__, __);
    let$ (e2, v2) = prems(1) >> Eval(__, __);
    let$ _ = concl >> Eval(BinOp(OpMinus, !e1, !e2), !!Minus(v1, v2));
    Ok();
  | S_Times =>
    let$ (ctx, (e1, _)) = prems(0) >> Entail(__, Ana(__, Num));
    let$ (_, (e2, _)) = prems(1) >> Entail(!ctx, Ana(__, Num));
    let$ _ = concl >> Entail(!ctx, Syn(BinOp(OpTimes, !e1, !e2), Num));
    Ok();
  | T_Times =>
    let$ (ctx, (e1, _)) = prems(0) >> Entail(__, HasTy(__, Num));
    let$ (_, (e2, _)) = prems(1) >> Entail(!ctx, HasTy(__, Num));
    let$ _ = concl >> Entail(!ctx, HasTy(BinOp(OpTimes, !e1, !e2), Num));
    Ok();
  | E_Times =>
    let$ (e1, v1) = prems(0) >> Eval(__, __);
    let$ (e2, v2) = prems(1) >> Eval(__, __);
    let$ _ = concl >> Eval(BinOp(OpTimes, !e1, !e2), !!Times(v1, v2));
    Ok();
  | S_Lt =>
    let$ (ctx, (e1, _)) = prems(0) >> Entail(__, Ana(__, Num));
    let$ (_, (e2, _)) = prems(1) >> Entail(!ctx, Ana(__, Num));
    let$ _ = concl >> Entail(!ctx, Syn(BinOp(OpLt, !e1, !e2), Bool));
    Ok();
  | T_Lt =>
    let$ (ctx, (e1, _)) = prems(0) >> Entail(__, HasTy(__, Num));
    let$ (_, (e2, _)) = prems(1) >> Entail(!ctx, HasTy(__, Num));
    let$ _ = concl >> Entail(!ctx, HasTy(BinOp(OpLt, !e1, !e2), Bool));
    Ok();
  | E_Lt_T =>
    let$ (e1, v1) = prems(0) >> Eval(__, __);
    let$ (e2, _) = prems(1) >> Eval(__, !!Lt(v1));
    let$ _ = concl >> Eval(BinOp(OpLt, !e1, !e2), True);
    Ok();
  | E_Lt_F =>
    let$ (e1, v1) = prems(0) >> Eval(__, __);
    let$ (e2, _) = prems(1) >> Eval(__, !!NotLt(v1));
    let$ _ = concl >> Eval(BinOp(OpLt, !e1, !e2), False);
    Ok();
  | S_Gt =>
    let$ (ctx, (e1, _)) = prems(0) >> Entail(__, Ana(__, Num));
    let$ (_, (e2, _)) = prems(1) >> Entail(!ctx, Ana(__, Num));
    let$ _ = concl >> Entail(!ctx, Syn(BinOp(OpGt, !e1, !e2), Bool));
    Ok();
  | T_Gt =>
    let$ (ctx, (e1, _)) = prems(0) >> Entail(__, HasTy(__, Num));
    let$ (_, (e2, _)) = prems(1) >> Entail(!ctx, HasTy(__, Num));
    let$ _ = concl >> Entail(!ctx, HasTy(BinOp(OpGt, !e1, !e2), Bool));
    Ok();
  | E_Gt_T =>
    let$ (e1, v1) = prems(0) >> Eval(__, __);
    let$ (e2, _) = prems(1) >> Eval(__, !!Gt(v1));
    let$ _ = concl >> Eval(BinOp(OpGt, !e1, !e2), True);
    Ok();
  | E_Gt_F =>
    let$ (e1, v1) = prems(0) >> Eval(__, __);
    let$ (e2, _) = prems(1) >> Eval(__, !!NotGt(v1));
    let$ _ = concl >> Eval(BinOp(OpGt, !e1, !e2), False);
    Ok();
  | S_Eq =>
    let$ (ctx, (e1, _)) = prems(0) >> Entail(__, Ana(__, Num));
    let$ (_, (e2, _)) = prems(1) >> Entail(!ctx, Ana(__, Num));
    let$ _ = concl >> Entail(!ctx, Syn(BinOp(OpEq, !e1, !e2), Bool));
    Ok();
  | T_Eq =>
    let$ (ctx, (e1, _)) = prems(0) >> Entail(__, HasTy(__, Num));
    let$ (_, (e2, _)) = prems(1) >> Entail(!ctx, HasTy(__, Num));
    let$ _ = concl >> Entail(!ctx, HasTy(BinOp(OpEq, !e1, !e2), Bool));
    Ok();
  | E_Eq_T =>
    let$ (e1, v1) = prems(0) >> Eval(__, __);
    let$ (e2, _) = prems(1) >> Eval(__, !!Eq(v1));
    let$ _ = concl >> Eval(BinOp(OpEq, !e1, !e2), True);
    Ok();
  | E_Eq_F =>
    let$ (e1, v1) = prems(0) >> Eval(__, __);
    let$ (e2, _) = prems(1) >> Eval(__, !!NotEq(v1));
    let$ _ = concl >> Eval(BinOp(OpEq, !e1, !e2), False);
    Ok();
  | S_If =>
    let$ (ctx, (e_cond, _)) = prems(0) >> Entail(__, Ana(__, Bool));
    let$ (_, (e_then, t)) = prems(1) >> Entail(!ctx, Syn(__, __));
    let$ (_, (e_else, _)) = prems(2) >> Entail(!ctx, Syn(__, !t));
    let$ _ = concl >> Entail(!ctx, Syn(If(!e_cond, !e_then, !e_else), !t));
    Ok();
  | A_If =>
    let$ (ctx, (e_cond, _)) = prems(0) >> Entail(__, Ana(__, Bool));
    let$ (_, (e_then, t)) = prems(1) >> Entail(!ctx, Ana(__, __));
    let$ (_, (e_else, _)) = prems(2) >> Entail(!ctx, Ana(__, !t));
    let$ _ = concl >> Entail(!ctx, Ana(If(!e_cond, !e_then, !e_else), !t));
    Ok();
  | T_If =>
    let$ (ctx, (e_cond, _)) = prems(0) >> Entail(__, HasTy(__, Bool));
    let$ (_, (e_then, t)) = prems(1) >> Entail(!ctx, HasTy(__, __));
    let$ (_, (e_else, _)) = prems(2) >> Entail(!ctx, HasTy(__, !t));
    let$ _ =
      concl >> Entail(!ctx, HasTy(If(!e_cond, !e_then, !e_else), !t));
    Ok();
  | E_If_T =>
    let$ (e_cond, _) = prems(0) >> Eval(__, True);
    let$ (e_then, v) = prems(1) >> Eval(__, __);
    let$ _ = concl >> Eval(If(!e_cond, !e_then, __), !v);
    Ok();
  | E_If_F =>
    let$ (e_cond, _) = prems(0) >> Eval(__, False);
    let$ (e_else, v) = prems(1) >> Eval(__, __);
    let$ _ = concl >> Eval(If(!e_cond, __, !e_else), !v);
    Ok();
  | S_Var =>
    let$ (ctx, (x, t)) = concl >> Entail(__, Syn(__, __));
    let$ _ = ctx >> !!MemHasTy(x, t);
    Ok();
  | T_Var =>
    let$ (ctx, p) = concl >> Entail(__, __);
    // TODO: The same as rule Assumption, we make it different by
    // checking p is a HasTy proposition
    let$ _ = p >> HasTy(__, __);
    let$ _ = ctx >> !!Mem(p);
    Ok();
  | S_LetAnn =>
    let$ (ctx, ((x, t_def, e_def, e_body), t)) =
      concl >> Entail(__, Syn(LetAnn(__, __, __, __), __));
    let ctx' = ConsHasTy((x, t_def), ctx);
    let$ _ = prems(0) >> Entail(!ctx, Syn(!e_def, !t_def));
    let$ _ = prems(1) >> Entail(!!ctx', Syn(!e_body, !t));
    Ok();
  | A_LetAnn =>
    let$ (ctx, ((x, t_def, e_def, e_body), t)) =
      concl >> Entail(__, Ana(LetAnn(__, __, __, __), __));
    let ctx' = ConsHasTy((x, t_def), ctx);
    let$ _ = prems(0) >> Entail(!ctx, Syn(!e_def, !t_def));
    let$ _ = prems(1) >> Entail(!!ctx', Ana(!e_body, !t));
    Ok();
  | T_LetAnn =>
    let$ (ctx, ((x, t_def, e_def, e_body), t)) =
      concl >> Entail(__, HasTy(LetAnn(__, __, __, __), __));
    let ctx' = ConsHasTy((x, t_def), ctx);
    let$ _ = prems(0) >> Entail(!ctx, HasTy(!e_def, !t_def));
    let$ _ = prems(1) >> Entail(!!ctx', HasTy(!e_body, !t));
    Ok();
  | S_Let =>
    let$ (ctx, ((x, e_def, e_body), t)) =
      concl >> Entail(__, Syn(Let(__, __, __), __));
    let$ (_, (_, t_def)) = prems(0) >> Entail(!ctx, Ana(!e_def, __));
    let ctx' = ConsHasTy((x, t_def), ctx);
    let$ _ = prems(1) >> Entail(!!ctx', Syn(!e_body, !t));
    Ok();
  | A_Let =>
    let$ (ctx, ((x, e_def, e_body), t)) =
      concl >> Entail(__, Ana(Let(__, __, __), __));
    let$ (_, (_, t_def)) = prems(0) >> Entail(!ctx, Ana(!e_def, __));
    let ctx' = ConsHasTy((x, t_def), ctx);
    let$ _ = prems(1) >> Entail(!!ctx', Ana(!e_body, !t));
    Ok();
  | T_Let =>
    let$ (ctx, ((x, e_def, e_body), t)) =
      concl >> Entail(__, HasTy(Let(__, __, __), __));
    let$ (_, (_, t_def)) = prems(0) >> Entail(!ctx, HasTy(!e_def, __));
    let ctx' = ConsHasTy((x, t_def), ctx);
    let$ _ = prems(1) >> Entail(!!ctx', HasTy(!e_body, !t));
    Ok();
  | E_Let =>
    let$ ((x, e_def, e_body), v) = concl >> Eval(Let(__, __, __), __);
    let$ (_, v_def) = prems(0) >> Eval(!e_def, __);
    let$ _ = prems(1) >> Eval(!!Subst((v_def, x), e_body), !v);
    Ok();
  | S_FunAnn =>
    let$ (ctx, ((x, t_in, e_body), (t_in', t_out))) =
      concl >> Entail(__, Syn(FunAnn(__, __, __), Arrow(__, __)));
    let$ _ = t_in' >> !t_in;
    let ctx' = ConsHasTy((x, t_in), ctx);
    let$ _ = prems(0) >> Entail(!!ctx', Syn(!e_body, !t_out));
    Ok();
  | A_FunAnn =>
    let$ (ctx, ((x, t_in, e_body), (t_in', t_out))) =
      concl >> Entail(__, Ana(FunAnn(__, __, __), Arrow(__, __)));
    let$ _ = t_in' >> !t_in;
    let ctx' = ConsHasTy((x, t_in), ctx);
    let$ _ = prems(0) >> Entail(!!ctx', Ana(!e_body, !t_out));
    Ok();
  | T_FunAnn =>
    let$ (ctx, ((x, t_in, e_body), (t_in', t_out))) =
      concl >> Entail(__, HasTy(FunAnn(__, __, __), Arrow(__, __)));
    let$ _ = t_in' >> !t_in;
    let ctx' = ConsHasTy((x, t_in), ctx);
    let$ _ = prems(0) >> Entail(!!ctx', HasTy(!e_body, !t_out));
    Ok();
  | A_Fun =>
    let$ (ctx, ((x, e_body), (t_in, t_out))) =
      concl >> Entail(__, Ana(Fun(__, __), Arrow(__, __)));
    let ctx' = ConsHasTy((x, t_in), ctx);
    let$ _ = prems(0) >> Entail(!!ctx', Ana(!e_body, !t_out));
    Ok();
  | T_Fun =>
    let$ (ctx, ((x, e_body), (t_in, t_out))) =
      concl >> Entail(__, HasTy(Fun(__, __), Arrow(__, __)));
    let ctx' = ConsHasTy((x, t_in), ctx);
    let$ _ = prems(0) >> Entail(!!ctx', HasTy(!e_body, !t_out));
    Ok();
  | V_Fun =>
    let$ _ = concl >> Val(Fun(__, __));
    Ok();
  | T_Fix =>
    let$ (ctx, ((x, e), t)) =
      concl >> Entail(__, HasTy(Fix(__, __), __));
    let$ _ = prems(0) >> Entail(!!ConsHasTy((x, t), ctx), HasTy(!e, !t));
    Ok();
  | T_FixAnn =>
    let$ (ctx, ((x, t, e), t')) =
      concl >> Entail(__, HasTy(FixAnn(__, __, __), __));
    let$ _ = t >> !t';
    let$ _ = prems(0) >> Entail(!!ConsHasTy((x, t), ctx), HasTy(!e, !t));
    Ok();
  | E_Fix =>
    let$ (e, v) = concl >> Eval(__, __);
    let$ (x, e_body) = e >> Fix(__, __);
    let$ _ = prems(0) >> Eval(!!Subst((e, x), e_body), !v);
    Ok();
  | S_Ap =>
    let$ (ctx, ((e_fun, e_arg), t_out)) =
      concl >> Entail(__, Syn(Ap(__, __), __));
    let$ (_, (_, (t_in, _))) =
      prems(0) >> Entail(!ctx, Syn(!e_fun, Arrow(__, !t_out)));
    let$ _ = prems(1) >> Entail(!ctx, Ana(!e_arg, !t_in));
    Ok();
  | T_Ap =>
    let$ (ctx, ((e_fun, e_arg), t_out)) =
      concl >> Entail(__, HasTy(Ap(__, __), __));
    let$ (_, (_, (t_in, _))) =
      prems(0) >> Entail(!ctx, HasTy(!e_fun, Arrow(__, !t_out)));
    let$ _ = prems(1) >> Entail(!ctx, HasTy(!e_arg, !t_in));
    Ok();
  | E_Ap =>
    let$ ((e_fun, e_arg), v) = concl >> Eval(Ap(__, __), __);
    let$ (_, (x, e_body)) = prems(0) >> Eval(!e_fun, Fun(__, __));
    let$ (_, v_arg) = prems(1) >> Eval(!e_arg, __);
    let$ _ = prems(2) >> Eval(!!Subst((v_arg, x), e_body), !v);
    Ok();
  | S_Pair =>
    let$ (ctx, (el, tl)) = prems(0) >> Entail(__, Syn(__, __));
    let$ (_, (er, tr)) = prems(1) >> Entail(!ctx, Syn(__, __));
    let$ _ = concl >> Entail(!ctx, Syn(Pair(!el, !er), Prod(!tl, !tr)));
    Ok();
  | A_Pair =>
    let$ (ctx, (el, tl)) = prems(0) >> Entail(__, Ana(__, __));
    let$ (_, (er, tr)) = prems(1) >> Entail(!ctx, Ana(__, __));
    let$ _ = concl >> Entail(!ctx, Ana(Pair(!el, !er), Prod(!tl, !tr)));
    Ok();
  | T_Pair =>
    let$ (ctx, (el, tl)) = prems(0) >> Entail(__, HasTy(__, __));
    let$ (_, (er, tr)) = prems(1) >> Entail(!ctx, HasTy(__, __));
    let$ _ = concl >> Entail(!ctx, HasTy(Pair(!el, !er), Prod(!tl, !tr)));
    Ok();
  | E_Pair =>
    let$ (el, vl) = prems(0) >> Eval(__, __);
    let$ (er, vr) = prems(1) >> Eval(__, __);
    let$ _ = concl >> Eval(Pair(!el, !er), Pair(!vl, !vr));
    Ok();
  | V_Pair =>
    let$ _ = concl >> Val(Pair(__, __));
    Ok();
  | S_LetPair =>
    let$ (ctx, ((x, y, e_def, e_body), t)) =
      concl >> Entail(__, Syn(LetPair(__, __, __, __), __));
    let$ (_, (_, (tl, tr))) =
      prems(0) >> Entail(!ctx, Syn(!e_def, Prod(__, __)));
    let ctx_xy = ConsHasTy2((x, tl), (y, tr), ctx);
    let$ _ = prems(1) >> Entail(!!ctx_xy, Syn(!e_body, !t));
    Ok();
  | A_LetPair =>
    let$ (ctx, ((x, y, e_def, e_body), t)) =
      concl >> Entail(__, Ana(LetPair(__, __, __, __), __));
    let$ (_, (_, (tl, tr))) =
      prems(0) >> Entail(!ctx, Syn(!e_def, Prod(__, __)));
    let ctx_xy = ConsHasTy2((x, tl), (y, tr), ctx);
    let$ _ = prems(1) >> Entail(!!ctx_xy, Ana(!e_body, !t));
    Ok();
  | T_LetPair =>
    let$ (ctx, ((x, y, e_def, e_body), t)) =
      concl >> Entail(__, HasTy(LetPair(__, __, __, __), __));
    let$ (_, (_, (tl, tr))) =
      prems(0) >> Entail(!ctx, HasTy(!e_def, Prod(__, __)));
    let ctx_xy = ConsHasTy2((x, tl), (y, tr), ctx);
    let$ _ = prems(1) >> Entail(!!ctx_xy, HasTy(!e_body, !t));
    Ok();
  | E_LetPair =>
    let$ ((x, y, e_def, e_body), v) =
      concl >> Eval(LetPair(__, __, __, __), __);
    let$ (_, (vl, vr)) = prems(0) >> Eval(!e_def, Pair(__, __));
    let e_body = Subst2((vl, x), (vr, y), e_body);
    let$ _ = prems(1) >> Eval(!!e_body, !v);
    Ok();
  | S_PrjL =>
    let$ (ctx, (e, tl)) = concl >> Entail(__, Syn(PrjL(__), __));
    let$ _ = prems(0) >> Entail(!ctx, Syn(!e, Prod(!tl, __)));
    Ok();
  | T_PrjL =>
    let$ (ctx, (e, tl)) = concl >> Entail(__, HasTy(PrjL(__), __));
    let$ _ = prems(0) >> Entail(!ctx, HasTy(!e, Prod(!tl, __)));
    Ok();
  | E_PrjL =>
    let$ (e, vl) = concl >> Eval(PrjL(__), __);
    let$ _ = prems(0) >> Eval(!e, Pair(!vl, __));
    Ok();
  | S_PrjR =>
    let$ (ctx, (e, tr)) = concl >> Entail(__, Syn(PrjR(__), __));
    let$ _ = prems(0) >> Entail(!ctx, Syn(!e, Prod(__, !tr)));
    Ok();
  | T_PrjR =>
    let$ (ctx, (e, tr)) = concl >> Entail(__, HasTy(PrjR(__), __));
    let$ _ = prems(0) >> Entail(!ctx, HasTy(!e, Prod(__, !tr)));
    Ok();
  | E_PrjR =>
    let$ (e, vr) = concl >> Eval(PrjR(__), __);
    let$ _ = prems(0) >> Eval(!e, Pair(__, !vr));
    Ok();
  | A_InjL =>
    let$ (ctx, (e, tl)) = prems(0) >> Entail(__, Ana(__, __));
    let$ _ = concl >> Entail(!ctx, Ana(InjL(!e), Sum(!tl, __)));
    Ok();
  | T_InjL =>
    let$ (ctx, (e, tl)) = prems(0) >> Entail(__, HasTy(__, __));
    let$ _ = concl >> Entail(!ctx, HasTy(InjL(!e), Sum(!tl, __)));
    Ok();
  | E_InjL =>
    let$ (e, v) = prems(0) >> Eval(__, __);
    let$ _ = concl >> Eval(InjL(!e), InjL(!v));
    Ok();
  | V_InjL =>
    let$ e = prems(0) >> Val(__);
    let$ _ = concl >> Val(InjL(!e));
    Ok();
  | A_InjR =>
    let$ (ctx, (e, tr)) = prems(0) >> Entail(__, Ana(__, __));
    let$ _ = concl >> Entail(!ctx, Ana(InjR(!e), Sum(__, !tr)));
    Ok();
  | T_InjR =>
    let$ (ctx, (e, tr)) = prems(0) >> Entail(__, HasTy(__, __));
    let$ _ = concl >> Entail(!ctx, HasTy(InjR(!e), Sum(__, !tr)));
    Ok();
  | E_InjR =>
    let$ (e, v) = prems(0) >> Eval(__, __);
    let$ _ = concl >> Eval(InjR(!e), InjR(!v));
    Ok();
  | V_InjR =>
    let$ e = prems(0) >> Val(__);
    let$ _ = concl >> Val(InjR(!e));
    Ok();
  | A_Case =>
    let$ (ctx, ((e_scrut, x, el, y, er), t)) =
      concl >> Entail(__, Ana(Case(__, __, __, __, __), __));
    let$ (_, (_, (tl, tr))) =
      prems(0) >> Entail(!ctx, Syn(!e_scrut, Sum(__, __)));
    let$ _ = prems(1) >> Entail(!!ConsHasTy((x, tl), ctx), Ana(!el, !t));
    let$ _ = prems(2) >> Entail(!!ConsHasTy((y, tr), ctx), Ana(!er, !t));
    Ok();
  | S_Case =>
    let$ (ctx, ((e_scrut, x, el, y, er), t)) =
      concl >> Entail(__, Syn(Case(__, __, __, __, __), __));
    let$ (_, (_, (tl, tr))) =
      prems(0) >> Entail(!ctx, Syn(!e_scrut, Sum(__, __)));
    let$ _ = prems(1) >> Entail(!!ConsHasTy((x, tl), ctx), Syn(!el, !t));
    let$ _ = prems(2) >> Entail(!!ConsHasTy((y, tr), ctx), Syn(!er, !t));
    Ok();
  | T_Case =>
    let$ (ctx, ((e_scrut, x, el, y, er), t)) =
      concl >> Entail(__, HasTy(Case(__, __, __, __, __), __));
    let$ (_, (_, (tl, tr))) =
      prems(0) >> Entail(!ctx, HasTy(!e_scrut, Sum(__, __)));
    let$ _ =
      prems(1) >> Entail(!!ConsHasTy((x, tl), ctx), HasTy(!el, !t));
    let$ _ =
      prems(2) >> Entail(!!ConsHasTy((y, tr), ctx), HasTy(!er, !t));
    Ok();
  | E_Case_L =>
    let$ ((e_scrut, x, el, _y, _er), v) =
      concl >> Eval(Case(__, __, __, __, __), __);
    let$ (_, v_data) = prems(0) >> Eval(!e_scrut, InjL(__));
    let$ _ = prems(1) >> Eval(!!Subst((v_data, x), el), !v);
    Ok();
  | E_Case_R =>
    let$ ((e_scrut, _x, _el, y, er), v) =
      concl >> Eval(Case(__, __, __, __, __), __);
    let$ (_, v_data) = prems(0) >> Eval(!e_scrut, InjR(__));
    let$ _ = prems(1) >> Eval(!!Subst((v_data, y), er), !v);
    Ok();
  | T_Roll =>
    let$ (ctx, (e_body, t)) = concl >> Entail(__, HasTy(Roll(__), __));
    let$ (a, t_body) = t >> Rec(__, __);
    let$ _ =
      prems(0) >> Entail(!ctx, HasTy(!e_body, !!SubstTy((t, a), t_body)));
    Ok();
  | E_Roll =>
    let$ (e, v) = concl >> Eval(Roll(__), Roll(__));
    let$ _ = prems(0) >> Eval(!e, !v);
    Ok();
  | V_Roll =>
    let$ e = prems(0) >> Val(__);
    let$ _ = concl >> Val(Roll(!e));
    Ok();
  | T_Unroll =>
    let$ (ctx, (e, t)) = prems(0) >> Entail(__, HasTy(__, __));
    let$ (a, t_body) = t >> Rec(__, __);
    let$ _ =
      concl >> Entail(!ctx, HasTy(Unroll(!e), !!SubstTy((t, a), t_body)));
    Ok();
  | E_Unroll =>
    let$ (e, v) = prems(0) >> Eval(__, Roll(__));
    let$ _ = concl >> Eval(Unroll(!e), !v);
    Ok();
  | Assumption =>
    let$ (ctx, p) = concl >> Entail(__, __);
    let$ _ = ctx >> !!Mem(p);
    Ok();
  | And_I =>
    let$ (ctx, (a, b)) = concl >> Entail(__, And(__, __));
    let$ _ = prems(0) >> Entail(!ctx, !a);
    let$ _ = prems(1) >> Entail(!ctx, !b);
    Ok();
  | And_E_L =>
    let$ (ctx, a) = concl >> Entail(__, __);
    let$ _ = prems(0) >> Entail(!ctx, And(!a, __));
    Ok();
  | And_E_R =>
    let$ (ctx, b) = concl >> Entail(__, __);
    let$ _ = prems(0) >> Entail(!ctx, And(__, !b));
    Ok();
  | Or_I_L =>
    let$ (ctx, (a, _b)) = concl >> Entail(__, Or(__, __));
    let$ _ = prems(0) >> Entail(!ctx, !a);
    Ok();
  | Or_I_R =>
    let$ (ctx, (_a, b)) = concl >> Entail(__, Or(__, __));
    let$ _ = prems(0) >> Entail(!ctx, !b);
    Ok();
  | Or_E =>
    let$ (ctx, c) = concl >> Entail(__, __);
    let$ (_, (a, b)) = prems(0) >> Entail(!ctx, Or(__, __));
    let$ _ = prems(1) >> Entail(!!Cons(a, ctx), !c);
    let$ _ = prems(2) >> Entail(!!Cons(b, ctx), !c);
    Ok();
  | Implies_I =>
    let$ (ctx, (a, b)) = concl >> Entail(__, Impl(__, __));
    let$ _ = prems(0) >> Entail(!!Cons(a, ctx), !b);
    Ok();
  | Implies_E =>
    let$ (ctx, (a, b)) = prems(0) >> Entail(__, Impl(__, __));
    let$ _ = prems(1) >> Entail(!ctx, !a);
    let$ _ = concl >> Entail(!ctx, !b);
    Ok();
  | Truth_I =>
    let$ _ = concl >> Entail(__, Truth);
    Ok();
  | Falsity_E =>
    let$ (ctx, _) = concl >> Entail(__, __);
    let$ _ = prems(0) >> Entail(!ctx, Falsity);
    Ok();
  };
};

let bind_ghost:
  (Prop.deduction(Prop.t), Prop.deduction(Prop.t)) => Prop.deduction(t) =
  (self, ghost) => {
    prems:
      List.map2(
        (p1, p2) => {self: p1, ghost: Some(p2)},
        self.prems,
        ghost.prems,
      ),
    concl: {
      self: self.concl,
      ghost: Some(ghost.concl),
    },
  };

let bind_none: Prop.deduction(Prop.t) => Prop.deduction(t) =
  self => {
    prems: List.map(p => {self: p, ghost: None}, self.prems),
    concl: {
      self: self.concl,
      ghost: None,
    },
  };

let verify =
    (rule: Rule.t, d: Prop.deduction(Prop.t), ~with_ghost: bool)
    : result(unit, failure) => {
  let Prop.{prems, concl} =
    if (with_ghost) {
      bind_ghost(d, Ghost.of_ghost(rule));
    } else {
      bind_none(d);
    };
  verify(rule, prems, concl);
};

let verify_ghost = (rule: Rule.t) =>
  verify(rule, Ghost.of_ghost(rule), ~with_ghost=false);

let verify_all_ghosts = (): unit => {
  let rules = Rule.all;
  List.iter(
    rule =>
      rule
      |> verify_ghost
      |> (
        fun
        | Ok(_) => ()
        | Error(f) => {
            print_endline("Failed to verify rule: " ++ Rule.show(rule));
            print_endline("Failure: " ++ show_failure(f));
          }
      ),
    rules,
  );
};
