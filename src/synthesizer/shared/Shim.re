open Types;
open DHExp;

let main: ZExp.t => exp = ze => {
    exp_of_dhexp(ZExp.erase(ze))
}

let rec exp_of_dhexp: t => exp =
  exp =>
    switch (exp) {
    | EmptyHole(_, _, _) => Other(Nil)
    | NonEmptyHole(_, _, _, _, t) => Other(Cons(exp_of_dhexp(t), Nil))
    | Keyword(_, _, _, _) => Other(Nil)
    | FreeVar(_, _, _, _) => Other(Nil)
    | InvalidText(_, _, _, _) => Other(Nil)
    | BoundVar(_) => Other(Nil)
    | Let(_, t1, t2) => Other(Cons(exp_of_dhexp(t1), Cons(exp_of_dhexp(t2), Nil)))
    | FixF(_, _, t) => Other(Cons(exp_of_dhexp(t), Nil))
    | Lam(_, _, t) => Other(Cons(exp_of_dhexp(t), Nil))
    | Ap(t1, t2) => Other(Cons(exp_of_dhexp(t1), Cons(exp_of_dhexp(t2), Nil)))
    | BoolLit(_) => Other(Nil)
    | IntLit(_) => Other(Nil)
    | AssertLit => Assert
    | FailedAssert(t) => Other(Cons(exp_of_dhexp(t), Nil))
    | FloatLit(_) => Other(Nil)
    | BinBoolOp(_, t1, t2) => Other(Cons(exp_of_dhexp(t1), Cons(exp_of_dhexp(t2), Nil)))
    | BinIntOp(_, t1, t2) => Other(Cons(exp_of_dhexp(t1), Cons(exp_of_dhexp(t2), Nil)))
    | BinFloatOp(_, t1, t2) => Other(Cons(exp_of_dhexp(t1), Cons(exp_of_dhexp(t2), Nil)))
    | ListNil(_) => Other(Nil)
    | Cons(t1, t2) => Other(Cons(exp_of_dhexp(t1), Cons(exp_of_dhexp(t2), Nil)))
    | Inj(_, _, t) => Other(Cons(exp_of_dhexp(t), Nil))
    | Pair(t1, t2) => Other(Cons(exp_of_dhexp(t1), Cons(exp_of_dhexp(t2), Nil)))
    | Triv => Other(Nil)
    | ConsistentCase(_case) => Case
    | InconsistentBranches(_, _, _, _case) => Case
    | Cast(t, _, _) => Other(Cons(exp_of_dhexp(t), Nil))
    | FailedCast(t, _, _) => Other(Cons(exp_of_dhexp(t), Nil))
    | InvalidOperation(t, _) => Other(Cons(exp_of_dhexp(t), Nil))
    };
