open Sexplib.Std;

exception NotImplemented;

module BinBoolOp = {
  [@deriving sexp]
  type t =
    | And
    | Or;

  let of_op = (op: DHExp.BinBoolOp.t): t =>
    switch (op) {
    | And => And
    | Or => Or
    };

  let to_op = (op: t): DHExp.BinBoolOp.t =>
    switch (op) {
    | And => And
    | Or => Or
    };
};

module BinIntOp = {
  [@deriving sexp]
  type t =
    | Minus
    | Plus
    | Times
    | Divide
    | LessThan
    | GreaterThan
    | Equals;

  let of_op = (op: DHExp.BinIntOp.t): t =>
    switch (op) {
    | Minus => Minus
    | Plus => Plus
    | Times => Times
    | Divide => Divide
    | LessThan => LessThan
    | GreaterThan => GreaterThan
    | Equals => Equals
    };

  let to_op = (bio: t): DHExp.BinIntOp.t =>
    switch (bio) {
    | Minus => Minus
    | Plus => Plus
    | Times => Times
    | Divide => Divide
    | LessThan => LessThan
    | GreaterThan => GreaterThan
    | Equals => Equals
    };
};

module BinFloatOp = {
  [@deriving sexp]
  type t =
    | FPlus
    | FMinus
    | FTimes
    | FDivide
    | FLessThan
    | FGreaterThan
    | FEquals;

  let of_op = (op: DHExp.BinFloatOp.t): t =>
    switch (op) {
    | FPlus => FPlus
    | FMinus => FMinus
    | FTimes => FTimes
    | FDivide => FDivide
    | FLessThan => FLessThan
    | FGreaterThan => FGreaterThan
    | FEquals => FEquals
    };

  let to_op = (bfo: t): DHExp.BinFloatOp.t =>
    switch (bfo) {
    | FPlus => FPlus
    | FMinus => FMinus
    | FTimes => FTimes
    | FDivide => FDivide
    | FLessThan => FLessThan
    | FGreaterThan => FGreaterThan
    | FEquals => FEquals
    };
};

[@deriving sexp]
type t =
  | EmptyHole(MetaVar.t, MetaVarInst.t, VarMap.t_(t))
  | NonEmptyHole(
      ErrStatus.HoleReason.t,
      MetaVar.t,
      MetaVarInst.t,
      VarMap.t_(t),
      t,
    )
  // TODO rename to ExpandingKeyword
  | Keyword(MetaVar.t, MetaVarInst.t, VarMap.t_(t), ExpandingKeyword.t)
  | FreeVar(MetaVar.t, MetaVarInst.t, VarMap.t_(t), Var.t)
  | InvalidText(MetaVar.t, MetaVarInst.t, VarMap.t_(t), string)
  | BoundVar(Var.t)
  | Let(DHPat.t, t, t)
  | FixF(Var.t, HTyp.t, t)
  | Lam(DHPat.t, HTyp.t, t)
  | Ap(t, t)
  | BoolLit(bool)
  | IntLit(int)
  | FloatLit(float)
  | BinBoolOp(BinBoolOp.t, t, t)
  | BinIntOp(BinIntOp.t, t, t)
  | BinFloatOp(BinFloatOp.t, t, t)
  | ListNil(HTyp.t)
  | Cons(t, t)
  | Inj(HTyp.t, InjSide.t, t)
  | Pair(t, t)
  | Triv
  | ConsistentCase(case)
  | InconsistentBranches(MetaVar.t, MetaVarInst.t, VarMap.t_(t), case)
  | Cast(t, HTyp.t, HTyp.t)
  | FailedCast(t, HTyp.t, HTyp.t)
  | InvalidOperation(t, InvalidOperationError.t)
and case =
  | Case(t, list(rule), int)
and rule =
  | Rule(DHPat.t, t);

let rec of_DHExp = (d: DHExp.t): t =>
  switch (d) {
  | EmptyHole(var, varinst, varmap) =>
    EmptyHole(var, varinst, of_DHExp_varmap(varmap))
  | NonEmptyHole(reason, var, varinst, varmap, d0) =>
    NonEmptyHole(
      reason,
      var,
      varinst,
      of_DHExp_varmap(varmap),
      of_DHExp(d0),
    )
  | Keyword(var, varinst, varmap, keyword) =>
    Keyword(var, varinst, of_DHExp_varmap(varmap), keyword)
  | FreeVar(var, varinst, varmap, v) =>
    FreeVar(var, varinst, of_DHExp_varmap(varmap), v)
  | InvalidText(var, varinst, varmap, text) =>
    InvalidText(var, varinst, of_DHExp_varmap(varmap), text)
  | BoundVar(v) => BoundVar(v)
  | Let(pat, d1, d2) => Let(pat, of_DHExp(d1), of_DHExp(d2))
  | FixF(v, ty, d0) => FixF(v, ty, of_DHExp(d0))
  | Lam(pat, ty, d0) => Lam(pat, ty, of_DHExp(d0))
  | Ap(d1, d2) => Ap(of_DHExp(d1), of_DHExp(d2))
  | BoolLit(b) => BoolLit(b)
  | IntLit(i) => IntLit(i)
  | FloatLit(f) => FloatLit(f)
  | BinBoolOp(op, d1, d2) =>
    BinBoolOp(BinBoolOp.of_op(op), of_DHExp(d1), of_DHExp(d2))
  | BinIntOp(op, d1, d2) =>
    BinIntOp(BinIntOp.of_op(op), of_DHExp(d1), of_DHExp(d2))
  | BinFloatOp(op, d1, d2) =>
    BinFloatOp(BinFloatOp.of_op(op), of_DHExp(d1), of_DHExp(d2))
  | ListNil(ty) => ListNil(ty)
  | Cons(d1, d2) => Cons(of_DHExp(d1), of_DHExp(d2))
  | Inj(ty, side, d0) => Inj(ty, side, of_DHExp(d0))
  | Pair(d1, d2) => Pair(of_DHExp(d1), of_DHExp(d2))
  | Triv => Triv
  | InvalidOperation(d0, operr) => InvalidOperation(of_DHExp(d0), operr)
  | _ => raise(NotImplemented)
  //   | ConsistentCase(c)
  //   | InconsistentBranches(var, varinst, varmap, c)
  //   | Cast(d0, ty1, ty2)
  //   | FailedCast(d0, ty1, ty2)
  }
and of_DHExp_varmap = (varmap: VarMap.t_(DHExp.t)): VarMap.t_(t) => {
  VarMap.map(((_, d0)) => of_DHExp(d0), varmap);
};
