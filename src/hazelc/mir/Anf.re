open Sexplib.Std;

[@deriving sexp]
type bin_op =
  | And
  | Or
  | Plus
  | Minus
  | Times
  | Divide
  | LessThan
  | GreaterThan
  | Equals
  | FPlus
  | FMinus
  | FTimes
  | FDivide
  | FLessThan
  | FGreaterThan
  | FEquals;

[@deriving sexp]
type imm =
  | IntLit(int)
  | FloatLit(float)
  | BoolLit(bool)
  | ListNil
  | Triv
  | List(list(imm))
  | Var(Var.t)
and comp =
  | Imm(imm)
  | BinOp(bin_op, imm, imm)
  | Ap(Var.t, list(imm))
  | Lam(list(Var.t), expr)
  | Cons(imm, imm)
  | Inj(HTyp.t, InjSide.t, imm)
  | Pair(imm, imm)
and expr =
  | Let(IHPat.t, comp, expr)
  | LetRec(IHPat.t, comp, expr)
  | Seq(comp, expr)
  | Comp(comp)
and prog = {body: expr};
