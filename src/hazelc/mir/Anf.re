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
and expr =
  | Let(Var.t, comp, expr)
  | LetRec(Var.t, comp, expr)
  | Seq(comp, expr)
  | Comp(comp)
and prog = {body: expr};
