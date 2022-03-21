open Sexplib.Std;

[@deriving sexp]
type var =
  | Named(Var.t)
  | Tmp(int);

[@deriving sexp]
type params = list(var);

module BinOp = {
  [@deriving sexp]
  type op =
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
};

[@deriving sexp]
type bin_op =
  | Prim(BinOp.op)
  | Hazel(BinOp.op);

[@deriving sexp]
type pat =
  | Wild
  | Var(Var.t)
  | IntLit(int)
  | FloatLit(float)
  | BoolLit(bool)
  | ListNil
  | Cons(pat, pat)
  | Pair(pat, pat)
  | Triv;

[@deriving sexp]
type expr =
  | BoolLit(bool)
  | IntLit(int)
  | FloatLit(float)
  | BinOp(bin_op, expr, expr)
  | List(list(expr))
  | Triv
  | Var(var)
  | Builtin(Var.t)
  | Inj(side, expr)
  | Lam(params, expr)
  | Ap(expr, args)
  | Match(expr, list(rule))
and args = list(expr)
and side =
  | L
  | R
and rule =
  | Rule(pat, expr);

[@deriving sexp]
type statement =
  | Let(params, expr)
  | LetRec(params, expr)
  | Expr(expr);

[@deriving sexp]
type block = list(statement);

module Block = {
  let append = (b1, b2) => List.append(b1, b2);
};

[@deriving sexp]
type t = block;
