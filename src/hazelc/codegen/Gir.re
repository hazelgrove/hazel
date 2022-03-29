open Sexplib.Std;

[@deriving sexp]
type top_block = list(top_statement)
and top_statement =
  | Import(Var.t, string)
  | Decl(decl)
and decl =
  | Enum(Var.t, list(Var.t), list((Var.t, list(Var.t))));

module TopBlock = {
  let join = tbs => List.concat(tbs);
};

[@deriving sexp]
type var =
  | Named(list(Var.t))
  | Tmp(int)
and params = list(var);

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
type block = list(statement)
and statement =
  | Let(params, expr)
  | LetRec(params, expr)
  | Expr(expr)
and expr =
  | BoolLit(bool)
  | IntLit(int)
  | FloatLit(float)
  | BinOp(bin_op, expr, expr)
  | List(list(expr))
  | Triv
  | Var(var)
  | Lam(params, expr)
  | Ap(expr, args)
  | Match(expr, list(rule))
  | Block(block)
and args = list(expr)
and rule =
  | Rule(pat, expr);

module Block = {
  let join = bs => List.concat(bs);
};

[@deriving sexp]
type program = (top_block, block);
