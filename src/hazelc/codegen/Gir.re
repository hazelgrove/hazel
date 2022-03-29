open Sexplib.Std;

[@deriving sexp]
type params = list(Var.t);

[@deriving sexp]
type top_block = list(top_statement)

[@deriving sexp]
and top_statement =
  | Import(Var.t, string)
  | Decl(decl)

[@deriving sexp]
and decl =
  | Enum(enum)

[@deriving sexp]
and enum = {
  name: Var.t,
  type_vars: list(Var.t),
  variants: list(enum_variant),
}

[@deriving sexp]
and enum_variant = {
  ctor: Var.t,
  params,
};

module TopBlock = {
  let join = tbs => List.concat(tbs);
};

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

[@deriving sexp]
and statement =
  | Let(params, expr)
  | LetRec(params, expr)
  | Expr(expr)

[@deriving sexp]
and expr =
  | BoolLit(bool)
  | IntLit(int)
  | FloatLit(float)
  | BinOp(bin_op, expr, expr)
  | List(list(expr))
  | Triv
  | Var(Var.t)
  | Lam(params, expr)
  | Ap(expr, args)
  | Ctor(Var.t, args)
  | Match(expr, list(rule))
  | Block(block)

[@deriving sexp]
and args = list(expr)

[@deriving sexp]
and rule =
  | Rule(pat, expr);

module Block = {
  let join = bs => List.concat(bs);
};

[@deriving sexp]
type program = (top_block, block);
