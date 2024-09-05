// ALFA logic
type typ =
  | Num
  | Bool
  | Arrow(typ, typ)
  | Prod(typ, typ)
  | Unit
  | Sum(typ, typ)
  | TVar(string)
  | Rec(tpat, typ)
and tpat =
  | TPat(string);
type expr =
  | NumLit(int)
  | UnOp(unop, expr)
  | BinOp(binop, expr, expr)
  | True
  | False
  | If(expr, expr, expr)
  | Var(string)
  | Let(expr, patexpr)
  | Fix(patexpr)
  | Fun(patexpr)
  | Ap(expr, expr)
  | Pair(expr, expr)
  | Triv
  | PrjL(expr)
  | PrjR(expr)
  | LetPair(expr, patexpr)
  | InjL(expr)
  | InjR(expr)
  | Case(expr, patexpr, patexpr)
  | Roll(expr)
  | Unroll(expr)
and pat =
  | Pat(string)
  | PatAnn(pat, typ)
and patexpr =
  | PatExpr(pat, patexpr)
and unop =
  | OpNeg
and binop =
  | OpLt
  | OpGt
  | OpEq
  | OpPlus
  | OpMinus
  | OpTimes;

type prop =
  // ALFp exclusive
  | HasTy(expr, typ)
  | Syn(expr, typ)
  | Ana(expr, typ)
  // Propositional logic
  | Atom(string)
  | And(prop, prop)
  | Or(prop, prop)
  | Implies(prop, prop)
  | Truth
  | Falsity;

// Judgements (the outermost layer of this syntax)
type judgement =
  | Val(expr)
  | Eval(expr, expr)
  | Entail(list(prop), prop);

// The following code is the implementation of the typeclass for Prop.t.
[@deriving (show({with_path: false}), sexp, yojson)]
type alias =
  | Hole
  | Judgement
  | Prop
  | Expr
  | Type
  | Pat
  | TPat
  | UnOp
  | BinOp;
