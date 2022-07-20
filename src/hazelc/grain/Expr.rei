[@deriving sexp]
type ident = string;

[@deriving sexp]
type bin_op =
  | OpAnd
  | OpOr
  | OpEquals
  | OpNotEquals;

[@deriving sexp]
type params = list(pat)

[@deriving sexp]
and pat =
  | PWild
  | PVar(ident)
  | PInt(int)
  | PFloat(float)
  | PBool(bool)
  | PNil
  | PCons(pat, pat)
  | PTuple(list(pat))
  | PTriv
  | PCtor(ident, params)

[@deriving sexp]
and block = list(stmt)

[@deriving sexp]
and stmt =
  | SLet(pat, expr)
  | SLetRec(pat, expr)
  | SExpr(expr)

[@deriving sexp]
and expr =
  | EBoolLit(bool)
  | EInt32Lit(int)
  | EInt64Lit(int)
  | EFloat32Lit(float)
  | EFloat64Lit(float)
  | ECharLit(char)
  | EStringLit(string)
  | EBinOp(bin_op, expr, expr)
  | EList(list(expr))
  | ETriv
  | ECons(expr, expr)
  | ETuple(list(expr))
  | EVar(ident)
  | ELam(params, expr)
  | EAp(expr, args)
  | ECtor(ident, args)
  | EMatch(expr, list(rule))
  | EBlock(block)

[@deriving sexp]
and args = list(expr)

[@deriving sexp]
and rule =
  | RRule(pat, expr);

module Block: {let join: list(block) => block;};

let var: ident => expr;
let ap: (expr, args) => expr;
let ctor: (ident, args) => expr;

let pvar: ident => pat;
let pctor: (ident, params) => pat;
