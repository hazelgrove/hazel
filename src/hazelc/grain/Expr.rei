[@deriving sexp]
type bin_op =
  | OpAnd
  | OpOr
  | OpEquals
  | OpNotEquals;

[@deriving sexp]
type params = list(Pat.t);

[@deriving sexp]
type t =
  | EBoolLit(bool)
  | EInt32Lit(int)
  | EInt64Lit(int)
  | EFloat32Lit(float)
  | EFloat64Lit(float)
  | ECharLit(char)
  | EStringLit(string)
  | EBinOp(bin_op, t, t)
  | EList(list(t))
  | ETriv
  | ECons(t, t)
  | ETuple(list(t))
  | EVar(Ident.t)
  | ELam(params, t)
  | EAp(t, args)
  | ECtor(Ident.t, args)
  | EMatch(t, list(rule))
  | EBlock(block)

[@deriving sexp]
and args = list(t)

[@deriving sexp]
and rule =
  | RRule(Pat.t, t)

[@deriving sexp]
and block = list(stmt)

[@deriving sexp]
and stmt =
  | SLet(Pat.t, t)
  | SLetRec(Pat.t, t)
  | SExpr(t);

let var: Ident.t => t;
let ap: (t, args) => t;
let ctor: (Ident.t, args) => t;
