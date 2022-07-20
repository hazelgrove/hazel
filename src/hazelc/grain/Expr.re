open Sexplib.Std;

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

/* FIXME: Move this type to Pat module. */
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
  | PCtor(ident, list(pat))

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

module Block = {
  let join = bs => List.concat(bs);
};

let mk_var = x => EVar(x);
let mk_ap = (fn, args) => EAp(fn, args);
let mk_ctor = (name, args) => ECtor(name, args);

let mk_pat_var = x => PVar(x);
let mk_pat_ctor = (name, args) => PCtor(name, args);
