open Sexplib.Std;

[@deriving sexp]
type top_block = list(top_stmt)

[@deriving sexp]
and top_stmt =
  | TSImport(Var.t, import_path)
  | TSDecl(decl)

[@deriving sexp]
and import_path =
  | ImportStd(string)
  | ImportRel(string)

[@deriving sexp]
and decl =
  | DEnum(enum)

[@deriving sexp]
and enum = {
  name: Var.t,
  type_vars: list(Var.t),
  variants: list(enum_variant),
}

[@deriving sexp]
and enum_variant = {
  ctor: Var.t,
  params: list(Var.t),
};

module TopBlock = {
  let join = tbs => List.concat(tbs);
};

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
  | PVar(Var.t)
  | PInt(int)
  | PFloat(float)
  | PBool(bool)
  | PNil
  | PCons(pat, pat)
  | PTuple(list(pat))
  | PTriv
  | PCtor(Var.t, list(pat))

[@deriving sexp]
and block = list(stmt)

[@deriving sexp]
and stmt =
  | SLet(params, expr)
  | SLetRec(params, expr)
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
  | EVar(Var.t)
  | ELam(params, expr)
  | EAp(expr, args)
  | ECtor(Var.t, args)
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

[@deriving sexp]
type prog = (top_block, block);
