[@deriving sexp]
type var = string;

[@deriving sexp]
type top_block = list(top_stmt)

[@deriving sexp]
and top_stmt =
  | TImport(var, import_path)
  | TDecl(decl)

[@deriving sexp]
and import_path =
  | ImportStd(string)
  | ImportRel(string)

[@deriving sexp]
and decl =
  | DEnum(enum)

[@deriving sexp]
and enum = {
  name: var,
  type_vars: list(var),
  variants: list(enum_variant),
}

[@deriving sexp]
and enum_variant = {
  ctor: var,
  params: list(var),
};

module TopBlock: {let join: list(top_block) => top_block;};

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
  | PVar(var)
  | PInt(int)
  | PFloat(float)
  | PBool(bool)
  | PNil
  | PCons(pat, pat)
  | PTuple(list(pat))
  | PTriv
  | PCtor(var, list(pat))

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
  | EVar(var)
  | ELam(params, expr)
  | EAp(expr, args)
  | ECtor(var, args)
  | EMatch(expr, list(rule))
  | EBlock(block)

[@deriving sexp]
and args = list(expr)

[@deriving sexp]
and rule =
  | RRule(pat, expr);

module Block: {let join: list(block) => block;};

[@deriving sexp]
type prog = (top_block, block);
