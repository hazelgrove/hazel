open Expr;

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
type prog = (top_block, Expr.block);
