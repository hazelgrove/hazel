open Sexplib.Std;

open Expr;

[@deriving sexp]
type top_block = list(top_stmt)

[@deriving sexp]
and top_stmt =
  | TImport(import)
  | TDecl(decl)

[@deriving sexp]
and import = (ident, import_path)

[@deriving sexp]
and import_path =
  | ImportStd(string)
  | ImportRel(string)

[@deriving sexp]
and decl =
  | DEnum(enum)
  | DStmt(stmt)

[@deriving sexp]
and enum = {
  name: ident,
  type_vars: list(ident),
  variants: list(enum_variant),
}

[@deriving sexp]
and enum_variant = {
  ctor: ident,
  params: list(ident),
};

module TopBlock = {
  let join = tbs => List.concat(tbs);
};

[@deriving sexp]
type prog = (top_block, block);

let empty = ([], []);
