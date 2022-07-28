[@deriving sexp]
type t =
  | DEnum(Enum.t)
  | DStmt(Expr.stmt)
  | DImport(Import.t);
