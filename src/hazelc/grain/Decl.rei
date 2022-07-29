[@deriving sexp]
type export =
  | ExPublic
  | ExPrivate;

[@deriving sexp]
type t =
  | DEnum(export, Enum.t)
  | DStmt(export, Expr.stmt)
  | DImport(Import.t);
