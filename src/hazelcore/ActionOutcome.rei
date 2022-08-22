[@deriving sexp]
type t('success) =
  | Succeeded('success)
  | CursorEscaped(Side.t)
  | Failed;

let of_option: option('success) => t('success);
