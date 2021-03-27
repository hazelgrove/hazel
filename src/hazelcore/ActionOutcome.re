[@deriving sexp]
type t('success) =
  | Succeeded('success)
  | CursorEscaped(Side.t)
  | Failed;

let of_option =
  fun
  | Some(x) => Succeeded(x)
  | None => Failed;
