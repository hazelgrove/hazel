[@deriving sexp]
type t('success) =
  | Succeeded('success)
  | CursorEscaped(Side.t)
  | Failed;

include Monads.MONAD with type t('a) := t('a);
