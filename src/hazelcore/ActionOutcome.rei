[@deriving sexp]
type t('success) =
  | Succeeded('success)
  | CursorEscaped(Side.t)
  | Failed;

/** Rescue a CursorEscaped(Side.t) into a new outcome */
let rescue_escaped: (Side.t => t('a), t('a)) => t('a);

let of_option: option('a) => t('a);

include Monads.MONAD with type t('a) := t('a);
