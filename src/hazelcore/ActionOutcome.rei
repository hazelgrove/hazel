[@deriving sexp]
type t_('success) =
  | Succeeded('success)
  | CursorEscaped(Side.t);

[@deriving sexp]
type t('success) = option(t_('success));

let succeeded: 'success => t('success);

let cursor_escaped: Side.t => t('success);

include Monads.MONAD with type t('a) := option('a);
