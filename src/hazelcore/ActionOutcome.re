open Sexplib.Std;

[@deriving sexp]
type t_('success) =
  | Succeeded('success)
  | CursorEscaped(Side.t);

[@deriving sexp]
type t('success) = option(t_('success));

let succeeded = x => Some(Succeeded(x));

let cursor_escaped = s => Some(CursorEscaped(s));

include OptUtil;
