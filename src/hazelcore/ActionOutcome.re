type t('success) =
  | Succeeded('success)
  | CursorEscaped(Side.t)
  | Failed;
