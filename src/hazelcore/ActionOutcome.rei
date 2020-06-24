type t('success) =
  | Succeeded('success)
  | CursorEscaped(Side.t)
  | Failed;

let map: ('success1 => 'success2, t('success1)) => t('success2);
