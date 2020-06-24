type t('success) =
  | Succeeded('success)
  | CursorEscaped(Side.t)
  | Failed;

let map = (f: 'success1 => 'success2) =>
  fun
  | (Failed | CursorEscaped(_)) as err => err
  | Succeeded(s) => Succeeded(f(s));
