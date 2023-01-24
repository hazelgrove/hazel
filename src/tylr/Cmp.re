type r('in_, 'lt, 'eq, 'gt) =
  | In('in_) // incomparable
  | Lt('lt)
  | Eq('eq)
  | Gt('gt);
type s('a) = r('a, 'a, 'a, 'a);
type t = s(unit);

type leg('a) = r(unit, 'a, 'a, 'a);

let of_int = (c: int) =>
  if (c < 0) {
    Lt();
  } else if (c > 0) {
    Gt();
  } else {
    Eq();
  };

let t_of_r: r(_) => t =
  fun
  | In(_) => In()
  | Lt(_) => Lt()
  | Eq(_) => Eq()
  | Gt(_) => Gt();