type r('in_, 'lt, 'eq, 'gt) =
  | In('in_) // incomparable
  | Lt('lt)
  | Eq('eq)
  | Gt('gt);
type s('a) = r('a, 'a, 'a, 'a);
type t = s(unit);

// todo: rename to in_else
type i_leg('in_, 'else_) = r('in_, 'else_, 'else_, 'else_);

type lt_else('lt, 'else_) = r('else_, 'lt, 'else_, 'else_);
type gt_else('gt, 'else_) = r('else_, 'else_, 'else_, 'gt);

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
