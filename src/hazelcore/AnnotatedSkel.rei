[@deriving sexp]
type t('op) =
  | Placeholder(int)
  | BinOp('op, int, t('op), t('op));

let mk: (Skel.t('op), int, int) => (t('op), int);

let get_root_num: t(_) => int;
