open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type pos =
  | Concl
  | Prems(int);

[@deriving (show({with_path: false}), sexp, yojson)]
type bind('a) = {
  pos,
  value: 'a,
};

let copy_pos = ({pos, _}, value) => {pos, value};

let copy_pos2 = (p, (a, b)) => (copy_pos(p, a), copy_pos(p, b));

let copy_pos3 = (p, (a, b, c)) => (
  copy_pos(p, a),
  copy_pos(p, b),
  copy_pos(p, c),
);
