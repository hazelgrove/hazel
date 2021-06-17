open Sexplib.Std;

[@deriving sexp]
type t('op) =
  | Placeholder(int)
  | BinOp('op, int, t('op), t('op));

let rec mk =
        (skel: Skel.t('op), start_index: int, length: int): (t('op), int) => {
  switch (skel) {
  | Placeholder(n) => (Placeholder(n), start_index)
  | BinOp(_, op, skel1, skel2) =>
    let (left_annotated, left_index) = mk(skel1, start_index, length);
    let (right_annotated, right_index) = mk(skel2, left_index + 1, length);
    (
      BinOp(op, left_index + length, left_annotated, right_annotated),
      right_index,
    );
  };
};
let get_root_num = (skel: t(_)): int => {
  switch (skel) {
  | Placeholder(n)
  | BinOp(_, n, _, _) => n
  };
};
