open Sexplib.Std;

[@deriving sexp]
type t('op) =
  | Placeholder(int)
  | BinOp(ErrStatus.t, 'op, t('op), t('op));

[@deriving sexp]
type range = (int, int);

let rec size = (skel: t(_)): int =>
  switch (skel) {
  | Placeholder(_) => 1
  | BinOp(_, _, skel1, skel2) => size(skel1) + size(skel2)
  };

let rec leftmost_tm_index = (skel: t(_)): int =>
  switch (skel) {
  | Placeholder(n) => n
  | BinOp(_, _, skel1, _) => leftmost_tm_index(skel1)
  };

let rec rightmost_tm_index = (skel: t(_)): int =>
  switch (skel) {
  | Placeholder(n) => n
  | BinOp(_, _, _, skel2) => rightmost_tm_index(skel2)
  };

let rec mk_skel_str' =
        (
          string_of_op: 'op => string,
          seq: Seq.t('operand, 'op),
          counter: ref(int),
          ph_map: Hashtbl.t(int, 'operand),
        )
        : string =>
  switch (seq) {
  | S(hd, E) =>
    let n = counter^;
    Hashtbl.add(ph_map, n, hd);
    string_of_int(n);
  | S(hd, A(op, seq)) =>
    let n = counter^;
    counter := n + 1;
    Hashtbl.add(ph_map, n, hd);
    let skel_str = mk_skel_str'(string_of_op, seq, counter, ph_map);
    let op_str = string_of_op(op);
    string_of_int(n) ++ op_str ++ skel_str;
  };

let mk_skel_str =
    (seq: Seq.t('operand, 'op), string_of_op: 'op => string): string => {
  let counter = ref(0);
  let ph_map = Hashtbl.create(8);
  mk_skel_str'(string_of_op, seq, counter, ph_map);
};
