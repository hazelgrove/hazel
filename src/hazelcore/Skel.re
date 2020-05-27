open Sexplib.Std;

[@deriving sexp]
type t('op) =
  | Placeholder(int)
  | BinOp(ErrStatus.t, 'op, t('op), t('op));

[@deriving sexp]
type range = (int, int);

let rec leftmost_op =
  fun
  | Placeholder(_) => None
  | BinOp(_, op, skel1, _) =>
    switch (leftmost_op(skel1)) {
    | Some(op1) => Some(op1)
    | None => Some(op)
    };

let rec rightmost_op =
  fun
  | Placeholder(_) => None
  | BinOp(_, op, _, skel2) =>
    switch (rightmost_op(skel2)) {
    | Some(op1) => Some(op1)
    | None => Some(op)
    };

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

let rec range = (skel: t(_)): range =>
  switch (skel) {
  | Placeholder(n) => (n, n)
  | BinOp(_, _, skel1, skel2) =>
    let (a, _) = range(skel1);
    let (_, b) = range(skel2);
    (a, b);
  };

// return bool is for internal use,
// indicates whether the accompanying
// skel is the target subskel
let rec _subskel_rooted_at_op =
        (op_index: OpIndex.t, skel: t('op)): (bool, t('op)) =>
  switch (skel) {
  | Placeholder(_) => (false, skel)
  | BinOp(_, _, skel1, skel2) =>
    let (found1, subskel1) = _subskel_rooted_at_op(op_index, skel1);
    let (found2, subskel2) = _subskel_rooted_at_op(op_index, skel2);
    switch (found1, found2) {
    | (true, _) => (true, subskel1)
    | (_, true) => (true, subskel2)
    | (false, false) => (leftmost_tm_index(subskel2) == op_index, skel)
    };
  };
let subskel_rooted_at_op = (op_index: OpIndex.t, skel: t('op)): t('op) => {
  let (_, subskel) = _subskel_rooted_at_op(op_index, skel);
  subskel;
};

let range_of_subskel_rooted_at_op = (op_index, skel) =>
  skel |> subskel_rooted_at_op(op_index) |> range;

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
