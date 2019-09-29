open GeneralUtil;

/**
 * An unassociated infix operator sequence.
 * Also used to represent the prefix or suffix
 * of a selected operator in a seq, in both
 * cases such that the head operand neighbors
 * the selected operator.
 */
[@deriving sexp]
type t('operand, 'operator) =
  /* Seq */
  | S('operand, affix('operand, 'operator))
/**
 * An unassociated infix operator sequence
 * without a head operand. Used to represent
 * the prefix or suffix of a selected operand
 * in a seq, in both cases such that the head
 * operator neighbors the selected operand.
 */
and affix('operand, 'operator) =
  /* Empty */
  | E
  /* Affix */
  | A('operator, t('operand, 'operator));

let rec mk_affix =
        (op_pairs: list(('operator, 'operand))): affix('operand, 'operator) =>
  switch (op_pairs) {
  | [] => E
  | [(op, operand), ...rest] => A(op, S(operand, mk_affix(rest)))
  };
let mk =
    (hd: 'operand, tl: list(('operator, 'operand))): t('operand, 'operator) =>
  S(hd, mk_affix(tl));

let rev = (seq: t('operand, 'operator)): t('operand, 'operator) => {
  let rec rev_t = (rev: affix(_, _), seq) => {
    let S(hd, tl) = seq;
    rev_affix(S(hd, rev), tl);
  }
  and rev_affix = (rev: t(_, _), affix) =>
    switch (affix) {
    | E => rev
    | A(op, seq) => rev_t(A(op, rev), seq)
    };
  rev_t(E, seq);
};

let rec affix_affix =
        (
          affix1: affix('operand, 'operator),
          affix2: affix('operand, 'operator),
        )
        : affix('operand, 'operator) =>
  switch (affix1) {
  | E => affix2
  | A(op, S(hd, tl)) => A(op, S(hd, affix_affix(tl, affix2)))
  };

let seq_op_seq =
    (
      seq1: t('operand, 'operator),
      op: 'operator,
      seq2: t('operand, 'operator),
    )
    : t('operand, 'operator) => {
  let S(hd1, tl1) = seq1;
  S(hd1, affix_affix(tl1, A(op, seq2)));
};

let prefix_seq =
    (prefix: affix('operand, 'operator), seq: t('operand, 'operator))
    : t('operand, 'operator) =>
  switch (prefix) {
  | E => seq
  | A(op, prefix_seq) => seq_op_seq(prefix_seq |> rev, op, seq)
  };

let seq_suffix =
    (seq: t('operand, 'operator), suffix: affix('operand, 'operator))
    : t('operand, 'operator) =>
  switch (suffix) {
  | E => seq
  | A(op, suffix_seq) => seq_op_seq(seq, op, suffix_seq)
  };

/**
 * Returns the number of operands.
 */
let rec length =
  fun
  | S(_, tail) => 1 + length_of_affix(tail)
and length_of_affix =
  fun
  | E => 0
  | A(_, seq) => length(seq);

/**
 * Returns the nth operand in seq if it exists,
 * otherwise raises `Invalid_argument`
 */
let rec nth_operand = (n: int, seq: t('operand, _)): 'operand => {
  let S(hd, tl) = seq;
  n === 0 ? hd : tl |> nth_operand_of_affix(n - 1);
}
and nth_operand_of_affix = (n: int, affix: affix('operand, _)): 'operand =>
  switch (affix) {
  | E => raise(Invalid_argument("Seq.nth_operand_of_affix"))
  | A(_, seq) => seq |> nth_operand(n)
  };

let operands_in_range =
    ((a, b): (int, int), seq: t('operand, _)): list('operand) =>
  range(~lo=a, b + 1) |> List.map(n => seq |> nth_operand(n));

let rec operands =
  fun
  | S(hd, tl) => [hd, ...operands_of_affix(tl)]
and operands_of_affix =
  fun
  | E => []
  | A(_, seq) => operands(seq);

/*
 let rec join = (operands: ListMinTwo.t('operand), op: 'op): t('operand, 'op) =>
   switch (operands) {
   | Pair(operand1, operand2) => Operand(operand1, op, operand2)
   | Cons(operand, operands) => operand_op_seq(operand, op, join(operands, op))
   };
 */

let rec operators =
  fun
  | S(_, tl) => operators_of_affix(tl)
and operators_of_affix =
  fun
  | E => []
  | A(op, seq) => [op, ...operators(seq)];

/* update the nth operand in seq, if it exists */
let rec update_nth_operand =
        (n: int, operand: 'operand, seq: t('operand, 'operator))
        : t('operand, 'operator) =>
  switch (n, seq) {
  | (0, S(_, tl)) => S(operand, tl)
  | (_, S(hd, tl)) =>
    S(hd, tl |> update_nth_operand_of_affix(n - 1, operand))
  }
and update_nth_operand_of_affix =
    (n: int, operand: 'operand, affix: affix('operand, 'operator))
    : affix('operand, 'operator) =>
  switch (affix) {
  | E => E
  | A(op, seq) => A(op, seq |> update_nth_operand(n, operand))
  };

let rec split_nth_operand =
        (n: int, seq: t('operand, 'operator))
        : (
            'operand,
            (affix('operand, 'operator), affix('operand, 'operator)),
          ) => {
  let exn = Invalid_argument("Seq.split_nth_operand");
  switch (n, seq) {
  | (_, _) when n < 0 => raise(exn)
  | (0, S(hd, tl)) => (hd, (E, tl))
  | (_, S(_, E)) => raise(exn)
  | (_, S(hd, A(op, seq))) =>
    let (found, (prefix, suffix)) = seq |> split_nth_operand(n - 1);
    (found, (affix_affix(prefix, A(op, S(hd, E))), suffix));
  };
};

let rec split_nth_operator =
        (n: int, seq: t('operand, 'operator))
        : ('operator, (t('operand, 'operator), t('operand, 'operator))) => {
  let exn = Invalid_argument("Seq.split_nth_operator");
  switch (n, seq) {
  | (_, _) when n < 0 => raise(exn)
  | (_, S(_, E)) => raise(exn)
  | (0, S(hd, A(op, seq))) => (op, (S(hd, E), seq))
  | (_, S(hd, A(op, seq))) =>
    let (found, (prefix, suffix)) = seq |> split_nth_operator(n - 1);
    (found, (seq_suffix(prefix, A(op, S(hd, E))), suffix));
  };
};

let split_first_and_suffix = seq => {
  let (first, (_, suffix)) = split_nth_operand(0, seq);
  (first, suffix);
};
let split_prefix_and_last = seq => {
  let (last, (prefix, _)) = split_nth_operand(length(seq) - 1, seq);
  (prefix, last);
};
