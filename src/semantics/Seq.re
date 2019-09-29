open Sexplib.Std;

type t('operand, 'operator) =
  /* Seq */
  | S('operand, affix('operand, 'operator))
and affix('operand, 'operator) =
  /* Empty */
  | E
  /* Affix */
  | A('operator, t('operand, 'operator));

let rec concat_affixes = (affix1: affix('operand, 'operator), affix2: affix('operand, 'operator)): affix('operand, 'operator) =>
  switch (affix1) {
  | E => affix2
  | A(op, S(operand, affix_rest)) => A(op, S(operand, concat_affixes(affix_rest, affix2)))
  };

let concat_seqs = (seq1: t('operand, 'operator), op: 'operator, seq2: t('operand, 'operator)): t('operand, 'operator) => {
  let S(hd1, tl1) = seq1;
  S(hd1, concat_affixes(tl1, A(op, seq2)));
};

/**
 * An unassociated infix operator sequence.
 */
[@deriving sexp]
type t('operand, 'operator) = ('operand, Affix.t('operand, 'operator));

let mk_affix = (op: 'operator, (first, tail): t('operand, 'operator)): Affix.t('operand, 'operator) =>
  [(op, first), ...tail];

/**
 * Concatenates two seqs.
 */
let seq_op_seq = ((first, tail): t('operand, 'operator), op: 'operator, seq: t('operand, 'operator)): t('operand, 'operator) =>
  (first, tail @ mk_affix(op, seq));

/**
 * Returns number of operands in seq.
 */
let length =
  fun
  | (_, tail) => 1 + List.length(tail);

/**
 * Returns the nth operand in seq if it exists,
 * otherwise raises `Invalid_argument`
 */
let nth_operand = (n: int, seq: t('operand, _)): 'operand =>
  switch (n, seq) {
  | (0, (first, _)) => first
  | (i, (_, tail)) => tail |> Affix.nth_operand(i - 1)
  };

let operands_of_range = (range: (int, int), seq: t('operand, _)): list('operand) =>
  switch (range, seq) {
  | ((0, 0), (first, _)) => [operand]
  | ((0, b), (first, tail)) =>
    [first, ...(tail |> Affix.operands |> sublist(b))]

let operands =
  fun
  | (first, tail) => [first, ...Affix.operands(tail)];

/*
 let rec join = (operands: ListMinTwo.t('operand), op: 'op): t('operand, 'op) =>
   switch (operands) {
   | Pair(operand1, operand2) => Operand(operand1, op, operand2)
   | Cons(operand, operands) => operand_op_seq(operand, op, join(operands, op))
   };
 */

let operators =
  fun
  | (_, tail) => Affix.operators(tail)

/* update the nth operand in seq, if it exists */
let update_nth_operand = (n: int, operand: 'operand, seq: t('operand, 'operator)): t('operand, 'operator) =>
  switch (n, seq) {
  | (0, (_, tail)) => (operand, tail)
  | (_, (first, tail)) => (first, tail |> Affix.update_nth_operand(n - 1, operand))
  };

/**
 * What surrounds a selected operand in a seq.
 * Both prefix and suffix are ordered from the
 * perspective of the selected operand, i.e.,
 * operands neighboring selected operand come first.
 */
[@deriving sexp]
type surround('operand, 'op) = (
  prefix('operand, 'op),
  suffix('operand, 'op),
)
and prefix('operand, 'op) = affix('operand, 'op)
and suffix('operand, 'op) = affix('operand, 'op)
and affix('operand, 'op) = list(('op, 'operand));

let empty_prefix = [];
let empty_suffix = [];

let mk_surround = (~prefix=empty_prefix, ~suffix=empty_suffix, ()) => (
  prefix,
  suffix,
);
let empty_surround = mk_surround();

let operands_of_affix = (affix: affix('operand, _)): list('operand) =>
  affix |> List.map(((_, operand)) => operand);

let operands_of_surround =
    ((prefix, suffix): surround('operand, _))
    : (list('operand), list('operand)) => (
  operands_of_affix(prefix),
  operands_of_affix(suffix) |> List.rev,
);

let ops_of_affix = (affix: affix(_, 'op)): list('op) =>
  affix |> List.map(((op, _)) => op);

let ops_of_surround =
    ((prefix, suffix): surround(_, 'op)): (list('op), list('op)) => (
  ops_of_affix(prefix),
  ops_of_affix(suffix) |> List.rev,
);

let rec split_prefix_and_last =
        (seq: t('operand, 'op)): (prefix('operand, 'op), 'operand) =>
  switch (seq) {
  | Operand(operand) => (empty_prefix, operand)
  | Seq(seq, op, operand) =>
    let (prefix, last) = split_prefix_and_last(seq);
    ([(op, operand), ...prefix], last);
  };

let rec split_first_and_suffix =
        (seq: t('operand, 'op)): ('operand, suffix('operand, 'op)) =>
  switch (seq) {
  | Operand(operand) => (operand, empty_suffix)
  | Seq(seq, op, operand) =>
    let (first, suffix) = split_first_and_suffix(seq);
    (first, suffix @ [(op, operand)]);
  };

let rec split =
        (n: int, seq: t('operand, 'op))
        : option(('operand, surround('operand, 'op))) =>
  switch (n, seq) {
  | (0, Operand(operand)) => Some((operand, empty_surround))
  | (_, Operand(_)) => None
  | (_, Seq(seq, op, operand)) =>
    let length = length(seq);
    if (n < length) {
      switch (split(n, seq)) {
      | None => None
      | Some((found, (prefix, suffix))) =>
        Some((
          found,
          mk_surround(~prefix, ~suffix=suffix @ [(op, operand)], ()),
        ))
      };
    } else if (n === length) {
      let (prefix, last) = split_prefix_and_last(seq);
      Some((operand, mk_surround(~prefix=[(op, last), ...prefix], ())));
    } else {
      None;
    };
  };

let prefix_length = List.length;
let suffix_length = List.length;

let surround_prefix_length = ((prefix, _): surround(_, _)): int =>
  prefix_length(prefix);
let surround_suffix_length = ((_, suffix): surround(_, _)): int =>
  suffix_length(suffix);

let rec t_of_prefix_and_seq = (prefix, seq) =>
  switch (prefix) {
  | [] => seq
  | [(op, operand), ...prefix_rest] =>
    t_of_prefix_and_seq(prefix_rest, operand_op_seq(operand, op, seq))
  };

let rec t_of_seq_and_suffix = (seq, suffix) =>
  switch (suffix) {
  | [] => seq
  | [(op, operand), ...suffix_rest] =>
    t_of_seq_and_suffix(Seq(seq, op, operand), suffix_rest)
  };

let t_of_prefix_and_last =
    (prefix: prefix('operand, 'op), last: 'operand): t('operand, 'op) =>
  t_of_prefix_and_seq(prefix, Operand(last));

let t_of_first_and_suffix =
    (first: 'operand, suffix: suffix('operand, 'op)): t('operand, 'op) =>
  t_of_seq_and_suffix(Operand(first), suffix);

let t_of_operand_and_surround =
    (operand: 'operand, (prefix, suffix): surround('operand, 'op)) =>
  t_of_prefix_and_seq(prefix, t_of_first_and_suffix(operand, suffix));

let nest_surrounds =
    (
      (inner_prefix, inner_suffix): surround('operand, 'op),
      (outer_prefix, outer_suffix): surround('operand, 'op),
    )
    : surround('operand, 'op) => (
  inner_prefix @ outer_prefix,
  inner_suffix @ outer_suffix,
);
