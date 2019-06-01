[@deriving (sexp, show)]
type opseq('tm, 'op) =
  | ExpOpExp('tm, 'op, 'tm)
  | SeqOpExp(opseq('tm, 'op), 'op, 'tm);

/* concatenates two opseqs */
let rec seq_op_seq = (seq1, op1) =>
  fun
  | ExpOpExp(e1, op2, e2) => SeqOpExp(SeqOpExp(seq1, op1, e1), op2, e2)
  | SeqOpExp(seq2', op2, ue') =>
    SeqOpExp(seq_op_seq(seq1, op1, seq2'), op2, ue');

/* prepends an expression to seq */
let rec exp_op_seq = (e1, op1) =>
  fun
  | ExpOpExp(e2, op2, e3) => SeqOpExp(ExpOpExp(e1, op1, e2), op2, e3)
  | SeqOpExp(seq', op', e') => SeqOpExp(exp_op_seq(e1, op1, seq'), op', e');

/* returns number of expressions in seq (not ops) */
let rec seq_length =
  fun
  | ExpOpExp(_, _, _) => 2
  | SeqOpExp(seq', _, _) => 1 + seq_length(seq');

/* nth expression in seq, if it exists */
let rec seq_nth = (n, seq) =>
  switch (n, seq) {
  | (0, ExpOpExp(e1, _, _)) => Some(e1)
  | (1, ExpOpExp(_, _, e2)) => Some(e2)
  | (_, ExpOpExp(_, _, _)) => None
  | (_, SeqOpExp(seq', _, e)) =>
    let len = seq_length(seq');
    if (n === len) {
      Some(e);
    } else {
      seq_nth(n, seq');
    };
  };

/* update the nth expression in seq, if it exists */
let rec seq_update_nth = (n, seq, e) =>
  switch (n, seq) {
  | (0, ExpOpExp(_, op, e2)) => Some(ExpOpExp(e, op, e2))
  | (1, ExpOpExp(e1, op, _)) => Some(ExpOpExp(e1, op, e))
  | (_, ExpOpExp(_, _, _)) => None
  | (_, SeqOpExp(seq', op, e')) =>
    let len = seq_length(seq');
    if (n === len) {
      Some(SeqOpExp(seq', op, e));
    } else {
      switch (seq_update_nth(n, seq', e)) {
      | Some(seq'') => Some(SeqOpExp(seq'', op, e'))
      | None => None
      };
    };
  };

/* set up this way to enforce the requirement that there be at least one op */
[@deriving (show, sexp)]
type opseq_surround('tm, 'op) =
  /* if the prefix is empty, there must be a non-empty suffix */
  | EmptyPrefix(opseq_suffix('tm, 'op))
  /* if the suffix is empty, there must be a non-empty prefix */
  | EmptySuffix(opseq_prefix('tm, 'op))
  /* both can be non-empty */
  | BothNonEmpty(opseq_prefix('tm, 'op), opseq_suffix('tm, 'op))
and opseq_prefix('tm, 'op) =
  /* a non-empty prefix is either one that contains a single expression */
  | ExpPrefix('tm, 'op)
  /* or one that contains two or more expressions, i.e. another opseq */
  | SeqPrefix(opseq('tm, 'op), 'op)
and opseq_suffix('tm, 'op) =
  /* analagous to opseq_prefix */
  | ExpSuffix('op, 'tm)
  | SeqSuffix('op, opseq('tm, 'op));

/* append an exp to a prefix */
let prefix_append_exp = (prefix, e, op2) =>
  switch (prefix) {
  | ExpPrefix(e1, op1) => SeqPrefix(ExpOpExp(e1, op1, e), op2)
  | SeqPrefix(seq1, op1) => SeqPrefix(SeqOpExp(seq1, op1, e), op2)
  };

/* prepend an exp to a suffix */
let suffix_prepend_exp = (suffix, op1, e) =>
  switch (suffix) {
  | ExpSuffix(op2, e') => SeqSuffix(op1, ExpOpExp(e, op2, e'))
  | SeqSuffix(op2, seq') => SeqSuffix(op1, exp_op_seq(e, op2, seq'))
  };

/* append an exp to a suffix */
let suffix_append_exp = (suffix, op2, e) =>
  switch (suffix) {
  | ExpSuffix(op1, e') => SeqSuffix(op1, ExpOpExp(e', op2, e))
  | SeqSuffix(op1, seq) => SeqSuffix(op1, SeqOpExp(seq, op2, e))
  };

/* append an exp to the suffix of a surround */
let surround_suffix_append_exp = (surround, op1, e) =>
  switch (surround) {
  | EmptyPrefix(suffix) =>
    let suffix' = suffix_append_exp(suffix, op1, e);
    EmptyPrefix(suffix');
  | EmptySuffix(prefix) =>
    let suffix' = ExpSuffix(op1, e);
    BothNonEmpty(prefix, suffix');
  | BothNonEmpty(prefix, suffix) =>
    let suffix' = suffix_append_exp(suffix, op1, e);
    BothNonEmpty(prefix, suffix');
  };

let rec split = (n, seq) =>
  switch (n, seq) {
  | (0, ExpOpExp(e1, op, e2)) =>
    Some((e1, EmptyPrefix(ExpSuffix(op, e2))))
  | (1, ExpOpExp(e1, op, e2)) =>
    Some((e2, EmptySuffix(ExpPrefix(e1, op))))
  | (_, ExpOpExp(_, _, _)) => None
  | (_, SeqOpExp(seq', op, e)) =>
    let length' = seq_length(seq');
    if (n < length') {
      switch (split(n, seq')) {
      | Some((e', surround)) =>
        let surround' = surround_suffix_append_exp(surround, op, e);
        Some((e', surround'));
      | None => None
      };
    } else if (n === length') {
      let prefix' = SeqPrefix(seq', op);
      let surround' = EmptySuffix(prefix');
      Some((e, surround'));
    } else {
      None;
    };
  };

let rec split0 =
  fun
  | ExpOpExp(e1, op, e2) => (e1, ExpSuffix(op, e2))
  | SeqOpExp(seq', op, e) => {
      let (e0, suffix') = split0(seq');
      (e0, suffix_append_exp(suffix', op, e));
    };

let split_tail =
  fun
  | ExpOpExp(e1, op, e2) => (e2, ExpPrefix(e1, op))
  | SeqOpExp(seq', op, e) => (e, SeqPrefix(seq', op));

let prefix_length =
  fun
  | ExpPrefix(_, _) => 1
  | SeqPrefix(seq, _) => seq_length(seq);

let surround_prefix_length =
  fun
  | EmptyPrefix(_) => 0
  | EmptySuffix(prefix) => prefix_length(prefix)
  | BothNonEmpty(prefix, _) => prefix_length(prefix);

let suffix_length =
  fun
  | ExpSuffix(_, _) => 1
  | SeqSuffix(_, seq) => seq_length(seq);

let surround_suffix_length =
  fun
  | EmptyPrefix(suffix) => suffix_length(suffix)
  | EmptySuffix(_) => 0
  | BothNonEmpty(_, suffix) => suffix_length(suffix);

let prefix_nth = (n: int, prefix: opseq_prefix('tm, 'op)): option('tm) =>
  switch (n, prefix) {
  | (0, ExpPrefix(tm, _)) => Some(tm)
  | (_, ExpPrefix(_, _)) => None
  | (_, SeqPrefix(seq, _)) => seq_nth(n, seq)
  };

let prefix_update_nth =
    (n: int, prefix: opseq_prefix('tm, 'op), tm: 'tm)
    : option(opseq_prefix('tm, 'op)) =>
  switch (n, prefix) {
  | (0, ExpPrefix(_, op)) => Some(ExpPrefix(tm, op))
  | (_, ExpPrefix(_, _)) => None
  | (_, SeqPrefix(seq, op)) =>
    switch (seq_update_nth(n, seq, tm)) {
    | None => None
    | Some(seq) => Some(SeqPrefix(seq, op))
    }
  };

let suffix_nth = (n: int, suffix: opseq_suffix('tm, 'op)): option('tm) =>
  switch (n, suffix) {
  | (0, ExpSuffix(_, tm)) => Some(tm)
  | (_, ExpSuffix(_, _)) => None
  | (_, SeqSuffix(_, seq)) => seq_nth(n, seq)
  };

let suffix_update_nth =
    (n: int, suffix: opseq_suffix('tm, 'op), tm: 'tm)
    : option(opseq_suffix('tm, 'op)) =>
  switch (n, suffix) {
  | (0, ExpSuffix(op, _)) => Some(ExpSuffix(op, tm))
  | (_, ExpSuffix(_, _)) => None
  | (_, SeqSuffix(op, seq)) =>
    switch (seq_update_nth(n, seq, tm)) {
    | None => None
    | Some(seq) => Some(SeqSuffix(op, seq))
    }
  };

let surround_nth = (n: int, surround: opseq_surround('tm, 'op)): option('tm) =>
  switch (surround) {
  | EmptyPrefix(suffix) => suffix_nth(n - 1, suffix)
  | EmptySuffix(prefix) => prefix_nth(n, prefix)
  | BothNonEmpty(prefix, suffix) =>
    switch (prefix_nth(n, prefix)) {
    | Some(_) as result => result
    | None => suffix_nth(n - 1 - prefix_length(prefix), suffix)
    }
  };

let surround_update_nth =
    (n: int, surround: opseq_surround('tm, 'op), tm: 'tm)
    : option(opseq_surround('tm, 'op)) =>
  switch (surround) {
  | EmptyPrefix(suffix) =>
    switch (suffix_update_nth(n - 1, suffix, tm)) {
    | None => None
    | Some(suffix) => Some(EmptyPrefix(suffix))
    }
  | EmptySuffix(prefix) =>
    switch (prefix_update_nth(n, prefix, tm)) {
    | None => None
    | Some(prefix) => Some(EmptySuffix(prefix))
    }
  | BothNonEmpty(prefix, suffix) =>
    switch (prefix_update_nth(n, prefix, tm)) {
    | Some(prefix) => Some(BothNonEmpty(prefix, suffix))
    | None =>
      switch (suffix_update_nth(n - 1 - prefix_length(prefix), suffix, tm)) {
      | None => None
      | Some(suffix) => Some(BothNonEmpty(prefix, suffix))
      }
    }
  };

let opseq_of_prefix_and_exp =
    (prefix: opseq_prefix('tm, 'op), e1: 'tm): opseq('tm, 'op) =>
  switch (prefix) {
  | ExpPrefix(e2, op) => ExpOpExp(e2, op, e1)
  | SeqPrefix(seq, op) => SeqOpExp(seq, op, e1)
  };

let opseq_of_exp_and_suffix =
    (e1: 'tm, suffix: opseq_suffix('tm, 'op)): opseq('tm, 'op) =>
  switch (suffix) {
  | ExpSuffix(op, e2) => ExpOpExp(e1, op, e2)
  | SeqSuffix(op, seq) => exp_op_seq(e1, op, seq)
  };

let rec opseq_of_prefix_and_seq = (prefix, seq) =>
  switch (prefix) {
  | ExpPrefix(e, op) => exp_op_seq(e, op, seq)
  | SeqPrefix(prefix_seq, op2) =>
    let (sub_prefix, e2) =
      switch (prefix_seq) {
      | ExpOpExp(e1, op1, e2) => (ExpPrefix(e1, op1), e2)
      | SeqOpExp(seq1, op1, e2) => (SeqPrefix(seq1, op1), e2)
      };
    opseq_of_prefix_and_seq(sub_prefix, exp_op_seq(e2, op2, seq));
  };

let opseq_of_exp_and_surround = e =>
  fun
  | EmptyPrefix(suffix) => opseq_of_exp_and_suffix(e, suffix)
  | EmptySuffix(prefix) => opseq_of_prefix_and_exp(prefix, e)
  | BothNonEmpty(prefix, suffix) =>
    opseq_of_prefix_and_seq(prefix, opseq_of_exp_and_suffix(e, suffix));
