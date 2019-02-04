module OperatorSeq =
 struct
  type ('tm, 'op) opseq =
  | ExpOpExp of 'tm * 'op * 'tm
  | SeqOpExp of ('tm, 'op) opseq * 'op * 'tm

  (* concatenates two opseqs *)
  let rec seq_op_seq seq1 op1 = function
  | ExpOpExp (e1, op2, e2) -> SeqOpExp ((SeqOpExp (seq1, op1, e1)), op2, e2)
  | SeqOpExp (seq2', op2, ue') ->
    SeqOpExp ((seq_op_seq seq1 op1 seq2'), op2, ue')

  (* prepends an expression to seq *)
  let rec exp_op_seq e1 op1 = function
  | ExpOpExp (e2, op2, e3) -> SeqOpExp ((ExpOpExp (e1, op1, e2)), op2, e3)
  | SeqOpExp (seq', op', e') -> SeqOpExp ((exp_op_seq e1 op1 seq'), op', e')

  (* returns number of expressions in seq (not ops) *)
  let rec seq_length = function
  | ExpOpExp (_, _, _) -> 2
  | SeqOpExp (seq', _, _) -> 1 + (seq_length seq')

  (* nth expression in seq, if it exists *)
  let rec seq_nth n0 seq =
    match (n, seq) with
    | (0, ExpOpExp (e1, _, _)) -> Some e1
    | (1, ExpOpExp (_, _, e2)) -> Some e2
    | (_, ExpOpExp (_, _, _)) -> None
    | (_, SeqOpExp (seq', _, e)) ->
      let len = seq_length seq' in
      if eqb n len then Some e else seq_nth n seq'

  (* update the nth expression in seq, if it exists *)
  let rec seq_update_nth n0 seq e =
    match (n, seq) with
    | (0, ExpOpExp (_, op, e2)) -> Some (ExpOpExp (e, op, e2))
    | (1, ExpOpExp (e1, op, _)) -> Some (ExpOpExp (e1, op, e))
    | (_, ExpOpExp (_, _, _)) -> None
    | (_, SeqOpExp (seq', op, e')) ->
      let len = seq_length seq' in
      if eqb n len then Some (SeqOpExp seq' op e)
      else match seq_update_nth n seq' e with
      | Some seq'' => Some (SeqOpExp seq'' op e')
      | None => None

  type ('tm, 'op) opseq_surround =
  (* set up this way to enforce the requirement that there be at least one op *)
  (* if the prefix is empty, there must be a non-empty suffix *)
  | EmptyPrefix of ('tm, 'op) opseq_suffix
  (* if the suffix is empty, there must be a non-empty prefix *)
  | EmptySuffix of ('tm, 'op) opseq_prefix
  (* both can be non-empty *)
  | BothNonEmpty of ('tm, 'op) opseq_prefix * ('tm, 'op) opseq_suffix
  and ('tm, 'op) opseq_prefix =
  (* a non-empty prefix is either one that contains a single expression *)
  | ExpPrefix of 'tm * 'op
  (* or one that contains two or more expressions, i.e. another opseq *)
  | SeqPrefix of ('tm, 'op) opseq * 'op
  and ('tm, 'op) opseq_suffix =
  (* analagous to opseq_prefix *)
  | ExpSuffix of 'op * 'tm
  | SeqSuffix of 'op * ('tm, 'op) opseq

  (* append an exp to a prefix *)
  let prefix_append_exp prefix e op2 =
    match prefix with
    | ExpPrefix (e1, op1) -> SeqPrefix ((ExpOpExp (e1, op1, e)), op2)
    | SeqPrefix (seq1, op1) -> SeqPrefix ((SeqOpExp (seq1, op1, e)), op2)

  (* prepend an exp to a suffix *)
  let suffix_prepend_exp suffix op1 e =
    match suffix with
    | ExpSuffix (op2, e') -> SeqSuffix (op1, (ExpOpExp (e, op2, e')))
    | SeqSuffix (op2, seq') -> SeqSuffix (op1, (exp_op_seq e op2 seq'))

  (* append an exp to a suffix *)
  let suffix_append_exp suffix op2 e =
    match suffix with
    | ExpSuffix (op1, e') -> SeqSuffix (op1, (ExpOpExp (e', op2, e)))
    | SeqSuffix (op1, seq) -> SeqSuffix (op1, (SeqOpExp (seq, op2, e)))

  (* append an exp to the suffix of a surround *)
  let surround_suffix_append_exp surround op1 e =
    match surround with
    | EmptyPrefix suffix ->
      let suffix' = suffix_append_exp suffix op1 e in EmptyPrefix suffix'
    | EmptySuffix prefix ->
      let suffix' = ExpSuffix (op1, e) in BothNonEmpty (prefix, suffix')
    | BothNonEmpty (prefix, suffix) ->
      let suffix' = suffix_append_exp suffix op1 e in
      BothNonEmpty (prefix, suffix')

  let rec split n0 seq =
    match (n, seq) with
    | (0, OperatorSeq.ExpOpExp (e1, op, e2)) ->
      Some (e1, EmptyPrefix (ExpSuffix (op, e2)))
    | (1, OperatorSeq.ExpOpExp (e1, op, e2)) ->
      Some (e2, EmptySuffix (ExpPrefix (e1, op)))
    | (_, OperatorSeq.ExpOpExp (_, _, _)) ->
      None
    | (_, OperatorSeq.SeqOpExp (seq', op, e)) ->
      let length' = OperatorSeq.seq_length seq' in
      if ltb n length' then
        match split n seq' with
        | Some (e', surround) ->
          let surround' = surround_suffix_append_exp surround op e in
          Some (e', surround')
        | None -> None
      else if eqb n length' then
        let prefix' = SeqPrefix (seq', op) in
        let surround' = EmptySuffix prefix' in
        Some (e, surround')
      else None

  let rec split0 = function
  | ExpOpExp (e1, op, e2) -> e1,(ExpSuffix (op, e2))
  | SeqOpExp (seq', op, e) ->
    let e0,suffix' = split0 seq' in e0,(suffix_append_exp suffix' op e)

  let split_tail = function
  | ExpOpExp (e1, op, e2) -> e2,(ExpPrefix (e1, op))
  | SeqOpExp (seq', op, e) -> e,(SeqPrefix (seq', op))

  let prefix_length = function
  | ExpPrefix (_, _) -> 1
  | SeqPrefix (seq, _) -> seq_length seq

  let surround_prefix_length = function
  | EmptyPrefix _ -> 0
  | EmptySuffix prefix -> prefix_length prefix
  | BothNonEmpty (prefix, _) -> prefix_length prefix

  let suffix_length = function
  | ExpSuffix (_, _) -> 1
  | SeqSuffix (_, seq) -> seq_length seq

  let surround_suffix_length = function
  | EmptyPrefix suffix -> suffix_length suffix
  | EmptySuffix _ -> 0
  | BothNonEmpty (_, suffix) -> suffix_length suffix

  let opseq_of_exp_and_surround e = function
  | EmptyPrefix suffix ->
    (match suffix with
     | ExpSuffix (op, e2) -> ExpOpExp (e, op, e2)
     | SeqSuffix (op, seq) -> exp_op_seq e op seq)
  | EmptySuffix prefix ->
    (match prefix with
     | ExpPrefix (e1, op) -> ExpOpExp (e1, op, e)
     | SeqPrefix (seq, op) -> SeqOpExp (seq, op, e))
  | BothNonEmpty (prefix, suffix) ->
    (match prefix with
     | ExpPrefix (e1, op1) ->
       (match suffix with
        | ExpSuffix (op2, e2) -> SeqOpExp ((ExpOpExp (e1, op1, e)), op2, e2)
        | SeqSuffix (op2, seq2) ->
          seq_op_seq (ExpOpExp (e1, op1, e)) op2 seq2)
     | SeqPrefix (seq1, op1) ->
       (match suffix with
        | ExpSuffix (op2, e2) ->
          SeqOpExp ((SeqOpExp (seq1, op1, e)), op2, e2)
        | SeqSuffix (op2, seq2) ->
          seq_op_seq (SeqOpExp (seq1, op1, e)) op2 seq2))
 end
