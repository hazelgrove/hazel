[@deriving sexp]
type steps = list(ChildIndex.t);

[@deriving sexp]
type rev_steps = steps;

[@deriving sexp]
type t = (steps, CursorPosition.t);

[@deriving sexp]
type rev_t = (CursorPosition.t, rev_steps);

let rev: rev_t => t;

let cons': (int, t) => t;

let of_zopseq_:
  (
    ~of_zoperand: 'zoperand => t,
    ZOpSeq.t('a, 'b, 'zoperand, (CursorPosition.t, 'c))
  ) =>
  t;

[@deriving sexp]
type hole_desc =
  | TypHole
  | PatHole(MetaVar.t)
  | ExpHole(MetaVar.t);

[@deriving sexp]
type hole_list = list((hole_desc, steps));

/* two hole lists, one for before the cursor, one for after */
[@deriving sexp]
type zhole_list = {
  holes_before: hole_list,
  hole_selected: option((hole_desc, steps)),
  holes_after: hole_list,
};

let mk_zholes:
  (
    ~holes_before: hole_list=?,
    ~hole_selected: option((hole_desc, steps))=?,
    ~holes_after: hole_list=?,
    unit
  ) =>
  zhole_list;

let no_holes: zhole_list;

let prev_hole_steps: zhole_list => option(steps);

let next_hole_steps: zhole_list => option(steps);

let follow_opseq_:
  (
    ~follow_operand: (t, 'operand) => option('zoperand),
    ~follow_operator: (t, 'operator) => option('zoperator),
    t,
    OpSeq.t('operand, 'operator)
  ) =>
  option(ZOpSeq.t('operand, 'operator, 'zoperand, 'zoperator));

let of_steps_opseq_:
  (
    ~of_steps_operand: (steps, ~side: Side.t, 'operand) => option(t),
    ~of_steps_operator: (steps, ~side: Side.t, 'operator) => option(t),
    steps,
    ~side: Side.t,
    OpSeq.t('operand, 'operator)
  ) =>
  option(t);

let holes_err:
  (~hole_desc: MetaVar.t => hole_desc, ErrStatus.t, rev_steps, hole_list) =>
  hole_list;

let holes_verr:
  (~hole_desc: MetaVar.t => hole_desc, VarErrStatus.t, rev_steps, hole_list) =>
  hole_list;

let holes_case_err:
  (
    ~hole_desc: MetaVar.t => hole_desc,
    CaseErrStatus.t,
    rev_steps,
    hole_list
  ) =>
  hole_list;

let holes_skel_:
  (
    ~holes_operand: ('operand, steps, hole_list) => hole_list,
    ~hole_desc: MetaVar.t => hole_desc,
    ~is_space: 'operator => bool,
    ~rev_steps: rev_steps,
    Skel.t('operator),
    Seq.t('operand, 'operator),
    hole_list
  ) =>
  hole_list;

let holes_opseq:
  (
    ~holes_operand: ('operand, steps, hole_list) => hole_list,
    ~hole_desc: MetaVar.t => hole_desc,
    ~is_space: 'operator => bool,
    ~rev_steps: rev_steps,
    OpSeq.t('operand, 'operator),
    hole_list
  ) =>
  hole_list;

let holes_zopseq_:
  (
    ~holes_operand: ('operand, rev_steps, hole_list) => hole_list,
    ~holes_zoperand: ('zoperand, rev_steps) => zhole_list,
    ~hole_desc: MetaVar.t => hole_desc,
    ~is_space: 'operator => bool,
    ~rev_steps: rev_steps,
    ~erase_zopseq: ZOpSeq.t('operand, 'operator, 'zoperand, 'zoperator) =>
                   OpSeq.t('operand, 'operator),
    ZOpSeq.t('operand, 'operator, 'zoperand, 'zoperator)
  ) =>
  zhole_list;

let append: (t, list(ChildIndex.t)) => t;

let steps_to_hole: (hole_list, MetaVar.t) => option(steps);

let steps_to_hole_z: (zhole_list, MetaVar.t) => option(steps);

let opt_steps_to_opt_path: (CursorPosition.t, option(steps)) => option(t);

let is_prefix_of: (list('a), list('a)) => bool;

let compare_steps: (list('a), list('a)) => int;
