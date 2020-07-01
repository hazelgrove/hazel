[@deriving sexp]
type operator_shape =
  | SMinus
  | SPlus
  | STimes
  | SDivide
  | SLessThan
  | SGreaterThan
  | SEquals
  | SSpace
  | SComma
  | SArrow
  | SVBar
  | SCons
  | SAnd
  | SOr;

[@deriving sexp]
type shape =
  | SList
  | SParenthesized
  | SChar(string)
  | SAsc
  | SLam
  | SListNil
  | SInj(InjSide.t)
  | SLet
  | SLine
  | SCase
  | SOp(operator_shape)
  | SApPalette(PaletteName.t);

[@deriving sexp]
type t =
  | MoveTo(CursorPath_common.t)
  | MoveLeft
  | MoveRight
  | MoveToNextHole
  | MoveToPrevHole
  | UpdateApPalette(SpliceGenMonad.t(SerializedModel.t))
  | Delete
  | Backspace
  | Construct(shape)
  | SwapLeft
  | SwapRight
  | SwapUp
  | SwapDown
  | Init;

let shape_to_string: shape => string;

module Outcome: {
  type t('success) =
    | Succeeded('success)
    | CursorEscaped(Side.t)
    | Failed;

  let map: ('success1 => 'success2, t('success1)) => t('success2);
};

let escape: Side.t => t;

let syn_insert_text_:
  (
    ~mk_syn_text: (Contexts.t, MetaVarGen.t, int, string) =>
                  Outcome.t('success),
    Contexts.t,
    MetaVarGen.t,
    (int, string),
    string
  ) =>
  Outcome.t('success);

let ana_insert_text_:
  (
    ~mk_ana_text: (Contexts.t, MetaVarGen.t, int, string, HTyp.t) =>
                  Outcome.t('success),
    Contexts.t,
    MetaVarGen.t,
    (int, string),
    string,
    HTyp.t
  ) =>
  Outcome.t('success);

let syn_backspace_text_:
  (
    ~mk_syn_text: (Contexts.t, MetaVarGen.t, int, string) =>
                  Outcome.t('success),
    Contexts.t,
    MetaVarGen.t,
    int,
    string
  ) =>
  Outcome.t('success);

let ana_backspace_text_:
  (
    ~mk_ana_text: (Contexts.t, MetaVarGen.t, int, string, HTyp.t) =>
                  Outcome.t('success),
    Contexts.t,
    MetaVarGen.t,
    int,
    string,
    HTyp.t
  ) =>
  Outcome.t('success);

let syn_delete_text_:
  (
    ~mk_syn_text: (Contexts.t, MetaVarGen.t, int, string) =>
                  Outcome.t('success),
    Contexts.t,
    MetaVarGen.t,
    int,
    string
  ) =>
  Outcome.t('success);

let ana_delete_text_:
  (
    ~mk_ana_text: (Contexts.t, MetaVarGen.t, int, string, HTyp.t) =>
                  Outcome.t('success),
    Contexts.t,
    MetaVarGen.t,
    int,
    string,
    HTyp.t
  ) =>
  Outcome.t('success);

let construct_operator_after_zoperand_:
  (
    ~is_Space: 'operator => bool,
    ~new_EmptyHole: MetaVarGen.t => ('operand, MetaVarGen.t),
    ~erase_zoperand: 'zoperand => 'operand,
    ~place_before_operand: 'operand => 'zoperand,
    ~place_after_operator: 'operator => option('zoperator),
    MetaVarGen.t,
    'operator,
    'zoperand,
    Seq.operand_surround('operand, 'operator)
  ) =>
  (ZSeq.t('operand, 'operator, 'zoperand, 'zoperator), MetaVarGen.t);

let construct_operator_before_zoperand_:
  (
    ~is_Space: 'operator => bool,
    ~new_EmptyHole: MetaVarGen.t => ('operand, MetaVarGen.t),
    ~erase_zoperand: 'zoperand => 'operand,
    ~place_before_operand: 'operand => 'zoperand,
    ~place_after_operator: 'operator => option('zoperator),
    MetaVarGen.t,
    'operator,
    'zoperand,
    Seq.operand_surround('operand, 'operator)
  ) =>
  (ZSeq.t('operand, 'operator, 'zoperand, 'zoperator), MetaVarGen.t);

let delete_operator_:
  (
    ~space: 'operator,
    ~is_EmptyHole: 'operand => bool,
    ~place_before_operand: 'operand => 'zoperand,
    ~place_after_operand: 'operand => 'zoperand,
    ~place_after_operator: 'operator => option('zoperator),
    Seq.operator_surround('operand, 'operator)
  ) =>
  ZSeq.t('operand, 'operator, 'zoperand, 'zoperator);

/**
 * Produce tuple completion upon entering comma after
 * opseq in analytic position against type ty.
 * Assumes no commas in input opseq and that ty is
 * an n-product where n >= 2.
 */
let complete_tuple_:
  (
    ~mk_ZOpSeq: ZSeq.t('operand, 'operator, 'zoperand, 'zoperator) =>
                ZOpSeq.t('operand, 'operator, 'zoperand, 'zoperator),
    ~comma: 'operator,
    ~zcomma: 'zoperator,
    ~new_EmptyHole: MetaVarGen.t => ('operand, MetaVarGen.t),
    MetaVarGen.t,
    OpSeq.t('operand, 'operator),
    HTyp.t
  ) =>
  (ZOpSeq.t('operand, 'operator, 'zoperand, 'zoperator), MetaVarGen.t);
