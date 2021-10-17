open Sexplib.Std;

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
  | SCommentLine
  | SList
  | SParenthesized
  | SChar(string)
  | SAnn
  | SLam
  | SListNil
  | SInj(InjSide.t)
  | SLet
  | SLine
  | SCase
  | SOp(operator_shape)
  | SApPalette(PaletteName.t);

[@deriving sexp]
type replace_operand_of_sort =
  | Exp(UHExp.operand, option(UHExp.t => ZExp.t))
  | Pat(UHPat.operand, option(UHPat.t => ZPat.t))
  | Typ(UHTyp.operand, option(UHTyp.t => ZTyp.t));

[@deriving sexp]
type t =
  | MoveTo(CursorPath.t)
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
  | Init
  | ReplaceOperand(replace_operand_of_sort)
  | ReplaceOpSeq(ZExp.zopseq);
