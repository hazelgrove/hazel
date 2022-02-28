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
  | SCloseParens
  | SCloseBraces
  | SCloseSquareBracket
  | SChar(string)
  | SAnn
  | SLam
  | SListNil
  | SInj
  | SLet
  | SLine
  | SCase
  | SOp(operator_shape);

[@deriving sexp]
type t =
  | MoveTo(CursorPath.t)
  | MoveLeft
  | MoveRight
  | MoveToNextHole
  | MoveToPrevHole
  | Delete
  | Backspace
  | Construct(shape)
  | SwapLeft
  | SwapRight
  | SwapUp
  | SwapDown
  | Init;
