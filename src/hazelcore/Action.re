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
  | SOr
  | SCaret;

[@deriving sexp]
type shape =
  | SCommentLine
  | SList
  | SQuote
  | SLeftBracket
  | SParenthesized
  | SChar(string)
  | SAnn
  | SLam
  | SListNil
  | SInj(InjSide.t)
  | SLet
  | SAbbrev
  | SLivelitDef
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
  | PerformLivelitAction(SerializedAction.t)
  | Delete
  | Backspace
  | Construct(shape)
  | SwapLeft
  | SwapRight
  | SwapUp
  | SwapDown
  | Init;

let is_movement =
  fun
  | MoveTo(_)
  | MoveLeft
  | MoveRight
  | MoveToPrevHole
  | MoveToNextHole => true
  | _ => false;
