open ViewUtil;

type term_shape =
  | TypOperand(UHTyp.operand)
  // invariant: skels do not contain Prod
  | TypBinOp(UHTyp.operator, UHTyp.skel, UHTyp.skel)
  | TypNProd(list(UHTyp.skel))
  | PatOperand(UHPat.operand)
  // invariant: skels do not contain Comma
  | PatBinOp(ErrStatus.t, UHPat.operator, UHPat.skel, UHPat.skel)
  | PatNTuple(ErrStatus.t, list(UHPat.skel))
  | ExpOperand(UHExp.operand)
  | ExpRule(UHExp.rule)
  // invariant: skels do not contain Comma
  | ExpBinOp(ErrStatus.t, UHExp.operator, UHExp.skel, UHExp.skel)
  | ExpNTuple(ErrStatus.t, list(UHExp.skel))
  // nested blocks starting with header line
  | ExpSubBlock(UHExp.line);

type t =
  | DelimGroup
  | Padding({
      path_before: CursorPath.t,
      path_after: CursorPath.t,
    })
  | Delim({
      path: delim_path,
      caret: option(Side.t),
    })
  | Op({
      steps: CursorPath.steps,
      caret: option(Side.t),
    })
  | Text({
      steps: CursorPath.steps,
      length: int,
      caret: option(int),
    })
  | Term({
      shape: term_shape,
      has_cursor: bool,
    });

let mk_Padding = (~path_before: CursorPath.t, ~path_after: CursorPath.t): t =>
  Padding({path_before, path_after});
let mk_Delim = (~caret: option(Side.t)=?, ~path: delim_path, ()): t =>
  Delim({caret, path});
let mk_Op = (~caret: option(Side.t)=?, ~steps: CursorPath.steps, ()): t =>
  Op({caret, steps});
let mk_Text =
    (~caret: option(int)=?, ~steps: CursorPath.steps, ~length: int, ()): t =>
  Text({caret, steps, length});
let mk_Term = (~has_cursor=false, ~shape: term_shape, ()): t =>
  Term({has_cursor, shape});
