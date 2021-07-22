[@deriving sexp]
type pattern_info =
  | Operand(UHPat.operand, option(UHTyp.t))
  | CommaOperator(list(UHPat.t), option(UHTyp.t))
  | BinOperator(UHPat.operator, UHPat.t, UHPat.t, option(UHTyp.t));

[@deriving sexp]
type type_info =
  | Operand(UHTyp.operand)
  | CommaOperator(list(UHTyp.t))
  | BinOperator(UHTyp.operator, UHTyp.t, UHTyp.t);

[@deriving sexp]
type explanation_info =
  | EmptyLine
  | CommentLine
  | LetLine(pattern_info, UHExp.t, UHExp.t)
  | ExpBaseOperand(UHExp.operand)
  | Lambda(pattern_info, UHExp.t)
  | Rule(int, UHExp.t, pattern_info, UHExp.t)
  | ExpCommaOperator(list(UHExp.opseq))
  | ExpBinOperator(UHExp.operator, UHExp.opseq, UHExp.opseq)
  | Pattern(pattern_info)
  | Typ(type_info);

let explanation_paths: ZExp.t => list(CursorPath.steps);

let mk_explanation_info: CursorInfo.cursor_term => explanation_info;
