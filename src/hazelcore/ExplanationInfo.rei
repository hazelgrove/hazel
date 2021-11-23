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
  | Block(UHExp.block, int, UHExp.line)
  | LetLine(pattern_info, UHExp.t, int, UHExp.t)
  | ExpBaseOperand(UHExp.operand)
  | Lambda(pattern_info, UHExp.t)
  | Rule(int, UHExp.t, pattern_info, UHExp.t)
  | ExpCommaOperator(list(UHExp.opseq))
  | ExpBinOperator(UHExp.operator, UHExp.opseq, UHExp.opseq)
  | Pattern(pattern_info)
  | Typ(type_info);

let mk_explanation_info: CursorInfo.cursor_term => explanation_info;
