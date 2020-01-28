open ViewUtil;

type doc = Doc.t(TermTag.t);

let mk =
    (~caret: option(Side.t)=?, ~path: delim_path, delim_text: string): doc =>
  Doc.Text(delim_text) |> Doc.tag(TermTag.mk_Delim(~caret?, ~path, ()));

let empty_hole_doc =
    (~caret: option(Side.t)=?, ~steps: CursorPath.steps, hole_lbl: string)
    : doc =>
  Doc.Text(hole_lbl)
  |> Doc.tag(TermTag.HoleLabel({len: hole_lbl |> StringUtil.utf8_length}))
  |> Doc.tag(TermTag.mk_Delim(~caret?, ~path=(steps, 0), ()));

let open_List = (~caret=?, steps) => mk(~caret?, ~path=(steps, 0), "[");
let close_List = (~caret=?, steps) => mk(~caret?, ~path=(steps, 1), "]");

let open_Parenthesized = (~caret=?, steps) =>
  mk(~caret?, ~path=(steps, 0), "(");
let close_Parenthesized = (~caret=?, steps) =>
  mk(~caret?, ~path=(steps, 1), ")");

let open_Inj = (~caret=?, steps, inj_side: InjSide.t) =>
  mk(
    ~caret?,
    ~path=(steps, 0),
    "inj[" ++ InjSide.to_string(inj_side) ++ "](",
  );
let close_Inj = (~caret=?, steps) => mk(~caret?, ~path=(steps, 1), ")");

let sym_Lam = (~caret=?, steps) =>
  mk(~caret?, ~path=(steps, 0), LangUtil.lamSym);
let colon_Lam = (~caret=?, steps) => mk(~caret?, ~path=(steps, 1), ":");
let open_Lam = (~caret=?, steps) => mk(~caret?, ~path=(steps, 2), ".{");
let close_Lam = (~caret=?, steps) => mk(~caret?, ~path=(steps, 3), "}");

let open_Case = (~caret=?, steps) => mk(~caret?, ~path=(steps, 0), "case");
let close_Case = (~caret=?, steps) => mk(~caret?, ~path=(steps, 1), "end");
let close_Case_ann = (~caret=?, steps) =>
  mk(~caret?, ~path=(steps, 1), "end :");

let bar_Rule = (~caret=?, steps) => mk(~caret?, ~path=(steps, 0), "|");
let arrow_Rule = (~caret=?, steps) =>
  mk(~caret?, ~path=(steps, 1), LangUtil.caseArrowSym);

let let_LetLine = (~caret=?, steps) => mk(~caret?, ~path=(steps, 0), "let");
let colon_LetLine = (~caret=?, steps) => mk(~caret?, ~path=(steps, 1), ":");
let eq_LetLine = (~caret=?, steps) => mk(~caret?, ~path=(steps, 2), "=");
let in_LetLine = (~caret=?, steps) => mk(~caret?, ~path=(steps, 3), "in");

let open_CommentLine = (~caret=?, steps) =>
  mk(~caret?, ~path=(steps, 0), "#");
let open_SubCommentLine = (~caret=?, steps) =>
  mk(~caret?, ~path=(steps, 0), "*");
