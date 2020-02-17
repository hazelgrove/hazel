type doc = Doc.t(TermAnnot.t);

let mk =
    (~caret: option(Side.t)=?, ~index: DelimIndex.t, delim_text: string): doc =>
  Doc.Text(delim_text) |> Doc.annot(TermAnnot.mk_Delim(~caret?, ~index, ()));

let empty_hole_doc = (~caret: option(Side.t)=?, hole_lbl: string): doc =>
  Doc.Text(hole_lbl)
  |> Doc.annot(
       TermAnnot.HoleLabel({len: hole_lbl |> StringUtil.utf8_length}),
     )
  |> Doc.annot(TermAnnot.mk_Delim(~caret?, ~index=0, ()));

let open_List = (~caret=?, ()) => mk(~caret?, ~index=0, "[");
let close_List = (~caret=?, ()) => mk(~caret?, ~index=1, "]");

let open_Parenthesized = (~caret=?, ()) => mk(~caret?, ~index=0, "(");
let close_Parenthesized = (~caret=?, ()) => mk(~caret?, ~index=1, ")");

let open_Inj = (~caret=?, (), inj_side: InjSide.t) =>
  mk(~caret?, ~index=0, "inj[" ++ InjSide.to_string(inj_side) ++ "](");
let close_Inj = (~caret=?, ()) => mk(~caret?, ~index=1, ")");

let sym_Lam = (~caret=?, ()) =>
  mk(~caret?, ~index=0, UnicodeConstants.lamSym);
let colon_Lam = (~caret=?, ()) => mk(~caret?, ~index=1, ":");
let open_Lam = (~caret=?, ()) => mk(~caret?, ~index=2, ".{");
let close_Lam = (~caret=?, ()) => mk(~caret?, ~index=3, "}");

let open_Case = (~caret=?, ()) => mk(~caret?, ~index=0, "case");
let close_Case = (~caret=?, ()) => mk(~caret?, ~index=1, "end");
let close_Case_ann = (~caret=?, ()) => mk(~caret?, ~index=1, "end :");

let bar_Rule = (~caret=?, ()) => mk(~caret?, ~index=0, "|");
let arrow_Rule = (~caret=?, ()) =>
  mk(~caret?, ~index=1, UnicodeConstants.caseArrowSym);

let let_LetLine = (~caret=?, ()) => mk(~caret?, ~index=0, "let");
let colon_LetLine = (~caret=?, ()) => mk(~caret?, ~index=1, ":");
let eq_LetLine = (~caret=?, ()) => mk(~caret?, ~index=2, "=");
let in_LetLine = (~caret=?, ()) => mk(~caret?, ~index=3, "in");
