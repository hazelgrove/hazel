open Haz3lcore;

type formattable_child = (~enforce_inline: bool) => DHDoc.t;

let precedence_const: int;
let precedence_Ap: int;
let precedence_Times: int;
let precedence_Power: int;
let precedence_Divide: int;
let precedence_Plus: int;
let precedence_Minus: int;
let precedence_Cons: int;
let precedence_Equals: int;
let precedence_LessThan: int;
let precedence_GreaterThan: int;
let precedence_And: int;
let precedence_Or: int;
let precedence_Comma: int;
let precedence_max: int;

let pad_child:
  (
    ~inline_padding: (Pretty.Doc.t(DHAnnot.t), Pretty.Doc.t(DHAnnot.t))=?,
    ~enforce_inline: bool,
    formattable_child
  ) =>
  DHDoc.t;

module Delim: {
  let mk: string => DHDoc.t;

  let empty_hole: HoleInstance.t => DHDoc.t;

  let list_nil: DHDoc.t;
  let triv: DHDoc.t;
  let wild: DHDoc.t;

  let open_Parenthesized: DHDoc.t;
  let close_Parenthesized: DHDoc.t;

  let sym_Fun: DHDoc.t;
  let colon_Fun: DHDoc.t;
  let open_Fun: DHDoc.t;
  let close_Fun: DHDoc.t;

  let fix_FixF: DHDoc.t;
  let colon_FixF: DHDoc.t;
  let open_FixF: DHDoc.t;
  let close_FixF: DHDoc.t;

  let open_Inj: InjSide.t => DHDoc.t;
  let close_Inj: DHDoc.t;

  let open_Case: DHDoc.t;
  let close_Case: DHDoc.t;

  let bar_Rule: DHDoc.t;
  let arrow_Rule: DHDoc.t;

  let open_Cast: DHDoc.t;
  let arrow_Cast: DHDoc.t;
  let close_Cast: DHDoc.t;

  let open_FailedCast: Pretty.Doc.t(DHAnnot.t);
  let arrow_FailedCast: Pretty.Doc.t(DHAnnot.t);
  let close_FailedCast: Pretty.Doc.t(DHAnnot.t);
};

let mk_EmptyHole:
  (~selected: bool=?, HoleInstance.t) => Pretty.Doc.t(DHAnnot.t);

let mk_ExpandingKeyword:
  (HoleInstance.t, ExpandingKeyword.t) => Pretty.Doc.t(DHAnnot.t);

let mk_InvalidText: (string, HoleInstance.t) => Pretty.Doc.t(DHAnnot.t);

let mk_Sequence: (Pretty.Doc.t('a), Pretty.Doc.t('a)) => Pretty.Doc.t('a);

let mk_TestLit: KeywordID.t => Pretty.Doc.t('a);

let mk_IntLit: int => Pretty.Doc.t('a);

let mk_FloatLit: float => Pretty.Doc.t('a);

let mk_BoolLit: bool => Pretty.Doc.t('a);

let mk_TagLit: string => Pretty.Doc.t('a);

let mk_StringLit: string => Pretty.Doc.t('a);

let mk_Inj: (InjSide.t, Pretty.Doc.t(DHAnnot.t)) => Pretty.Doc.t(DHAnnot.t);

let mk_Cons: (Pretty.Doc.t('a), Pretty.Doc.t('a)) => Pretty.Doc.t('a);

let mk_ListLit: list(Pretty.Doc.t('a)) => Pretty.Doc.t('a);

let mk_Tuple: list(Pretty.Doc.t('a)) => Pretty.Doc.t('a);

let mk_Ap: (Pretty.Doc.t('a), Pretty.Doc.t('a)) => Pretty.Doc.t('a);

let mk_Prj: (Pretty.Doc.t(DHAnnot.t), int) => Pretty.Doc.t(DHAnnot.t);
