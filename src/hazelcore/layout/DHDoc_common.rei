[@deriving sexp]
type t = Pretty.Doc.t(DHAnnot.t);

type formattable_child = (~enforce_inline: bool) => t;

let precedence_const: int;
let precedence_Ap: int;
let precedence_Times: int;
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
let precedence_Label: int;

let pad_child:
  (
    ~inline_padding: (Pretty.Doc.t(DHAnnot.t), Pretty.Doc.t(DHAnnot.t))=?,
    ~enforce_inline: bool,
    formattable_child
  ) =>
  t;

module Delim: {
  let mk: string => t;

  let empty_hole: HoleInstance.t => t;

  let list_nil: t;
  let triv: t;
  let wild: t;

  let open_Parenthesized: t;
  let close_Parenthesized: t;

  let sym_Lam: t;
  let colon_Lam: t;
  let open_Lam: t;
  let close_Lam: t;

  let fix_FixF: t;
  let colon_FixF: t;
  let open_FixF: t;
  let close_FixF: t;

  let open_Inj: InjSide.t => t;
  let close_Inj: t;

  let open_Case: t;
  let close_Case: t;

  let bar_Rule: t;
  let arrow_Rule: t;

  let open_Cast: t;
  let arrow_Cast: t;
  let close_Cast: t;

  let open_FailedCast: Pretty.Doc.t(DHAnnot.t);
  let arrow_FailedCast: Pretty.Doc.t(DHAnnot.t);
  let close_FailedCast: Pretty.Doc.t(DHAnnot.t);
};

let mk_EmptyHole:
  (~selected: bool=?, (MetaVar.t, MetaVarInst.t)) => Pretty.Doc.t(DHAnnot.t);

let mk_Keyword:
  (MetaVar.t, MetaVarInst.t, ExpandingKeyword.t) => Pretty.Doc.t(DHAnnot.t);

let mk_InvalidText:
  (string, (MetaVar.t, MetaVarInst.t)) => Pretty.Doc.t(DHAnnot.t);

let mk_IntLit: int => Pretty.Doc.t('a);

let mk_FloatLit: float => Pretty.Doc.t('a);

let mk_BoolLit: bool => Pretty.Doc.t('a);

let mk_Inj: (InjSide.t, Pretty.Doc.t(DHAnnot.t)) => Pretty.Doc.t(DHAnnot.t);

let mk_Cons: (Pretty.Doc.t('a), Pretty.Doc.t('a)) => Pretty.Doc.t('a);

let mk_Pair: (Pretty.Doc.t('a), Pretty.Doc.t('a)) => Pretty.Doc.t('a);

let mk_Ap: (Pretty.Doc.t('a), Pretty.Doc.t('a)) => Pretty.Doc.t('a);

let mk_Label: Label.t => Pretty.Doc.t('a);

let mk_Label_Elt: (Label.t, Pretty.Doc.t('a)) => Pretty.Doc.t('a);
