type t = Pretty.Doc.t(HTypAnnot.t);

type formattable_child = (~enforce_inline: bool) => t;

let pad_child:
  (
    ~inline_padding: (Pretty.Doc.t(HTypAnnot.t), Pretty.Doc.t(HTypAnnot.t))=?,
    ~enforce_inline: bool,
    formattable_child
  ) =>
  t;

let mk_delim: string => Pretty.Doc.t(HTypAnnot.t);

let mk: (~parenthesize: bool=?, ~enforce_inline: bool, HTyp.t) => t;
