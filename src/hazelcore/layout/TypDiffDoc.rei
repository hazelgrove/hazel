type t = Pretty.Doc.t(TypDiffAnnot.t);

type formattable_child = (~enforce_inline: bool) => t;

let pad_child:
  (
    ~inline_padding: (
                       Pretty.Doc.t(TypDiffAnnot.t),
                       Pretty.Doc.t(TypDiffAnnot.t),
                     )
                       =?,
    ~enforce_inline: bool,
    formattable_child
  ) =>
  t;

let mk_delim: string => Pretty.Doc.t(TypDiffAnnot.t);

let mk: (~parenthesize: bool=?, ~enforce_inline: bool, TypDiff.t) => t;
