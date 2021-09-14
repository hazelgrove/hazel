type t = Pretty.Doc.t(HTypAnnot.t);

let mk:
  (
    ~parenthesize: bool=?,
    ~enforce_inline: bool,
    ~selected_tag_hole: option(MetaVar.t),
    HTyp.t
  ) =>
  t;
