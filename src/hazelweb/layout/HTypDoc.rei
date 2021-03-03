type t = Pretty.Doc.t(HTypAnnot.t);

let mk:
  (
    ~parenthesize: bool=?,
    ~strategy_guide: bool=?,
    ~enforce_inline: bool,
    HTyp.t
  ) =>
  t;
