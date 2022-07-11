let precedence: DHPat.t => int;

let mk:
  (
    ~parenthesize: bool=?,
    ~enforce_inline: bool,
    ~selected_tag_hole: option(MetaVar.t),
    DHPat.t
  ) =>
  DHDoc.t;
