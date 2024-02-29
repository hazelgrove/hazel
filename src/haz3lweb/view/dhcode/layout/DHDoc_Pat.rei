open Haz3lcore;

let precedence: TermBase.UPat.t => int;

let mk:
  (
    ~infomap: Statics.Map.t,
    ~parenthesize: bool=?,
    ~enforce_inline: bool,
    TermBase.UPat.t
  ) =>
  DHDoc.t;
