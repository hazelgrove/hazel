open Haz3lcore;

let precedence: Pat.t => int;

let mk:
  (
    ~infomap: Statics.Map.t,
    ~parenthesize: bool=?,
    ~enforce_inline: bool,
    Pat.t
  ) =>
  DHDoc.t;
