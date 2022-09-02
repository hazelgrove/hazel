open Haz3lcore;

let mk:
  (
    ~settings: Settings.Evaluation.t,
    ~parenthesize: bool=?,
    ~enforce_inline: bool,
    ~selected_instance: option(HoleInstance.t),
    DHExp.t
  ) =>
  DHDoc.t;
