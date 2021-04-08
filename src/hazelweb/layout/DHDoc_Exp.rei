let mk:
  (
    ~settings: Settings.Evaluation.t,
    ~parenthesize: bool=?,
    ~enforce_inline: bool,
    ~selected_instance: option(HoleInstance.t),
    AssertMap.t,
    DHExp.t
  ) =>
  DHDoc.t;
