let mk:
  (
    ~settings: Settings.Evaluation.t,
    ~parenthesize: bool=?,
    ~check_step: bool=?,
    ~enforce_inline: bool,
    ~selected_instance: option(HoleInstance.t),
    DHExp.t
  ) =>
  DHDoc.t;
