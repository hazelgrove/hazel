let mk:
  (
    ~settings: Settings.Evaluation.t,
    ~parenthesize: bool=?,
    ~enforce_inline: bool,
    ~selected_instance: option(TaggedNodeInstance.t),
    DHExp.t
  ) =>
  DHDoc.t;
