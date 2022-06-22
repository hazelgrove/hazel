let mk:
  (
    ~settings: DHSettings.t,
    ~parenthesize: bool=?,
    ~enforce_inline: bool,
    ~selected_instance: option(HoleInstance.t),
    DHExp.t
  ) =>
  DHDoc.t;
