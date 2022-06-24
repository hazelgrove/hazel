let mk:
  (
    ~settings: DHSettings.t,
    ~parenthesize: bool=?,
    ~enforce_inline: bool,
    ~selected_hole_closure: option(HoleClosure.t),
    DHExp.t
  ) =>
  DHDoc.t;
