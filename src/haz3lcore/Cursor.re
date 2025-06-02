type shortcut = {
  update_action: option(Ui_effect.t(unit)),
  hotkey: option(string),
  label: string,
  mdIcon: option(string),
  section: option(string),
};

type t = {
  info: option(Info.t),
  contextual_actions: list(shortcut),
};

let empty = {
  info: None,
  contextual_actions: [],
};
