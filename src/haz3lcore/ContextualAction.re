type t = {
  update_action: option(Ui_effect.t(unit)),
  hotkey: option(string),
  label: string,
  mdIcon: option(string),
  section: option(string),
};

let mk = (~hotkey=?, ~mdIcon=?, ~section=?, label, update_action): t => {
  {
    update_action: Some(update_action),
    hotkey,
    label,
    mdIcon,
    section,
  };
};
