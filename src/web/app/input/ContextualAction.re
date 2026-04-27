open Virtual_dom.Vdom;

type t = {
  update_action: option(Effect.t(unit)),
  hotkey: option(string),
  label: string,
  mdIcon: option(string),
  section: option(string),
};

let mk = (~hotkey=?, ~mdIcon=?, ~section=?, ~action=?, label): t => {
  update_action: action,
  hotkey,
  label,
  mdIcon,
  section,
};
