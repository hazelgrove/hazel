open Virtual_dom.Vdom;

/* A palette entry. Everything static about an action — its label, section,
   icon and default binding — comes from ShortcutAction; the call site
   supplies only the effect, which is the one part that needs local context.

   There is deliberately no `mk` taking a raw label: that is what let the
   config's action names drift from the real ones. Naming a ShortcutAction
   variant is the only way to build one of these. */
type t = {
  id: ShortcutAction.t,
  update_action: option(Effect.t(unit)),
  hotkey: option(string),
  label: string,
  mdIcon: option(string),
  section: option(string),
};

let of_shortcut = (~action=?, id: ShortcutAction.t): t => {
  id,
  update_action: action,
  hotkey: ShortcutAction.default_hotkey(id),
  label: ShortcutAction.label(id),
  mdIcon: Some(ShortcutAction.md_icon(id)),
  section: ShortcutAction.section_string(id),
};
