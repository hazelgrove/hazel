open Virtual_dom.Vdom;

/* A palette entry. Everything static about an action — its label, section,
   icon and default binding — comes from ShortcutAction; the call site
   supplies only the effect, which is the one part that needs local context.

   There is deliberately no `mk` taking a raw label: that is what let the
   config's action names drift from the real ones. Naming a ShortcutAction
   variant is the only way to build a rebindable entry; of_dynamic below is
   the exception for labels computed from the program. */
type t = {
  /* None: a dynamic entry, which no Shortcuts-slide field names. */
  id: option(ShortcutAction.t),
  update_action: option(Effect.t(unit)),
  hotkey: option(string),
  label: string,
  mdIcon: option(string),
  section: option(string),
};

let of_shortcut = (~action=?, id: ShortcutAction.t): t => {
  id: Some(id),
  update_action: action,
  hotkey: ShortcutAction.default_hotkey(id),
  label: ShortcutAction.label(id),
  mdIcon: Some(ShortcutAction.md_icon(id)),
  section: ShortcutAction.section_string(id),
};

/* An entry whose label depends on the program, e.g. the refactorings
   offered at the caret ("Rename x to y"). It has no registry variant, so it
   carries no hotkey and the Shortcuts slide never lists it: there is no
   config entry for it to drift from. */
let of_dynamic =
    (
      ~action: Effect.t(unit),
      ~section: ShortcutAction.section,
      ~mdIcon: string,
      label: string,
    )
    : t => {
  id: None,
  update_action: Some(action),
  hotkey: None,
  label,
  mdIcon: Some(mdIcon),
  section: ShortcutAction.section_group(section),
};
