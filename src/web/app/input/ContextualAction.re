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
  binding: ShortcutAction.binding,
  label: string,
  mdIcon: option(string),
  section: option(string),
};

let of_shortcut = (~action=?, id: ShortcutAction.t): t => {
  id,
  update_action: action,
  binding: ShortcutAction.default_binding(id),
  label: ShortcutAction.label(id),
  mdIcon: Some(ShortcutAction.md_icon(id)),
  section: ShortcutAction.section_string(id),
};

/* The Shortcuts config slide wins over the ShortcutAction registry defaults.
   An action the config leaves Unbound CLEARS the default rather than
   falling back to it.

   Overrides are applied on the way in rather than mutated afterwards: the
   actions are rebuilt from Page.View.view on every cursor change, so any
   post-hoc mutation would be overwritten on the next keystroke. */
let with_overrides =
    (~overrides: list((string, ShortcutAction.binding)), action: t): t =>
  switch (List.assoc_opt(action.label, overrides)) {
  | Some(binding) => {
      ...action,
      binding,
    }
  | None => action
  };

/* What a key press triggers: the first action bound to the pressed chord
   that has something to do. */
let of_key = (actions: list(t), key: Util.Key.t): option(Effect.t(unit)) =>
  Option.bind(ShortcutAction.S.binding_of_key(key), pressed =>
    List.find_map(
      a =>
        ShortcutAction.S.same_chord(a.binding, pressed)
          ? a.update_action : None,
      actions,
    )
  );
