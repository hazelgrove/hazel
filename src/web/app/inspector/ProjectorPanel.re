open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Util;
open WebUtil;

/* This defines the projector selection menu/toggle at the bottom right */

let knob: Node.t =
  div(
    ~attrs=[clss(["toggle-knob"])],
    [create("img", ~attrs=[Attr.src("img/noun-fold-1593402.svg")], [])],
  );

let toggle_view =
    (
      unproject: option(ContextualAction.t),
      applicable_projectors: list(ContextualAction.t),
    )
    : Node.t =>
  switch (unproject, applicable_projectors) {
  | (None, []) =>
    div(~attrs=[clss(["toggle-switch", "inactive"])], [knob])
  | (None, [first, ..._]) =>
    div(
      ~attrs=[
        clss(["toggle-switch"]),
        Attr.on_mousedown(_ =>
          first.update_action |> Option.value(~default=Ui_effect.Ignore)
        ),
      ],
      [knob],
    )
  | (Some(unproject), _) =>
    div(
      ~attrs=[
        Attr.tabindex(-1),
        clss(["toggle-switch", "active"]),
        Attr.on_mousedown(_ =>
          unproject.update_action |> Option.value(~default=Ui_effect.Ignore)
        ),
      ],
      [knob],
    )
  };

let option_view = (shortcut: ContextualAction.t): Node.t =>
  option(
    ~attrs=[
      Attr.title(
        switch (shortcut.hotkey) {
        | None => "No shortcut"
        | Some(hotkey) => hotkey
        },
      ),
    ],
    [text(shortcut.label)],
  );

let effect_of = (shortcut: ContextualAction.t) => (
  shortcut.label,
  shortcut.update_action |> Option.value(~default=Effect.Ignore),
);

let get_effect = (name: string, actions: list(ContextualAction.t)) =>
  List.assoc_opt(name, List.map(effect_of, actions));

/* A selection input for contetually applicable projectors */
let select_view = (actions: list(ContextualAction.t)) =>
  select(
    ~attrs=[
      Attr.id("projector-select"),
      Attr.title("Select a projector"),
      Attr.on_change((_, name) => {
        switch (get_effect(name, actions)) {
        | None => Ui_effect.Ignore
        | Some(effect) => effect
        }
      }),
    ],
    List.map(option_view, actions),
  );

let view = (cursor: Cursor.t) => {
  let applicable_projectors =
    List.filter(
      (p: ContextualAction.t) =>
        //TODO(andrew): Separate category for things here, combining seems fraught
        p.section == Some("Projection") && p.label != "Unproject",
      cursor.contextual_actions,
    );
  let unproject =
    List.find_opt(
      (p: ContextualAction.t) => p.label == "Unproject",
      cursor.contextual_actions,
    );
  div(
    ~attrs=[Attr.id("projectors")],
    [select_view(applicable_projectors)]
    @ [toggle_view(unproject, applicable_projectors)],
  );
};
