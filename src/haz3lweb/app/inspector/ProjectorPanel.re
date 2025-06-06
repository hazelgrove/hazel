open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Util;
open Web;

/* This defines the projector selection menu/toggle at the bottom right */

//TODO(andrew): cleanup
let knob =
  div(
    ~attrs=[clss(["toggle-knob"])],
    [create("img", ~attrs=[Attr.src("img/noun-fold-1593402.svg")], [])],
  );

let toggle_view =
    (
      unproject: option(ContextualAction.t),
      applicable_projectors: list(ContextualAction.t),
    ) =>
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
        clss(["toggle-switch", "active"]),
        Attr.on_mousedown(_ =>
          unproject.update_action |> Option.value(~default=Ui_effect.Ignore)
        ),
      ],
      [knob],
    )
  };

let keyboard_shortcut_of = (kind: ProjectorCore.Kind.t): string =>
  //TODO(andrew): reinstate. this should be a hover; get from ctxaction?
  switch (kind) {
  // | Fold => "Option-f"
  // | Probe => "Option-v"
  | Info => "Option-t"
  | _ => "Option-l"
  };

/* A selection input for contetually applicable projectors */
let select_view =
    (
      current_projector: option(string),
      applicable_projectors: list(ContextualAction.t),
    ) => {
  switch (current_projector, applicable_projectors) {
  | (None, []) => select(~attrs=[Attr.id("projector-select")], [])
  | (_x, y) =>
    // TODO(andrew): this looks funky
    // let current_option =
    //   x
    //   |> Option.map(name =>
    //        option(~attrs=[Attr.title(name)], [text(name)])
    //      );
    //let current_effect = x |> Option.map(name => (name, Effect.Ignore));
    let applicable_options =
      y
      |> List.map((shortcut: ContextualAction.t) =>
           option(
             ~attrs=[
               Attr.title(
                 shortcut.label
                 ++ (
                   switch (shortcut.hotkey) {
                   | None => ""
                   | Some(hotkey) => " (" ++ hotkey ++ ")"
                   }
                 ),
               ),
             ],
             [text(shortcut.label)],
           )
         );
    let applicable_effects =
      y
      |> List.map((shortcut: ContextualAction.t) =>
           (
             shortcut.label,
             shortcut.update_action |> Option.value(~default=Effect.Ignore),
           )
         );
    let options = /*Option.to_list(current_option) @ */ applicable_options;
    let effects = /*Option.to_list(current_effect) @ */ applicable_effects;
    select(
      ~attrs=[
        Attr.id("projector-select"),
        Attr.title("select projector"),
        Attr.on_change((_, name) => {
          switch (List.assoc_opt(name, effects)) {
          | None => Ui_effect.Ignore
          | Some(effect) => effect
          }
        }),
      ],
      options,
    );
  };
};

let view = (cursor: Cursor.t) => {
  let applicable_projectors =
    List.filter(
      (p: ContextualAction.t) =>
        //TODO(andrew): separate category for things here, combining seems fraught
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
    [select_view(cursor.current_projector, applicable_projectors)]
    @ [toggle_view(unproject, applicable_projectors)],
  );
};
