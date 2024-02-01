open Virtual_dom.Vdom;

let btn = (~inject as _, caption, action) => {
  Node.(
    button(
      ~attr=
        Attr.many([
          Attr.on_click(_ => {
            DebugAction.perform(action);
            Ui_effect.Ignore;
          }),
        ]),
      [text(caption)],
    )
  );
};

let view = (~inject) => {
  Node.(
    div([
      btn(~inject, "turn off dynamics", TurnOffDynamics),
      btn(~inject, "clear local storage (LOSE ALL DATA!)", ClearStore),
      /* The following exist to satisfy various unchecked getElement calls */
      div(~attr=Attr.id("caret"), []),
      div(~attr=Attr.id("clipboard-shim"), []),
      div(~attr=Attr.id("font-specimen"), []),
      div(~attr=Attr.id("main"), []),
    ])
  );
};
