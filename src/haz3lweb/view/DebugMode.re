open Virtual_dom.Vdom;

let btn = (~inject, caption, action) => {
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
    ])
  );
};
