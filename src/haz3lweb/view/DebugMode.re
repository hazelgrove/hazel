open Virtual_dom.Vdom;

let btn = (~inject, caption, action) => {
  Node.(
    button(
      ~attr=Attr.many([Attr.on_click(_ => inject(action))]),
      [text(caption)],
    )
  );
};

let view = (~inject) => {
  Node.(
    div([
      btn(
        ~inject,
        "turn off dynamics",
        UpdateAction.DebugAction(TurnOffDynamics),
      ),
      btn(
        ~inject,
        "clear local storage (LOSE ALL DATA!)",
        UpdateAction.DebugAction(ClearLocalStorage),
      ),
    ])
  );
};
