open Virtual_dom.Vdom;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | TurnOffDynamics
  | ClearStore;

let perform = (action: action): unit => {
  switch (action) {
  | TurnOffDynamics =>
    let settings = Store.Settings.load();
    Store.Settings.save({
      ...settings,
      core: {
        ...settings.core,
        dynamics: false,
      },
    });
  | ClearStore => JsUtil.clear_localstore()
  };
  Js_of_ocaml.Dom_html.window##.location##replace(
    Js_of_ocaml.Js.string("#"),
  );
  Js_of_ocaml.Dom_html.window##.location##reload;
};

let btn = (caption, action) => {
  Node.(
    button(
      ~attrs=[
        Attr.on_click(_ => {
          perform(action);
          Ui_effect.Ignore;
        }),
      ],
      [text(caption)],
    )
  );
};

let view = {
  Node.(
    div([
      btn("turn off dynamics", TurnOffDynamics),
      btn("clear local storage (LOSE ALL DATA!)", ClearStore),
    ])
  );
};

let go = () =>
  Bonsai_web.Start.start(
    Bonsai.Computation.return(view),
    ~bind_to_element_with_id="container",
  );
