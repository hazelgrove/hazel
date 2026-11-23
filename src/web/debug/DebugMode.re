open Virtual_dom.Vdom;

[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | TurnOffDynamics
  | ClearStore;

let perform = (action: action): unit => {
  switch (action) {
  | TurnOffDynamics =>
    let settings = Settings.Store.load();
    Settings.Store.save({
      ...settings,
      core: {
        ...settings.core,
        dynamics: false,
      },
    });
  | ClearStore => HazelDB.clear_all()
  };
  Js_of_ocaml.Dom_html.window##.location##replace(
    Js_of_ocaml.Js.string("#"),
  );
  Js_of_ocaml.Dom_html.window##.location##reload;
};

let btn = (btn_caption, action) => {
  Node.(
    button(
      ~attrs=[
        Attr.on_click_js_of_ocaml(_ => {
          perform(action);
          Ui_effect.Ignore;
        }),
      ],
      [text(btn_caption)],
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
    Bonsai_web_proc.Bonsai.const(view),
    ~bind_to_element_with_id="container",
  );
