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

module App = {
  module Model = {
    type t = unit;
    let cutoff = (_, _) => false;
  };
  module Action = {
    type t = unit;
    let sexp_of_t = _ => Sexplib.Sexp.unit;
  };
  module State = {
    type t = unit;
  };
  let on_startup = (~schedule_action as _, _) =>
    Async_kernel.Deferred.return();
  let create = (_, ~old_model as _, ~inject as _) =>
    Incr_dom.Incr.return()
    |> Incr_dom.Incr.map(~f=_ =>
         Incr_dom.Component.create(
           ~apply_action=(_, _, ~schedule_action as _) => (),
           (),
           view,
         )
       );
};

let go = () =>
  Incr_dom.Start_app.start(
    (module App),
    ~debug=false,
    ~bind_to_element_with_id="container",
    ~initial_model=(),
  );
