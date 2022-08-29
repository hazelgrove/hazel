open Js_of_ocaml;
open Incr_dom;
open Web;

let observe_font_specimen = (id, update) =>
  ResizeObserver.observe(
    ~node=JsUtil.get_elem_by_id(id),
    ~f=
      (entries, _) => {
        let specimen = Js.to_array(entries)[0];
        let rect = specimen##.contentRect;
        update(
          Web.FontMetrics.{
            row_height: rect##.bottom -. rect##.top,
            col_width: rect##.right -. rect##.left,
          },
        );
      },
    (),
  );

let restart_caret_animation = () =>
  // necessary to trigger reflow
  // <https://css-tricks.com/restart-css-animation/>
  try({
    let caret_elem = JsUtil.get_elem_by_id("caret");
    caret_elem##.classList##remove(Js.string("blink"));
    let _ = caret_elem##getBoundingClientRect;
    caret_elem##.classList##add(Js.string("blink"));
  }) {
  | _ => ()
  };

let apply = (model, action, state, ~schedule_action): Model.t => {
  restart_caret_animation();
  switch (
    try(
      Update.apply(model, action, state, ~schedule_action)
      |> Log.update(action, model)
    ) {
    | exc => Error(Exception(Printexc.to_string(exc)))
    }
  ) {
  | Ok(model) => model
  | Error(FailedToPerform(err)) =>
    // TODO(andrew): reinstate this history functionality
    print_endline(Update.Failure.show(FailedToPerform(err)));
    //{...model, history: ActionHistory.failure(err, model.history)};
    model;
  | Error(UnrecognizedInput(reason)) =>
    // TODO(andrew): reinstate this history functionality
    print_endline(Update.Failure.show(UnrecognizedInput(reason)));
    model;
  //{...model, history: ActionHistory.just_failed(reason, model.history)};
  | Error(err) =>
    print_endline(Update.Failure.show(err));
    model;
  };
};

let do_many = (evts): Virtual_dom.Vdom.Effect.t(unit) => {
  Virtual_dom.Vdom.Effect.(
    switch (evts) {
    | [] => Many([])
    | evts => Many([Prevent_default, Stop_propagation, ...evts])
    }
  );
};

let update_handler = (~inject, ~model, ~dir: Key.dir, evt) => {
  let key = Key.mk(dir, evt);
  Keyboard.handle_key_event(key, ~model)
  |> Log.keystoke(key)
  |> List.map(inject)
  |> do_many;
};

let handlers = (~inject, ~model: Model.t) =>
  Virtual_dom.Vdom.[
    Attr.on_keypress(_ => Effect.Prevent_default),
    Attr.on_keyup(update_handler(~inject, ~model, ~dir=KeyUp)),
    Attr.on_keydown(update_handler(~inject, ~model, ~dir=KeyDown)),
  ];

module App = {
  module Model = Model;
  module Action = Update;
  module State = State;

  let on_startup = (~schedule_action, _) => {
    let _ =
      observe_font_specimen("font-specimen", fm =>
        schedule_action(Web.Update.SetFontMetrics(fm))
      );
    // let _ =
    //   observe_font_specimen("logo-font-specimen", fm =>
    //     schedule_action(Web.Update.SetLogoFontMetrics(fm))
    //   );
    Os.is_mac :=
      Dom_html.window##.navigator##.platform##toUpperCase##indexOf(
        Js.string("MAC"),
      )
      >= 0;
    Async_kernel.Deferred.return();
  };

  let create = (model: Incr.t(Web.Model.t), ~old_model as _, ~inject) => {
    open Incr.Let_syntax;
    let%map model = model;
    Component.create(
      ~apply_action=apply(model),
      model,
      Web.Page.view(~inject, ~handlers, model),
    );
  };
};

let initial_model: Model.t =
  apply(Model.blank, LoadInit, (), ~schedule_action=());

Incr_dom.Start_app.start(
  (module App),
  ~debug=false,
  ~bind_to_element_with_id="container",
  ~initial_model,
);
