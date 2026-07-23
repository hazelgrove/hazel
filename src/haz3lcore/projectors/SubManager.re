open Language;
open IdTagged.FreshGrammar;
open MvuShape;

// SubManager: Manages subscriptions for Hazel apps
//
// Sub is a sum type defined in BuiltinsADT.re (Elm-mode handler types;
// legacy mode prepends the current Html model to the handler args):
//   | SubNone
//   | SubBatch(List(Sub))
//   | OnResize((Int, Int) -> Msg)
//   | OnVisibilityChange(Bool -> Msg)
//   | OnDocumentKeyDown(KeyEvent -> Msg)
//   | OnDocumentKeyUp(KeyEvent -> Msg)
//   | Every(Float, Float -> Msg)
//   | AnimationFrame(Float -> Msg)

type context = {
  model: DHExp.t,
  inject: DHExp.t => Ui_effect.t(unit),
  update_fn: option(DHExp.t),
};

// Track active subscriptions for cleanup
type sub_handle =
  | IntervalHandle(Js_of_ocaml.Dom_html.interval_id)
  | EventHandle(unit => unit); // cleanup function

type active_subs = list(sub_handle);

// Apply a handler with the current model and additional args
// With error boundary - logs errors instead of crashing
let apply_handler = (ctx: context, handler: DHExp.t, args: list(DHExp.t)) => {
  switch (ctx.update_fn) {
  | Some(_) =>
    // Elm mode: handler takes just event data, produces msg
    let result =
      switch (args) {
      | [] => Ok(handler) // handler IS the msg (no event data)
      | [single] => safe_evaluate(Exp.ap(Forward, handler, single))
      | _ => safe_evaluate(Exp.ap(Forward, handler, Exp.tuple(args)))
      };
    switch (result) {
    | Ok(msg) => Bonsai.Effect.Expert.handle(ctx.inject(msg))
    | Error(err) =>
      Js_of_ocaml.Firebug.console##error(
        Js_of_ocaml.Js.string("Subscription handler error: " ++ err),
      )
    };
  | None =>
    // Legacy: handler takes (model, ...args) -> model
    let arg_exp =
      switch (args) {
      | [] => ctx.model
      | _ => Exp.tuple([ctx.model, ...args])
      };
    switch (safe_evaluate(Exp.ap(Forward, handler, arg_exp))) {
    | Ok(new_model) => Bonsai.Effect.Expert.handle(ctx.inject(new_model))
    | Error(msg) =>
      Js_of_ocaml.Firebug.console##error(
        Js_of_ocaml.Js.string("Subscription handler error: " ++ msg),
      )
    };
  };
};

// Build a KeyEvent value (labeled tuple, see BuiltinsADT.Event.key)
// from a JS keyboard event
let key_event_of_js = evt => {
  open Js_of_ocaml;
  let get_key = evt =>
    Js.to_string(Js.Optdef.get(evt##.key, () => Js.string("")));
  let get_code = evt =>
    Js.to_string(Js.Optdef.get(evt##.code, () => Js.string("")));
  Exp.tuple([
    field("key", Exp.string(get_key(evt))),
    field("code", Exp.string(get_code(evt))),
    field("ctrl", Exp.bool(Js.to_bool(evt##.ctrlKey))),
    field("shift", Exp.bool(Js.to_bool(evt##.shiftKey))),
    field("alt", Exp.bool(Js.to_bool(evt##.altKey))),
    field("meta", Exp.bool(Js.to_bool(evt##.metaKey))),
  ]);
};

// Subscribe to a single subscription, returning handles for cleanup
let rec subscribe =
        (ctx: context, sub: DHExp.t, get_model: unit => DHExp.t)
        : list(sub_handle) => {
  Js_of_ocaml.(
    switch (of_constructor_raw(sub)) {
    | None =>
      prerr_endline("SubManager: not a Sub constructor");
      [];

    | Some(("SubNone", _)) => []

    | Some(("SubBatch", body)) =>
      switch (of_list(body)) {
      | Some(subs) =>
        List.concat(List.map(subscribe(ctx, _, get_model), subs))
      | None => []
      }

    | Some(("OnResize", handler)) =>
      let listener =
        Dom.handler(_evt => {
          let w = Dom_html.window##.innerWidth;
          let h = Dom_html.window##.innerHeight;
          let current_model = get_model();
          let ctx' = {
            ...ctx,
            model: current_model,
          };
          apply_handler(ctx', handler, [Exp.int(w), Exp.int(h)]);
          Js._true;
        });
      let listener_id =
        Dom.addEventListener(
          Dom_html.window,
          Dom.Event.make("resize"),
          listener,
          Js._false,
        );
      // Return cleanup function
      [EventHandle(() => Dom.removeEventListener(listener_id))];

    | Some(("OnVisibilityChange", handler)) =>
      let listener =
        Dom.handler(_evt => {
          let visible = Dom_html.document##.hidden |> Js.to_bool |> (!);
          let current_model = get_model();
          let ctx' = {
            ...ctx,
            model: current_model,
          };
          apply_handler(ctx', handler, [Exp.bool(visible)]);
          Js._true;
        });
      let listener_id =
        Dom.addEventListener(
          Dom_html.document,
          Dom.Event.make("visibilitychange"),
          listener,
          Js._false,
        );
      [EventHandle(() => Dom.removeEventListener(listener_id))];

    | Some(("OnDocumentKeyDown", handler)) =>
      let listener =
        Dom.handler(evt => {
          let key_event = key_event_of_js(evt);
          let current_model = get_model();
          let ctx' = {
            ...ctx,
            model: current_model,
          };
          apply_handler(ctx', handler, [key_event]);
          Js._true;
        });
      // Use capture phase (Js._true) so we fire before Hazel's editor handlers
      let listener_id =
        Dom.addEventListener(
          Dom_html.document,
          Dom.Event.make("keydown"),
          listener,
          Js._true,
        );
      [EventHandle(() => Dom.removeEventListener(listener_id))];

    | Some(("OnDocumentKeyUp", handler)) =>
      let listener =
        Dom.handler(evt => {
          let key_event = key_event_of_js(evt);
          let current_model = get_model();
          let ctx' = {
            ...ctx,
            model: current_model,
          };
          apply_handler(ctx', handler, [key_event]);
          Js._true;
        });
      // Use capture phase (Js._true) so we fire before Hazel's editor handlers
      let listener_id =
        Dom.addEventListener(
          Dom_html.document,
          Dom.Event.make("keyup"),
          listener,
          Js._true,
        );
      [EventHandle(() => Dom.removeEventListener(listener_id))];

    | Some(("Every", body)) =>
      switch (of_tuple(body)) {
      | Some([ms_exp, handler]) =>
        switch (of_float(ms_exp)) {
        | Some(ms) =>
          let interval_id =
            Dom_html.window##setInterval(
              Js.wrap_callback(() => {
                let perf = Js.Unsafe.coerce(Dom_html.window)##.performance;
                let timestamp = Js.to_float(perf##now());
                let current_model = get_model();
                let ctx' = {
                  ...ctx,
                  model: current_model,
                };
                apply_handler(ctx', handler, [Exp.float(timestamp)]);
              }),
              ms,
            );
          [IntervalHandle(interval_id)];
        | None => []
        }
      | _ => []
      }

    | Some(("AnimationFrame", handler)) =>
      // Recursive animation frame loop with cleanup via running flag
      let running = ref(true);
      let rec request_frame = () =>
        if (running^) {
          let _ =
            Dom_html.window##requestAnimationFrame(
              Js.wrap_callback(timestamp =>
                if (running^) {
                  let ts = Js.to_float(timestamp);
                  let current_model = get_model();
                  let ctx' = {
                    ...ctx,
                    model: current_model,
                  };
                  apply_handler(ctx', handler, [Exp.float(ts)]);
                  request_frame();
                }
              ),
            );
          ();
        };
      request_frame();
      [EventHandle(() => running := false)];

    | Some((name, _)) =>
      prerr_endline("SubManager: unknown subscription: " ++ name);
      [];
    }
  );
};

// Cleanup all active subscriptions
let cleanup = (handles: active_subs): unit => {
  Js_of_ocaml.(
    List.iter(
      handle =>
        switch (handle) {
        | IntervalHandle(id) => Dom_html.window##clearInterval(id)
        | EventHandle(cleanup_fn) => cleanup_fn()
        },
      handles,
    )
  );
};
