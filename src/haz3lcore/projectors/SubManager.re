open Language;
open IdTagged.FreshGrammar;

// SubManager: Manages subscriptions for Hazel apps
//
// Sub is a sum type defined in BuiltinsADT.re:
//   | SubNone
//   | SubBatch(List(Sub))
//   | OnResize((Html, Int, Int) -> Html)
//   | OnVisibilityChange((Html, Bool) -> Html)
//   | OnDocumentKeyDown((Html, KeyEvent) -> Html)
//   | OnDocumentKeyUp((Html, KeyEvent) -> Html)
//   | Every(Float, (Html, Float) -> Html)
//   | AnimationFrame((Html, Float) -> Html)

type context = {
  model: DHExp.t,
  inject: DHExp.t => Ui_effect.t(unit),
  update_fn: option(DHExp.t),
};

// Track active subscriptions for cleanup
type sub_handle =
  | IntervalHandle(Js_of_ocaml.Dom_html.interval_id)
  | AnimationHandle(Js_of_ocaml.Dom_html.animation_frame_request_id)
  | EventHandle(unit => unit); // cleanup function

type active_subs = list(sub_handle);

// Strip evaluator wrappers (Asc, Closure, Parens) to find constructor
let rec of_constructor = (d: DHExp.t): option((string, DHExp.t)) =>
  switch (d.term) {
  | Asc(inner, _)
  | Closure(_, inner)
  | Parens(inner) => of_constructor(inner)
  | Ap(Forward, fn, body) =>
    switch (fn.term) {
    | Constructor(name, _) => Some((name, body))
    | Asc({term: Constructor(name, _), _}, _) => Some((name, body))
    | Closure(_, {term: Constructor(name, _), _}) => Some((name, body))
    | _ => None
    }
  | Constructor(name, _) =>
    Some((
      name,
      {
        ...d,
        term: Tuple([]),
      },
    ))
  | _ => None
  };

// Strip evaluator wrappers (Asc, Closure, Parens) from outermost level
let rec strip_wrappers = (d: DHExp.t): DHExp.t =>
  switch (d.term) {
  | Asc(inner, _)
  | Closure(_, inner)
  | Parens(inner) => strip_wrappers(inner)
  | _ => d
  };

// Extract float from DHExp
let of_float = (d: DHExp.t): option(float) => {
  let d = strip_wrappers(d);
  switch (d.term) {
  | Atom(Float(f)) => Some(f)
  | _ => None
  };
};

// Extract list from DHExp
let of_list = (d: DHExp.t): option(list(DHExp.t)) => {
  let d = strip_wrappers(d);
  switch (d.term) {
  | ListLit(items) => Some(items)
  | _ => None
  };
};

// Extract tuple components
let of_tuple = (d: DHExp.t): option(list(DHExp.t)) => {
  let d = strip_wrappers(d);
  switch (d.term) {
  | Tuple(items) => Some(items)
  | _ => None
  };
};

// Evaluate a Hazel expression directly (skip elaboration/statics).
// Handlers from subscriptions are already-evaluated Closures, so
// re-elaborating would fail on runtime-only nodes like Closure.
let evaluate = exp => fst(Evaluator.evaluate(~env=Builtins.env_init, exp));

// Error boundary: wrap evaluate to catch exceptions
let safe_evaluate = (exp: DHExp.t): result(DHExp.t, string) =>
  try(Ok(evaluate(exp))) {
  | exn => Error(Printexc.to_string(exn))
  };

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

// Build a KeyEvent tuple from a JS keyboard event
let key_event_of_js = evt => {
  open Js_of_ocaml;
  let get_key = evt =>
    Js.to_string(Js.Optdef.get(evt##.key, () => Js.string("")));
  let get_code = evt =>
    Js.to_string(Js.Optdef.get(evt##.code, () => Js.string("")));
  Exp.tuple([
    Exp.string(get_key(evt)),
    Exp.string(get_code(evt)),
    Exp.bool(Js.to_bool(evt##.ctrlKey)),
    Exp.bool(Js.to_bool(evt##.shiftKey)),
    Exp.bool(Js.to_bool(evt##.altKey)),
    Exp.bool(Js.to_bool(evt##.metaKey)),
  ]);
};

// Subscribe to a single subscription, returning handles for cleanup
let rec subscribe =
        (ctx: context, sub: DHExp.t, get_model: unit => DHExp.t)
        : list(sub_handle) => {
  Js_of_ocaml.(
    switch (of_constructor(sub)) {
    | None =>
      Firebug.console##log("SubManager: not a constructor");
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
      Firebug.console##log(
        Js.string("SubManager: unknown subscription: " ++ name),
      );
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
        | AnimationHandle(id) => Dom_html.window##cancelAnimationFrame(id)
        | EventHandle(cleanup_fn) => cleanup_fn()
        },
      handles,
    )
  );
};
