/* Embed mode, index.html#embed: Hazel in another page's iframe. The page
   posts a program; the frame shows it as one read-only cell of tiles with
   statics on and Hazel's own error marks, and posts back its size and the
   program's errors and holes (EmbedProtocol has the messages, and
   EmbedModel the state).

   None of the app's startup runs here: no IndexedDB, saved settings or
   theme, autosave, replay clock, worker, focus grabs, selection guards or
   press tracking. The page root carries disable_tab_index, so incr_dom
   gives it no tabindex, and its refocusing (on startup, and on a window
   blur) has nothing to focus: the frame does not take focus from the page
   by itself. A click in it focuses it, as it does any frame.

   Messages are taken only from the parent window, from an origin
   EmbedProtocol.origin_allowed allows, and the frame then posts only to
   that origin. Before any, it posts ready and crash (which carry no
   program) to any origin. */
open Js_of_ocaml;
open Util;
open Virtual_dom.Vdom;
open Bonsai.Let_syntax;

let window: Js.Unsafe.any = Js.Unsafe.inject(Dom_html.window);

let parent = (): Js.Unsafe.any => Js.Unsafe.get(window, "parent");

/* opened on its own, not in a frame */
let standalone = (): bool => Js.Unsafe.equals(parent(), window);

let own_origin = (): string =>
  Js.to_string(Js.Unsafe.get(Js.Unsafe.get(window, "location"), "origin"));

/* the origin of the last message taken from the page */
let page_origin: ref(option(string)) = ref(None);

let post = (~target: string, message: EmbedModel.Outbox.message): unit =>
  ignore(
    Js.Unsafe.meth_call(
      parent(),
      "postMessage",
      [|
        EmbedProtocol.encode(message),
        Js.Unsafe.inject(Js.string(target)),
      |],
    ): Js.Unsafe.any,
  );

let ready = (~target) => post(~target, Ready(BuildStamp.commit));

/* each distinct crash is posted once */
let crashes: ref(list(string)) = ref([]);

let crash = (message: string): unit => {
  print_endline("ERROR (embed): " ++ message);
  if (!List.mem(message, crashes^)) {
    crashes := [message, ...crashes^];
    try(
      post(~target=Option.value(page_origin^, ~default="*"), Crash(message))
    ) {
    | _ => ()
    };
  };
};

let model = Bonsai.Var.create(EmbedModel.Model.init);

let update = (action: EmbedModel.Update.t): unit =>
  Bonsai.Var.update(model, ~f=m =>
    try(EmbedModel.Update.update(action, m)) {
    | exn =>
      crash(Printexc.to_string(exn));
      m;
    }
  );

let on_message = (event: Js.Unsafe.any): unit =>
  try({
    let origin = Js.to_string(Js.Unsafe.get(event, "origin"));
    if (Js.Unsafe.equals(Js.Unsafe.get(event, "source"), parent())
        && EmbedProtocol.origin_allowed(~own=own_origin(), origin)) {
      switch (EmbedProtocol.decode(Js.Unsafe.get(event, "data"))) {
      | Some(Set(program)) =>
        page_origin := Some(origin);
        update(Set(program));
      | Some(Ping) =>
        page_origin := Some(origin);
        ready(~target=origin);
      | None => ()
      };
    };
  }) {
  | exn => crash(Printexc.to_string(exn))
  };

let listen = (name: string, f: Js.Unsafe.any => unit): unit =>
  ignore(
    Dom.addEventListener(
      Dom_html.window,
      Dom.Event.make(name),
      Dom.handler(event => {
        f(Js.Unsafe.inject(event));
        Js._true;
      }),
      Js._false,
    ): Dom.event_listener_id,
  );

/* After a display, the height of the page root and the status, posted to
   the page when they change */
let sent = ref(EmbedModel.Outbox.nothing_sent);

let report = (model: EmbedModel.Model.t): unit =>
  switch (page_origin^, JsUtil.get_elem_by_id_opt("page")) {
  | (Some(target), Some(page)) =>
    let (messages, now_sent) =
      EmbedModel.Outbox.after_display(
        ~height=page##.scrollHeight,
        model,
        sent^,
      );
    sent := now_sent;
    List.iter(post(~target), messages);
  | _ => ()
  };

let set_font_metrics = (~col_width: float, ~row_height: float): unit => {
  JsUtil.set_css_custom_property(
    "--row-height-px",
    Printf.sprintf("%fpx", row_height),
  );
  update(
    SetFontMetrics({
      row_height,
      col_width,
    }),
  );
};

let note =
  Node.div(
    ~attrs=[Attr.class_("embed-note")],
    [
      Node.text(
        "Hazel's embed mode: a page that frames this one sends it a program "
        ++ "to show, by postMessage (docs/hazel-as-a-js-library.md).",
      ),
    ],
  );

/* The page root: no tabindex for incr_dom to give it, so nothing to
   refocus */
let page = (children: list(Node.t)): Node.t =>
  Node.div(
    ~attrs=[
      Attr.id("page"),
      Attr.class_("embed"),
      Attr.create("disable_tab_index", ""),
    ],
    [FontSpecimen.view, Node.div(~attrs=[Attr.id("main")], children)],
  );

let view = (~standalone: bool, model: EmbedModel.Model.t): Node.t =>
  page(
    switch (model.program) {
    | None => standalone ? [note] : []
    | Some(_) => [EmbedModel.View.view(model)]
    },
  );

let app = (~standalone: bool) => {
  let%sub size =
    BonsaiUtil.SizeObserver.observer(
      () => JsUtil.get_elem_by_id("font-specimen"),
      ~default=
        BonsaiUtil.SizeObserver.Size.{
          width: 10.,
          height: 10.,
        },
    );
  let%sub () =
    Bonsai.Edge.on_change(
      (module BonsaiUtil.SizeObserver.Size),
      size,
      ~callback=
        Bonsai.Value.return((rect: BonsaiUtil.SizeObserver.Size.t) =>
          Bonsai.Effect.of_sync_fun(
            () =>
              set_font_metrics(
                ~col_width=rect.width,
                ~row_height=rect.height,
              ),
            (),
          )
        ),
    );
  let%sub () =
    BonsaiUtil.OnStartup.on_startup(
      Bonsai.Value.return(
        Bonsai.Effect.of_sync_fun(
          () => {
            /* ResizeObserver misses a zoom (a DPR change) */
            JsUtil.on_dpr_change(() => {
              let (col_width, row_height) =
                JsUtil.font_metrics_from_specimen();
              set_font_metrics(~col_width, ~row_height);
            });
            ready(~target=Option.value(page_origin^, ~default="*"));
          },
          (),
        ),
      ),
    );
  let%sub () =
    Bonsai.Edge.after_display(
      {
        let%map model = Bonsai.Var.value(model);
        Bonsai.Effect.of_sync_fun(
          () =>
            try(report(model)) {
            | exn => crash(Printexc.to_string(exn))
            },
          (),
        );
      },
    );
  let%arr model = Bonsai.Var.value(model);
  try(view(~standalone, model)) {
  | exn =>
    crash(Printexc.to_string(exn));
    page([Node.text("This program could not be shown.")]);
  };
};

let go = () => {
  Dom_html.document##.documentElement##.classList##add(
    Js.string("hazel-embed"),
  );
  listen("message", on_message);
  /* an exception nothing else caught */
  listen("error", event =>
    crash(
      switch (Js.Optdef.to_option(Js.Unsafe.get(event, "message"))) {
      | Some(message) => Js.to_string(message)
      | None => "an uncaught error"
      },
    )
  );
  Bonsai_web.Start.start(
    app(~standalone=standalone()),
    ~bind_to_element_with_id="container",
  );
};
