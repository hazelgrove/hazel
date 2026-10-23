/* Embed mode, index.html#embed: Hazel in another page's iframe. The page
   posts a program; the frame shows it as one read-only cell of tiles with
   statics on and Hazel's own error marks, which a click, a drag or the
   arrow keys select in (EmbedModel.View), and posts back its size and the
   program's errors and holes (EmbedProtocol has the messages, and
   EmbedModel the state).

   None of the app's startup runs here: no IndexedDB, saved settings or
   theme, autosave, replay clock, worker, focus grabs, selection guards or
   press tracking. The page root carries disable_tab_index, so incr_dom
   gives it no tabindex, and its refocusing (on startup, and on a window
   blur) has nothing to focus: the frame does not take focus from the page
   by itself. A click in it focuses it, as it does any frame.

   EmbedLink has what is taken from the page and what is posted to it.
   A page may send a program on every change: the frame shows the latest
   one sent, set once per animation frame. */
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

let link = (): EmbedLink.t =>
  EmbedLink.make(
    ~parent=parent(), ~own_origin=own_origin(), ~post=(data, target) =>
    ignore(
      Js.Unsafe.meth_call(
        parent(),
        "postMessage",
        [|data, Js.Unsafe.inject(Js.string(target))|],
      ): Js.Unsafe.any,
    )
  );

let crash = (~link, message: string): unit => {
  print_endline("ERROR (embed): " ++ message);
  EmbedLink.crash(link, message);
};

let model = Bonsai.Var.create(EmbedModel.Model.init);

let update = (~link, action: EmbedModel.Update.t): unit =>
  Bonsai.Var.update(model, ~f=m =>
    try(EmbedModel.Update.update(action, m)) {
    | exn =>
      crash(~link, Printexc.to_string(exn));
      m;
    }
  );

/* The latest program sent and not yet set: one is set per animation
   frame, so programs sent faster than they are read do not queue up */
let pending: ref(option(EmbedModel.Program.t)) = ref(None);

let set_soon = (~link, program: EmbedModel.Program.t): unit => {
  let scheduled = Option.is_some(pending^);
  pending := Some(program);
  if (!scheduled) {
    ignore(
      Dom_html.window##requestAnimationFrame(
        Js.wrap_callback(_ =>
          switch (pending^) {
          | Some(program) =>
            pending := None;
            update(~link, Set(program));
          | None => ()
          }
        ),
      ),
    );
  };
};

/* (ticket 578, Matthew 2026-10-23: dark mode and high contrast when the
   page around is dark, "I want that default in general everywhere") the
   theme a page asks for: Hazel's own Colors slide, its dark_mode and
   high_contrast switches set as asked, evaluated as the app does at
   startup, its colors put on the frame's document. Once per choice. */
let themes: Hashtbl.t((bool, bool), list((string, string))) =
  Hashtbl.create(4);
let apply_theme = (~dark: bool, ~contrast: bool): unit => {
  let vars =
    switch (Hashtbl.find_opt(themes, (dark, contrast))) {
    | Some(vars) => vars
    | None =>
      let set = (name, on, text) =>
        StringUtil.plain_replace(
          "let " ++ name ++ " = \\^\\^check\\((true|false)\\)",
          text,
          "let " ++ name ++ " = ^^check(" ++ string_of_bool(on) ++ ")",
        );
      let text =
        ColorConfiguration.source.backup_text
        |> set("dark_mode", dark)
        |> set("high_contrast", contrast);
      let vars =
        ColorConfiguration.vars_of_source(
          Haz3lcore.PersistentZipper.of_text(text),
        );
      Hashtbl.replace(themes, (dark, contrast), vars);
      vars;
    };
  if (vars != []) {
    ConfigurationMode.apply_colors(vars);
  };
};

let set_font_metrics = (~link, ~col_width: float, ~row_height: float): unit => {
  JsUtil.set_css_custom_property(
    "--row-height-px",
    Printf.sprintf("%fpx", row_height),
  );
  update(
    ~link,
    SetFontMetrics({
      row_height,
      col_width,
    }),
  );
};

/* (^ticket(587), Matthew 2026-10-23: "font size chips don't affect hazel
   tile editor yet?") the page's font for the code: Hazel's own two
   settings set (--base-font-size, --code-font, as variables.css has
   them), and the font measured again from the specimen, as a zoom does */
let apply_font =
    (~link, ~size: option(float), ~family: option(string)): unit => {
  switch (size) {
  | Some(px) when px >= 6. && px <= 48. =>
    JsUtil.set_css_custom_property(
      "--base-font-size",
      Printf.sprintf("%gpx", px),
    )
  | _ => ()
  };
  switch (family) {
  | Some(f) when String.length(f) > 0 && String.length(f) < 200 =>
    JsUtil.set_css_custom_property("--code-font", f)
  | _ => ()
  };
  let (col_width, row_height) = JsUtil.font_metrics_from_specimen();
  set_font_metrics(~link, ~col_width, ~row_height);
};

let on_message = (~link, event: Js.Unsafe.any): unit =>
  try(
    switch (EmbedLink.receive(link, event)) {
    | Some(program) => set_soon(~link, program)
    | None =>
      switch (link.editable) {
      | Some(on) =>
        link.editable = None;
        update(~link, SetEditable(on));
      | None => ()
      };
      switch (link.font) {
      | Some((size, family)) =>
        link.font = None;
        apply_font(~link, ~size, ~family);
      | None => ()
      };
      switch (link.reveal) {
      | Some(name) =>
        link.reveal = None;
        update(~link, Reveal(name));
      | None => ()
      };
      switch (link.lines) {
      | Some(on) =>
        link.lines = None;
        update(~link, SetLineNumbers(on));
      | None => ()
      };
      switch (link.theme) {
      | Some((dark, contrast)) =>
        link.theme = None;
        apply_theme(~dark, ~contrast);
      | None => ()
      };
      switch (link.focus) {
      | Some(name) =>
        link.focus = None;
        update(~link, SetFocus(name));
      | None => ()
      };
    }
  ) {
  | exn => crash(~link, Printexc.to_string(exn))
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

/* After a display, the height of the page root, rounded up so the
   content always fits the frame sized to it (rows are a fraction of a
   pixel tall), and the status, posted to the page when they change */
let report = (~link, model: EmbedModel.Model.t): unit =>
  switch (JsUtil.get_elem_by_id_opt("page")) {
  | Some(page) =>
    let rect: Js.Unsafe.any =
      Js.Unsafe.meth_call(page, "getBoundingClientRect", [||]);
    let height =
      int_of_float(
        Float.ceil(Js.float_of_number(Js.Unsafe.get(rect, "height"))),
      );
    /* (^ticket(587)) the caret's top, for the page to scroll to after a
       reveal */
    let caret =
      switch (JsUtil.get_elem_by_id_opt("caret")) {
      | Some(c) =>
        let r: Js.Unsafe.any =
          Js.Unsafe.meth_call(c, "getBoundingClientRect", [||]);
        Some(int_of_float(Js.float_of_number(Js.Unsafe.get(r, "top"))));
      | None => None
      };
    EmbedLink.report(link, ~caret?, ~height, model);
  | None => ()
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

let view = (~link, ~standalone: bool, model: EmbedModel.Model.t): Node.t =>
  page(
    switch (model.program) {
    | None => standalone ? [note] : []
    | Some(_) => [
        EmbedModel.View.view(
          ~inject=
            action => Effect.of_sync_fun(() => update(~link, action), ()),
          model,
        ),
      ]
    },
  );

let app = (~link, ~standalone: bool) => {
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
                ~link,
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
              set_font_metrics(~link, ~col_width, ~row_height);
            });
            EmbedLink.ready(link);
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
            try(report(~link, model)) {
            | exn => crash(~link, Printexc.to_string(exn))
            },
          (),
        );
      },
    );
  let%arr model = Bonsai.Var.value(model);
  try(view(~link, ~standalone, model)) {
  | exn =>
    crash(~link, Printexc.to_string(exn));
    page([Node.text("This program could not be shown.")]);
  };
};

let go = () => {
  let link = link();
  let standalone = standalone();
  let classes = Dom_html.document##.documentElement##.classList;
  classes##add(Js.string("hazel-embed"));
  /* in a frame, the page sizes the frame to the height posted, so the
     frame itself does not scroll (embed.css) */
  if (!standalone) {
    classes##add(Js.string("framed"));
  };
  listen("message", on_message(~link));
  /* an exception nothing else caught */
  listen("error", event =>
    crash(
      ~link,
      switch (Js.Optdef.to_option(Js.Unsafe.get(event, "message"))) {
      | Some(message) => Js.to_string(message)
      | None => "an uncaught error"
      },
    )
  );
  Bonsai_web.Start.start(
    app(~link, ~standalone),
    ~bind_to_element_with_id="container",
  );
};
