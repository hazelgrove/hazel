open Js_of_ocaml;
open WebUtil;

/* A Menu presented as a modal bottom sheet ("action sheet"), the shape a
 * menu takes under a finger instead of an anchored popup. Items, state and
 * keyboard handling are the popup's (see Menu); only the presentation
 * differs:
 *
 *   - a <dialog> opened with showModal(), so the browser supplies the
 *     scrim (::backdrop), the top layer and an inert page behind it;
 *   - a header naming what the menu acts on, or the submenu drilled into,
 *     with a back control in place of Menu's `← Back` row;
 *   - touch-sized rows that fire on click, without shortcut hints.
 *
 * The sheet itself dismisses through `on_close` on a scrim tap, a drag
 * down on the header, and the dialog's `cancel` (Esc, Android back).
 * Choosing a row closes it the way it closes the popup: the caller's
 * action handler does.
 *
 * The dialog carries the caller's `menu_class`, so the caller's
 * MenuListener counts every tap while it is open as inside the menu. The
 * scrim included: it hit-tests as the dialog. */

/* A header drag past this, or past a quarter of the sheet if that is
 * shorter, dismisses it; a shorter one springs back. */
let dismiss_drag_px = 96.0;
/* Matches the transform transition on .action-sheet in action-sheet.css. */
let slide_out_ms = 150.0;

let set_style = (el: Js.t(Dom_html.element), prop: string, value: string) =>
  Js.Unsafe.set(el##.style, prop, Js.string(value));

let closest = (target: Js.Opt.t(Js.t(Dom_html.element)), sel: string): bool =>
  switch (Js.Opt.to_option(target)) {
  | None => false
  | Some(t) =>
    Js.Opt.test(
      Js.Unsafe.meth_call(
        t,
        "closest",
        [|Js.Unsafe.inject(Js.string(sel))|],
      ),
    )
  };

let listen =
    (
      ~capture=false,
      el: Js.t(Dom_html.element),
      name: string,
      handler: Js.t(Dom_html.pointerEvent) => unit,
    )
    : unit =>
  ignore(
    Dom_html.addEventListener(
      el,
      Dom.Event.make(name),
      Dom_html.handler(e => {
        handler(e);
        Js._true;
      }),
      Js.bool(capture),
    ),
  );

module Hook =
  Attr.Hooks.Make({
    module Input = {
      type t = unit => Ui_effect.t(unit);
      let sexp_of_t = _ => Sexplib0.Sexp.Atom("<on_close>");
      let combine = (_, newer) => newer;
    };
    module State = {
      type t = {
        /* Refreshed on every render, like MenuListener's closures. */
        on_close: ref(Input.t),
        /* clientY where the current header drag started. */
        drag_from: ref(option(float)),
        /* Whether a press since the last click began in the sheet (or on
         * its scrim) and didn't become a drag: the only presses whose
         * clicks are taps on the sheet. */
        pressed: ref(bool),
      };
    };

    let init = (on_close, el: Js.t(Dom_html.element)): State.t => {
      let state: State.t = {
        on_close: ref(on_close),
        drag_from: ref(None),
        pressed: ref(false),
      };
      let close = () => Ui_effect.Expert.handle(state.on_close^());
      let drag_dy = (e: Js.t(Dom_html.pointerEvent)) =>
        switch (state.drag_from^) {
        | Some(from) => Float.max(0.0, float_of_int(e##.clientY) -. from)
        | None => 0.0
        };
      let end_drag = (~dismiss: bool) => {
        state.drag_from := None;
        set_style(el, "transition", "");
        if (dismiss) {
          set_style(el, "transform", "translateY(100%)");
          ignore(
            Dom_html.window##setTimeout(
              Js.wrap_callback(close),
              Js.float(slide_out_ms),
            ),
          );
        } else {
          set_style(el, "transform", "");
        };
      };
      listen(
        el,
        "pointerdown",
        e => {
          state.pressed := true;
          if (Js.to_bool(e##.isPrimary)
              && closest(e##.target, ".action-sheet-header")
              && !closest(e##.target, "button")) {
            state.drag_from := Some(float_of_int(e##.clientY));
            set_style(el, "transition", "none");
            try(
              Js.Unsafe.meth_call(
                el,
                "setPointerCapture",
                [|Js.Unsafe.inject(e##.pointerId)|],
              )
            ) {
            | _ => ()
            };
          };
        },
      );
      listen(el, "pointermove", e =>
        if (state.drag_from^ != None) {
          let dy = drag_dy(e);
          if (dy > 0.0) {
            state.pressed := false;
          };
          set_style(el, "transform", Printf.sprintf("translateY(%fpx)", dy));
        }
      );
      listen(el, "pointerup", e =>
        if (state.drag_from^ != None) {
          let height = float_of_int(el##.offsetHeight);
          end_drag(
            ~dismiss=drag_dy(e) > Float.min(dismiss_drag_px, height /. 4.0),
          );
        }
      );
      listen(el, "pointercancel", _ =>
        if (state.drag_from^ != None) {
          end_drag(~dismiss=false);
        }
      );
      /* The dialog sits inside its menu's caller in the DOM (the top layer
       * changes painting, not propagation), so its presses would otherwise
       * bubble on to the caller as presses there: a code editor would take
       * one for a tap and move its caret. */
      List.iter(
        name => listen(el, name, Dom_html.stopPropagation),
        [
          "pointerdown",
          "pointerup",
          "pointermove",
          "pointercancel",
          "mousedown",
          "mouseup",
          "click",
        ],
      );
      listen(el, "contextmenu", Dom.preventDefault);
      /* Runs in the capture phase, ahead of the rows' own handlers. A
       * click whose press began outside the sheet is swallowed: lifting
       * the long-press that opened the sheet makes one wherever the
       * finger is, which is now over a row. Keyboard clicks (detail 0)
       * pass. The scrim is the dialog's ::backdrop, which hit-tests as the
       * dialog itself; the dialog's own box has no bare area (the header
       * and rows fill it), so a click on the dialog outside its rect is a
       * scrim tap. */
      listen(
        ~capture=true,
        el,
        "click",
        e => {
          let pressed = state.pressed^;
          state.pressed := false;
          let rect = el##getBoundingClientRect;
          let (x, y) = (
            float_of_int(e##.clientX),
            float_of_int(e##.clientY),
          );
          let outside =
            x <
            rect##.left
            || x >
            rect##.right
            || y <
            rect##.top
            || y >
            rect##.bottom;
          let on_dialog =
            switch (Js.Opt.to_option(e##.target)) {
            | Some(t) => t === el
            | None => false
            };
          if (!pressed && Js.Unsafe.get(e, "detail") != 0) {
            Dom_html.stopPropagation(e);
            Dom.preventDefault(e);
          } else if (on_dialog && outside) {
            close();
          };
        },
      );
      /* Esc and Android back. Cancelling keeps the dialog open until the
       * caller's state closes it and the render removes it. */
      listen(
        el,
        "cancel",
        e => {
          Dom.preventDefault(e);
          close();
        },
      );
      state;
    };

    let on_mount = (_, _, el: Js.t(Dom_html.element)) =>
      if (!Js.to_bool(Js.Unsafe.get(el, "open"))) {
        Js.Unsafe.meth_call(el, "showModal", [||]);
      };

    let update = (~old_input as _, ~new_input, state: State.t, _) =>
      state.on_close := new_input;

    /* close() hands focus back to whatever held it before showModal. */
    let destroy = (_, _, el: Js.t(Dom_html.element)) =>
      if (Js.to_bool(Js.Unsafe.get(el, "open"))) {
        Js.Unsafe.meth_call(el, "close", [||]);
      };
  });

/* The chevron is drawn in CSS, to match the title's weight. */
let back_button = (~inject_menu: Menu.action => Ui_effect.t(unit)) =>
  Node.button(
    ~attrs=[
      clss(["action-sheet-back"]),
      Attr.create("aria-label", "Back"),
      Attr.on_click(_ => inject_menu(BackSubmenu)),
    ],
    [],
  );

/* What the header names: `title`, set in the code font when `code`, under
 * an optional small `label` saying what kind of thing it is. */
type heading = {
  label: option(string),
  title: string,
  code: bool,
};

let view =
    (
      ~menu_class: string,
      ~heading: heading,
      ~on_close: unit => Ui_effect.t(unit),
      ~inject_action: 'a => Ui_effect.t(unit),
      ~inject_menu: Menu.action => Ui_effect.t(unit),
      ~items: list(Menu.item('a)),
      model: Menu.t,
    )
    : Node.t => {
  let path = Menu.path(model);
  /* Drilled into a submenu, the header names it and offers the way back. */
  let (back, {label, title, code}) =
    switch (List.rev(path)) {
    | [] => ([], heading)
    | [submenu, ..._] => (
        [back_button(~inject_menu)],
        {
          label: None,
          title: submenu,
          code: false,
        },
      )
    };
  let titles =
    (
      switch (label) {
      | Some(l) => [div_c("action-sheet-label", [Node.text(l)])]
      | None => []
      }
    )
    @ [
      Node.div(
        ~attrs=[
          clss(["action-sheet-title"] @ (code ? ["code-title"] : [])),
        ],
        [Node.text(title)],
      ),
    ];
  Node.create(
    "dialog",
    ~attrs=[
      clss([menu_class, "action-sheet"]),
      Attr.create("aria-label", title),
      Attr.create_hook("action-sheet", Hook.create(on_close)),
    ],
    [
      div_c(
        "action-sheet-header",
        [div_c("action-sheet-grabber", [])]
        @ back
        @ [div_c("action-sheet-titles", titles)],
      ),
      /* Keyed by path so each submenu panel is a fresh node and replays
       * the drill-in animation. */
      Node.div(
        ~key=String.concat("\000", path),
        ~attrs=[
          clss(["action-sheet-rows"] @ (path == [] ? [] : ["nested"])),
        ],
        Menu.render(
          ~sheet=true,
          ~inject_action,
          ~inject_menu,
          ~item_class="action-sheet-row",
          ~items,
          model,
        ),
      ),
    ],
  );
};
