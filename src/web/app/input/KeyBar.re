open Js_of_ocaml;
open Util;
open Util.WebUtil;
open Virtual_dom.Vdom;

/* The keys a phone keyboard lacks, as a row of buttons just above it: the
 * arrows, a Select toggle that makes them extend the selection (Shift on
 * desktop), Tab, Undo and Redo, and a key for the context menu.
 *
 * A button presses its key on the focused element, the editor's hidden
 * input, as a keydown the editor and page handle exactly as a typed one,
 * so each does what that key does on desktop. Buttons never take focus:
 * the input keeps it, and the phone keyboard stays up. Arrows repeat
 * while held.
 *
 * Shown by CSS only: on a coarse pointer, while an editor's input has
 * focus (see key-bar.css). */

let select_mode: ref(bool) = ref(false);

let press = (~shift=false, ~command=false, key: string): unit =>
  switch (Js.Opt.to_option(Dom_html.document##.activeElement)) {
  | None => ()
  | Some(target) =>
    let init =
      Js.Unsafe.obj([|
        ("key", Js.Unsafe.inject(Js.string(key))),
        ("bubbles", Js.Unsafe.inject(Js._true)),
        ("cancelable", Js.Unsafe.inject(Js._true)),
        ("shiftKey", Js.Unsafe.inject(Js.bool(shift))),
        ("metaKey", Js.Unsafe.inject(Js.bool(command && Os.is_mac^))),
        ("ctrlKey", Js.Unsafe.inject(Js.bool(command && ! Os.is_mac^))),
      |]);
    let evt =
      Js.Unsafe.new_obj(
        Js.Unsafe.global##.KeyboardEvent,
        [|Js.Unsafe.inject(Js.string("keydown")), Js.Unsafe.inject(init)|],
      );
    ignore(Js.Unsafe.meth_call(target, "dispatchEvent", [|evt|]));
  };

/* Held arrows: a first press, then repeats after a pause, until any
 * pointer lifts or is cancelled. */
module Repeat = {
  let delay_ms = 400.0;
  let interval_ms = 70.0;
  let timer: ref(option(Dom_html.timeout_id_safe)) = ref(None);
  let listening = ref(false);

  let stop = (): unit =>
    switch (timer^) {
    | Some(id) =>
      Dom_html.clearTimeout(id);
      timer := None;
    | None => ()
    };

  let rec schedule = (ms: float, f: unit => unit): unit =>
    timer :=
      Some(
        Dom_html.setTimeout(
          () => {
            f();
            schedule(interval_ms, f);
          },
          ms,
        ),
      );

  let start = (f: unit => unit): unit => {
    if (! listening^) {
      listening := true;
      List.iter(
        name =>
          ignore(
            Dom_html.addEventListener(
              Dom_html.window,
              Dom.Event.make(name),
              Dom_html.handler(_ => {
                stop();
                Js._true;
              }),
              Js._true,
            ),
          ),
        ["pointerup", "pointercancel"],
      );
    };
    stop();
    schedule(delay_ms, f);
  };
};

/* Pressing never moves focus off the editor's input, which would put the
 * phone keyboard away. */
let key =
    (
      ~label: string,
      ~classes=[],
      ~pressed: option(bool)=?,
      ~repeat=false,
      ~content: list(Node.t),
      on_press: Js.t(Dom_html.pointerEvent) => unit,
    )
    : Node.t =>
  Node.button(
    ~attrs=
      [
        clss(["key-bar-key"] @ classes),
        Attr.create("aria-label", label),
        Attr.tabindex(-1),
        Attr.on_pointerdown(evt => {
          on_press(evt);
          if (repeat) {
            Repeat.start(() => on_press(evt));
          };
          Effect.Many([Effect.Prevent_default, Effect.Stop_propagation]);
        }),
      ]
      @ (
        switch (pressed) {
        | Some(p) => [Attr.create("aria-pressed", p ? "true" : "false")]
        | None => []
        }
      ),
    content,
  );

let arrow = (label: string, glyph: string, key_name: string): Node.t =>
  key(~label, ~repeat=true, ~content=[Node.text(glyph)], _ =>
    press(~shift=select_mode^, key_name)
  );

/* Flipped in place: the bar is not re-rendered for it. */
let toggle_select = (evt: Js.t(Dom_html.pointerEvent)): unit => {
  select_mode := ! select_mode^;
  Js.Opt.iter(evt##.currentTarget, button =>
    button##setAttribute(
      Js.string("aria-pressed"),
      Js.string(select_mode^ ? "true" : "false"),
    )
  );
};

/* What Tab would do at the caret, if anything worth a key: accept the
 * completion on offer, put down the backpack, or move to the next problem. */
let tab_label = (~has_problems: bool, z: Haz3lcore.Zipper.t): option(string) =>
  if (Haz3lcore.Selection.is_buffer(z.selection)) {
    Some("Accept");
  } else if (Haz3lcore.Zipper.can_put_down(z)) {
    Some("Put down");
  } else if (has_problems) {
    Some("Next ⇥");
  } else {
    None;
  };

/* Keys show only when they would do something: Undo and Redo with history
 * to step through, Tab under the label of what it would do. The menu key
 * opens the context menu (Shift+F10). */
let view =
    (~can_undo: bool, ~can_redo: bool, cursor: Cursor.cursor(_)): Node.t => {
  /* Tab's next-problem targets (Perform's Move) are errors, warnings and
     holes; the cursor carries the first and the editor the last. */
  let tab =
    switch (cursor.editor) {
    | Some(editor) =>
      tab_label(
        ~has_problems=
          cursor.error_ids != []
          || List.exists(
               (g: Haz3lcore.Grout.t) => g.shape == Convex,
               Haz3lcore.Segment.holes(editor.syntax.segment),
             ),
        editor.state.zipper,
      )
    | None => None
    };
  let only = (show, node) => show ? [node] : [];
  Node.div(
    ~attrs=[Attr.id("key-bar")],
    [
      arrow("Left", "←", "ArrowLeft"),
      arrow("Up", "↑", "ArrowUp"),
      arrow("Down", "↓", "ArrowDown"),
      arrow("Right", "→", "ArrowRight"),
      key(
        ~label="Select",
        ~classes=["key-bar-word"],
        ~pressed=select_mode^,
        ~content=[Node.text("Select")],
        toggle_select,
      ),
    ]
    @ (
      switch (tab) {
      | Some(label) => [
          key(
            ~label,
            ~classes=["key-bar-word"],
            ~content=[Node.text(label)],
            _ =>
            press("Tab")
          ),
        ]
      | None => []
      }
    )
    @ only(
        can_undo,
        key(~label="Undo", ~content=[Icons.undo], _ =>
          press(~command=true, "z")
        ),
      )
    @ only(
        can_redo,
        key(
          ~label="Redo", ~classes=["key-bar-redo"], ~content=[Icons.undo], _ =>
          press(~command=true, ~shift=true, "z")
        ),
      )
    @ [
      key(
        ~label="Menu",
        ~classes=["key-bar-menu"],
        ~content=[Node.text("⋯")],
        _ =>
        press(~shift=true, "F10")
      ),
    ],
  );
};
