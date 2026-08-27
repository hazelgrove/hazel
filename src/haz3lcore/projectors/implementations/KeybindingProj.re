open Util;
open ProjectorBase;
open Virtual_dom.Vdom;

/* Projects a `Shortcut` value (Language.BuiltinsADT.Shortcut) as a
   click-to-record keybinding widget — the editing UI for the Shortcuts
   config slide.

   It reads and writes shortcut SYNTAX, not evaluated values: the decoder
   matches `Bound(([Meta], "z"))` as a plain constructor application, which
   is what sits in the buffer. Recording produces `Meta` (never a literal
   "cmd"/"ctrl") whenever the platform modifier is held, so a shortcut
   recorded on a Mac means the same thing on Windows. */
module S = Language.BuiltinsADT.Shortcut;

/* Outside M so slide generators can build the initial model: a config source
   can ship with these projectors already attached (see
   ShortcutConfiguration), which needs the serialized model up front. */
module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    committed: S.binding,
    isRecording: bool,
  };
};

let model_string = (b: S.binding): string =>
  Model.{
    committed: b,
    isRecording: false,
  }
  |> Model.sexp_of_t
  |> Sexplib.Sexp.to_string;

module M: Projector = {
  module S = S;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model =
    Model.t = {
      committed: S.binding,
      isRecording: bool,
    };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | StartRecording
    | CommitRecording
    | CancelRecording;

  let binding_of = (any: Language.Any.t): option(S.binding) =>
    switch (any) {
    | Exp(e) => S.binding_of_exp(e)
    | _ => None
    };

  let init = (any: Language.Any.t) =>
    switch (binding_of(any)) {
    | Some(b) =>
      Some({
        committed: b,
        isRecording: false,
      })
    | None => None
    };

  /* Total on purpose: the underlying syntax can be edited out from under a
     projector, and a failwith here takes the whole editor down. */
  let get = (info: info): S.binding =>
    switch (info.syntax |> info.utility.seg_to_term) {
    | Some(any) => binding_of(any) |> Option.value(~default=S.Unbound)
    | None => S.Unbound
    };

  /* ---- Rendering ----

     Shortcuts read as keycaps rather than as a "cmd+shift+z" string: it is
     the convention everywhere else shortcuts are shown, it is far narrower
     inline (⌘⇧Z vs cmd+shift+z), and it separates the modifiers from the key
     so the chord is scannable at a glance.

     The chord is shown RESOLVED — ⌘ on a Mac, Ctrl elsewhere — because that
     is what you actually press. The underlying syntax still says `Meta`, and
     the modifier chip carries a tooltip saying so, since the resolved form
     otherwise hides that the binding is portable. */

  let is_mac = () => Util.Os.is_mac^;

  let mod_symbol = (m: S.key_mod): string =>
    switch (m, is_mac()) {
    | (Meta, true) => {js|⌘|js}
    | (Meta, false) => "Ctrl"
    | (Ctrl, true) => {js|⌃|js}
    | (Ctrl, false) => "Ctrl"
    | (Alt, true) => {js|⌥|js}
    | (Alt, false) => "Alt"
    | (Shift, true) => {js|⇧|js}
    | (Shift, false) => "Shift"
    };

  let mod_tooltip = (m: S.key_mod): string =>
    switch (m) {
    | Meta => "Platform key: Command on macOS, Ctrl elsewhere. Portable."
    | Ctrl => "Literal Control. Does not follow the platform."
    | Alt => is_mac() ? "Option" : "Alt"
    | Shift => "Shift"
    };

  let key_symbol = (k: string): string =>
    switch (k) {
    | "left" => {js|←|js}
    | "right" => {js|→|js}
    | "up" => {js|↑|js}
    | "down" => {js|↓|js}
    | "tab" => {js|⇥|js}
    | "space" => {js|␣|js}
    | "enter" => {js|↩|js}
    | "escape" => "Esc"
    | "backspace" => {js|⌫|js}
    | k => String.uppercase_ascii(k)
    };

  /* Canonical order, so the same chord always reads the same way. */
  let ordered_mods = (mods: list(S.key_mod)): list(S.key_mod) =>
    List.filter(m => List.mem(m, mods), [S.Meta, S.Ctrl, S.Alt, S.Shift]);

  /* One description of the caps, so the rendered widget and the width the
     editor reserves for it cannot disagree — they did, and a 3-character cap
     like F12 pushed the clear button onto a second line. */
  let cap_specs = (b: S.binding): list((string, string, string)) =>
    switch (b) {
    | Unbound => [
        ({js|–|js}, "kbd-none", "No shortcut. Click to set one."),
      ]
    | Bound(mods, key) =>
      List.map(
        m => (mod_symbol(m), "kbd-mod", mod_tooltip(m)),
        ordered_mods(mods),
      )
      @ [(key_symbol(key), "", "")]
    };

  let cap = ((text, cls, tooltip)): Node.t =>
    Node.span(
      ~attrs=
        [Attr.classes(["kbd", ...cls == "" ? [] : [cls]])]
        @ (tooltip == "" ? [] : [Attr.title(tooltip)]),
      [Node.text(text)],
    );

  let caps_of_binding = (b: S.binding): list(Node.t) =>
    List.map(cap, cap_specs(b));

  /* Width is in editor columns. A cap costs its own text plus padding; the
     modifier symbols are single glyphs whose byte length would over-count,
     so anything non-ASCII counts as one column. */
  let display_len = (s: string): int =>
    String.for_all(c => Char.code(c) < 128, s) ? String.length(s) : 1;

  let placeholder = (model, info) => {
    let cols =
      model.isRecording
        ? 9
        : List.fold_left(
            (acc, (text, _, _)) => acc + display_len(text) + 1,
            /* the clear button, always present on a bound shortcut */
            get(info) == S.Unbound ? 1 : 3,
            cap_specs(get(info)),
          );
    ProjectorCore.Shape.inline(cols);
  };

  /* The recorder needs REAL DOM focus. With Focusable.non the editor keeps
     focus, so the widget never receives the keydown that stops recording nor
     the blur that ends it — recording would start on click and never clear. */
  let focus_element = (id: Id.t) =>
    switch (JsUtil.get_elem_by_id_opt(Id.cls(id))) {
    | Some(el) => el##focus
    | None => ()
    };

  let focusable =
    Focusable.{
      pointer: Some(focus_element),
      keyboard: Some((id, _d: Direction.t) => focus_element(id)),
    };

  let dynamics = false;
  /* Projects the raw Shortcut syntax; no elaboration needed. */
  let elaborate_syntax = false;
  let error = (_, _): option(ProjectorBase.error) => None;

  /* ---- Interaction ---- */

  /* Deferred and non-asserting on purpose. The old version called
     get_elem_by_id (which asserts) while BUILDING the Effect.Many list, so a
     missing element aborted construction and the recording state was never
     cleared — the widget kept its recording styling after losing focus. */
  /* DOM focus has to be (re)asserted AFTER the render, not during the event:
     a capture dispatches SetSyntax, the editor re-renders and pulls focus
     back to its cell, and anything done inline is undone. This is the
     framework's own hook for that — Main.re runs it in after_display. */
  let keep_focus = info =>
    Effect.of_sync_fun(() => FocusEffect.schedule(info.id), ());

  let blur = info =>
    Effect.of_sync_fun(
      () =>
        switch (JsUtil.get_elem_by_id_opt(Id.cls(info.id))) {
        | Some(el) => el##blur
        | None => ()
        },
      (),
    );

  let set_binding = (info, ~parent, b: S.binding) =>
    parent(
      SetSyntax(
        info.utility.term_to_seg(~inline=true, Exp(S.exp_of_binding(b))),
      ),
    );

  /* The physical key, normalised to what hotkeys-js expects. Modifier-only
     presses yield None so holding ⌘ alone does not commit a binding. */
  let key_name_of = (key: Key.t): option(string) => {
    let raw =
      switch (key.key) {
      | D(k) => k
      | U(k) => k
      };
    switch (raw) {
    | "Control"
    | "Shift"
    | "Alt"
    | "Meta" => None
    | "ArrowUp" => Some("up")
    | "ArrowDown" => Some("down")
    | "ArrowLeft" => Some("left")
    | "ArrowRight" => Some("right")
    | " " => Some("space")
    | k => Some(String.lowercase_ascii(k))
    };
  };

  /* The platform modifier records as the abstract `Meta`, never as a literal
     cmd/ctrl — that is what makes a shortcut recorded here mean the same
     thing on another machine. On a Mac ctrl stays available as literal
     `Ctrl`; on PC ctrl IS the platform modifier, so it records as Meta and
     there is no literal-ctrl to ask for, the two being the same key. */
  let mods_of = (key: Key.t): list(S.key_mod) => {
    let platform =
      switch (key.sys) {
      | Mac =>
        (key.meta == Down ? [S.Meta] : [])
        @ (key.ctrl == Down ? [S.Ctrl] : [])
      | PC => key.ctrl == Down || key.meta == Down ? [S.Meta] : []
      };
    platform
    @ (key.alt == Down ? [S.Alt] : [])
    @ (key.shift == Down ? [S.Shift] : []);
  };

  /* Modal capture: once recording, EVERY key is a candidate binding —
     including Enter, Tab and Backspace, which is the point (they are all
     bindable). Escape is the single exception and the only key that exits,
     besides clicking away. Unbinding is the × button, because no key can
     mean "no key". */
  let key_handler = (model, info, ~local, ~parent, evt) => {
    open Effect;
    let key = Key.mk(KeyDown, evt);
    switch (key.key) {
    | D("Escape") =>
      Many([
        local(CancelRecording),
        set_binding(info, ~parent, model.committed),
        blur(info),
        Stop_propagation,
        Prevent_default,
      ])
    | _ =>
      switch (key_name_of(key)) {
      | Some(name) =>
        Many([
          set_binding(info, ~parent, S.Bound(mods_of(key), name)),
          keep_focus(info),
          Stop_propagation,
          Prevent_default,
        ])
      /* A bare modifier: swallow it so it neither escapes to the editor nor
         clears what is already shown. */
      | None => Many([Stop_propagation, Prevent_default])
      }
    };
  };

  let update = (model, info, action) =>
    switch (action) {
    | StartRecording =>
      /* Idempotent: focus can fire again after a re-render, and restarting
         would overwrite `committed` and break Escape's revert. */
      model.isRecording
        ? model
        : {
          committed: info |> get,
          isRecording: true,
        }
    | CommitRecording => {
        committed: info |> get,
        isRecording: false,
      }
    | CancelRecording => {
        ...model,
        isRecording: false,
      }
    };

  let view = ({model, info, local, parent, _}: View.args(model, action)) => {
    let binding = get(info);
    let recording = model.isRecording;
    let caps =
      recording
        ? [cap(({js|press…|js}, "kbd-rec", ""))]
        : caps_of_binding(binding);
    /* Always rendered for a bound shortcut rather than revealed on hover, so
       the widget does not change width under the cursor. */
    let clear =
      !recording && binding != S.Unbound
        ? [
          Node.span(
            ~attrs=[
              Attr.classes(["keybinding-clear"]),
              Attr.title("Remove this shortcut"),
              Attr.on_pointerdown(_ =>
                Effect.Many([
                  set_binding(info, ~parent, S.Unbound),
                  Effect.Stop_propagation,
                ])
              ),
            ],
            [Node.text({js|×|js})],
          ),
        ]
        : [];
    /* A real, transparent <input> laid over the widget does the focusing.
       A plain div could not: the projector wrapper handles pointerdown by
       dispatching editor actions, which re-render before a click handler on
       the div ever runs, so recording never started on a real click. A
       native control is focused by the browser itself on mousedown, before
       any of that, and virtual_dom patches it in place across re-renders so
       the focus survives — the same reason TextAreaProj works. */
    let capture =
      Node.input(
        ~attrs=[
          Attr.id(Id.cls(info.id)),
          Attr.classes(["keybinding-capture"]),
          Attr.create("readonly", "readonly"),
          Attr.create("aria-label", "Keyboard shortcut"),
          /* Stop the press here. The browser focuses this input natively on
             mousedown, but the projector wrapper and the enclosing cell both
             handle pointerdown too — moving the caret and pulling focus onto
             the cell. No Prevent_default: that would suppress the native
             focus we are relying on. */
          Attr.on_pointerdown(_ => Effect.Stop_propagation),
          /* Stop_propagation is load-bearing: Page.re attaches a page-level
             on_focus that calls JsUtil.focus_clipboard_shim(), and focus
             events bubble — so without this, focusing here is immediately
             redirected to the clipboard shim and recording never starts. */
          Attr.on_focus(_ =>
            Effect.Many([local(StartRecording), Effect.Stop_propagation])
          ),
          Attr.on_blur(_ =>
            Effect.Many([local(CommitRecording), Effect.Stop_propagation])
          ),
          Attr.on_keydown(key_handler(model, info, ~local, ~parent)),
        ],
        (),
      );
    ProjectorBase.View.mk(
      Node.div(
        ~attrs=[
          Attr.classes([
            "kb-widget",
            ...recording ? ["keybinding-recording"] : [],
          ]),
          Attr.title(
            recording
              ? "Press a shortcut. Esc cancels; click away to keep it."
              : "Click to set a shortcut",
          ),
        ],
        caps @ clear @ [capture],
      ),
    );
  };
};
