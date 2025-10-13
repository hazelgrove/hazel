open Util;
open ProjectorBase;
open Virtual_dom.Vdom;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = {
    committed_keybinding: string,
    isRecording: bool,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | StartRecording
    | CommitRecording
    | CancelRecording;

  let string_of = (any: Language.Any.t): option(string) =>
    switch (any) {
    | Exp({term: Atom(String(s)), _}) =>
      Some(StringUtil.unescape_linebreaks(s))
    | _ => None
    };

  let init = (any: Language.Any.t) =>
    switch (string_of(any)) {
    | Some(s) =>
      Some({
        committed_keybinding: s,
        isRecording: false,
      })
    | None => None
    };

  let get = (info: info): string => {
    switch (info.syntax |> info.utility.seg_to_term) {
    | Some(s) =>
      switch (string_of(s)) {
      | Some(s) => s
      | None => failwith("Keybinding: get: Not string literal")
      }
    | None => failwith("Keybinding: get: Not string literal")
    };
  };

  let format_keybinding = (keybinding: string): string =>
    if (keybinding == "") {
      "Click to set";
    } else {
      keybinding;
    };

  let format_key_combination = (key: Key.t): string => {
    let key_name =
      switch (key.key) {
      | D(k) => k
      | U(k) => k
      };

    let mods =
      (key.ctrl == Down ? ["ctrl"] : [])
      @ (key.meta == Down ? [key.sys == Mac ? "cmd" : "meta"] : [])
      @ (key.alt == Down ? ["alt"] : [])
      @ (key.shift == Down ? ["shift"] : []);

    // Ignore modifier-only keybindings
    let key_name =
      switch (key_name) {
      | "Control"
      | "Shift"
      | "Alt"
      | "Meta" => []
      | "ArrowUp" => ["up"]
      | "ArrowDown" => ["down"]
      | "ArrowLeft" => ["left"]
      | "ArrowRight" => ["right"]
      | " " => ["space"]
      | _ => [String.lowercase_ascii(key_name)]
      };

    let keys = mods @ key_name;

    String.concat(" + ", keys);
  };

  let key_handler = (model, info, ~local, ~parent, evt) => {
    open Effect;
    let key = Key.mk(KeyDown, evt);

    switch (key.key) {
    | D("Enter") =>
      /* Commit recording: update model with current syntax value and stop recording */
      Many([
        local(CommitRecording),
        {
          JsUtil.get_elem_by_id(Id.cls(info.id))##blur;
          Stop_propagation;
        },
      ])
    | D("Escape") =>
      /* Cancel recording: revert syntax to committed value and stop recording */
      Many([
        local(CancelRecording),
        parent(
          SetSyntax(
            info.utility.term_to_seg(
              Exp({
                term: Atom(String(model.committed_keybinding)),
                annotation: Language.IdTagged.IdTag.fresh(),
              }),
            ),
          ),
        ),
        {
          JsUtil.get_elem_by_id(Id.cls(info.id))##blur;
          Stop_propagation;
        },
      ])
    | D("Backspace") =>
      /* Clear current keybinding during recording */
      Many([
        parent(
          SetSyntax(
            info.utility.term_to_seg(
              Exp({
                term: Atom(String("")),
                annotation: Language.IdTagged.IdTag.fresh(),
              }),
            ),
          ),
        ),
        Stop_propagation,
      ])
    | D("Tab") =>
      /* Prevent tab from leaving focus during recording */
      Many([Prevent_default, Stop_propagation])
    | _ when String.length(format_key_combination(key)) > 0 =>
      /* Update syntax with pressed key during recording */
      let key_str = format_key_combination(key);
      Many([
        parent(
          SetSyntax(
            info.utility.term_to_seg(
              Exp({
                term: Atom(String(key_str)),
                annotation: Language.IdTagged.IdTag.fresh(),
              }),
            ),
          ),
        ),
        Stop_propagation,
        Prevent_default,
      ]);
    | _ => Stop_propagation
    };
  };

  let focusable = Focusable.non;
  let dynamics = false;

  let placeholder = (model, info) => {
    /* Show what's currently displayed in the view */
    let current_display = info |> get;
    let display_text =
      if (model.isRecording) {
        if (current_display == "") {
          "Recording...";
        } else {
          current_display ++ " ●";
        };
      } else {
        format_keybinding(current_display);
      };
    ProjectorCore.Shape.inline(1 + String.length(display_text));
  };

  let update = (model, info, action) => {
    switch (action) {
    | StartRecording => {
        /* Capture current syntax value as committed value */
        committed_keybinding: info |> get,
        isRecording: true,
      }
    | CommitRecording => {
        /* Update model with current syntax value */
        committed_keybinding: info |> get,
        isRecording: false,
      }
    | CancelRecording => {
        /* Just stop recording, model already has the committed value */

        ...model,
        isRecording: false,
      }
    };
  };

  let view = (model, info, ~local, ~parent, ~view_seg as _) => {
    let base_class = "keybinding";
    let recording_class = model.isRecording ? "keybinding-recording" : "";
    let all_classes =
      [base_class, recording_class] |> List.filter(s => s != "");

    /* Get current display value from syntax */
    let current_display = info |> get;

    /* Show different text based on state */
    let display_text =
      if (model.isRecording) {
        if (current_display == "") {
          "Recording...";
        } else {
          current_display ++ " ●";
        };
      } else {
        format_keybinding(current_display);
      };

    ProjectorBase.View.mk(
      Node.div(
        ~attrs=
          [
            Attr.id(Id.cls(info.id)),
            Attr.classes(all_classes),
            Attr.on_click(_ =>
              Effect.Many([local(StartRecording), Effect.Stop_propagation])
            ),
          ]
          @ (
            if (model.isRecording) {
              [
                Attr.on_keydown(key_handler(model, info, ~local, ~parent)),
                Attr.on_focus(_ => Effect.Stop_propagation),
                Attr.on_blur(_
                  /* Cancel recording if focus is lost during recording */
                  => Effect.Many([local(CancelRecording)])),
              ];
            } else {
              [
                Attr.on_focus(_ => Effect.Stop_propagation),
                Attr.on_blur(_ => Effect.Stop_propagation),
              ];
            }
          )
          @ [Attr.tabindex(0)],
        [Node.text(display_text)],
      ),
    );
  };
};
