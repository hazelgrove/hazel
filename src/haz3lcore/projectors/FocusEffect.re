/* Deferred DOM focus for editor and projector elements: the new DOM lands after action
 * processing, so focus is stashed in a ref (not model state, which would
 * loop) and applied from Main.after_display once the DOM is in place. */
open Util;
open Js_of_ocaml;

/* UI history metadata, separate from the program and projector provenance. */
[@deriving (show({with_path: false}), sexp, yojson)]
type snapshot =
  | EditorFocus
  | TextAreaFocus(string, int, int, string);

let input_start: ref(option(snapshot)) = ref(None);
let input_focus: ref(option(snapshot)) = ref(None);

let textarea_snapshot = (area: WebUtil.TextArea.t): snapshot =>
  TextAreaFocus(
    Js.to_string(area##.id),
    area##.selectionStart,
    area##.selectionEnd,
    Js.to_string(Js.Unsafe.get(area, "selectionDirection")),
  );

let before_textarea_input = (id: Id.t): unit =>
  input_start := Some(textarea_snapshot(WebUtil.TextArea.get(Id.cls(id))));

let after_textarea_input = (): unit => {
  input_focus := input_start^;
  input_start := None;
};

let capture_dom = (): option(snapshot) =>
  /* Core/history tests run without a browser document. */
  try(
    switch (Js.Opt.to_option(Dom_html.document##.activeElement)) {
    | Some(el)
        when
          Js.to_bool(
            el##.classList##contains(Js.string("text-projector-input")),
          ) =>
      let area = WebUtil.TextArea.get(Js.to_string(el##.id));
      Some(textarea_snapshot(area));
    | Some(el)
        when Js.to_bool(el##.classList##contains(Js.string("code-editor"))) =>
      Some(EditorFocus)
    | _ => None
    }
  ) {
  | _ => None
  };

let capture = (): option(snapshot) => {
  let before_input = input_focus^;
  input_focus := None;
  switch (before_input) {
  | Some(_) => before_input
  | None => capture_dom()
  };
};

type target =
  | Editor
  | Cell
  | Probe(Id.t)
  | TextArea(string, int, int, string)
  | EditorIfMissing(Id.t);

let scheduled: ref(option(target)) = ref(None);

let schedule = (probe_id: Id.t): unit => {
  scheduled := Some(Probe(probe_id));
};

let schedule_editor = (): unit => {
  scheduled := Some(Editor);
};

/* a sidebar jump moves the model selection but not DOM focus; this restores it */
let schedule_cell = (): unit => {
  scheduled := Some(Cell);
};

let schedule_textarea = (id: Id.t, offset: int): unit => {
  scheduled := Some(TextArea(Id.cls(id), offset, offset, "none"));
};

let restore = (snapshot: option(snapshot)): unit =>
  switch (snapshot) {
  | Some(EditorFocus) => schedule_editor()
  | Some(TextAreaFocus(id, start, finish, direction)) =>
    scheduled := Some(TextArea(id, start, finish, direction))
  | None => ()
  };

/* Undo can remove a focused projector; return keyboard control to its editor. */
let schedule_editor_if_missing = (id: Id.t): unit => {
  scheduled := Some(EditorIfMissing(id));
};

let execute = (): bool =>
  switch (scheduled^) {
  | Some(Editor) =>
    scheduled := None;
    /* focus the editor itself: the caret CSS is gated on
       `.code-editor:focus` */
    JsUtil.focus_active_editor();
    true;
  | Some(Cell) =>
    scheduled := None;
    JsUtil.focus_active_cell();
  | Some(Probe(probe_id)) =>
    scheduled := None;
    let elem_id = Id.cls(probe_id);
    switch (JsUtil.get_elem_by_id_opt(elem_id)) {
    | Some(elem) =>
      elem##focus;
      true;
    | None => false
    };
  | Some(EditorIfMissing(id)) =>
    scheduled := None;
    if (Option.is_none(JsUtil.get_elem_by_id_opt(Id.cls(id)))) {
      JsUtil.focus_active_editor();
      true;
    } else {
      false;
    };
  | Some(TextArea(id, start, finish, direction)) =>
    scheduled := None;
    switch (JsUtil.get_elem_by_id_opt(id)) {
    | Some(elem) =>
      switch (Js.Opt.to_option(Dom_html.CoerceTo.textarea(elem))) {
      | Some(textarea) =>
        textarea##focus;
        textarea##.selectionStart := min(start, textarea##.value##.length);
        textarea##.selectionEnd := min(finish, textarea##.value##.length);
        Js.Unsafe.set(textarea, "selectionDirection", Js.string(direction));
        true;
      | None => false
      }
    | None => false
    };
  | None => false
  };

/* Focus keeper: vdom's keyed reorder drops DOM focus from a moved element
 * under culling churn, so a focused probe goes dark on scroll. Each frame,
 * if focus has fallen to nothing but the remembered .live-offside still
 * exists, re-focus it. Deliberate blurs call `expect_blur` so this doesn't
 * fight them. */

let kept: ref(option(string)) = ref(None);
let blur_expected: ref(bool) = ref(false);
/* A press outside the kept probe since the last frame. */
let pressed_elsewhere: ref(bool) = ref(false);

let expect_blur = (): unit => blur_expected := true;

/* Both a vdom drop and a press on plain text leave focus on #page; only the
   press is deliberate (it may be starting a text selection). Capture phase,
   since projectors stop pointerdown propagation. */
let install_press_tracking = (): unit => {
  let _: Dom_html.event_listener_id =
    Dom_html.addEventListenerWithOptions(
      Dom_html.document,
      Dom_html.Event.make("pointerdown"),
      ~capture=Js._true,
      Dom_html.handler((evt: Js.t(Dom_html.event)) => {
        switch (kept^, Js.Opt.to_option(evt##.target)) {
        | (Some(id), Some(target)) =>
          let inside =
            switch (JsUtil.get_elem_by_id_opt(id)) {
            | Some(el) =>
              Js.to_bool(
                Js.Unsafe.meth_call(
                  el,
                  "contains",
                  [|Js.Unsafe.inject(target)|],
                ),
              )
            | None => false
            };
          pressed_elsewhere := !inside;
        | _ => ()
        };
        Js._true;
      }),
    );
  ();
};

let focus_no_scroll = (elem: Js.t(Dom_html.element)): unit =>
  Js.Unsafe.meth_call(
    elem,
    "focus",
    [|
      Js.Unsafe.inject(
        Js.Unsafe.obj([|("preventScroll", Js.Unsafe.inject(Js._true))|]),
      ),
    |],
  );

/* Text projector views can also be moved by a later evaluation render. Keep
 * the native selection as well as focus when that DOM move drops them. */
let kept_textarea: ref(option(snapshot)) = ref(None);

let remember_textarea = (id: Id.t): unit => {
  let area = WebUtil.TextArea.get(Id.cls(id));
  kept_textarea := Some(textarea_snapshot(area));
};

let keep_textarea_focus = (): unit => {
  let active = Js.Opt.to_option(Dom_html.document##.activeElement);
  switch (active) {
  | Some(el)
      when
        Js.to_bool(
          el##.classList##contains(Js.string("text-projector-input")),
        ) =>
    switch (Js.Opt.to_option(Dom_html.CoerceTo.textarea(el))) {
    | Some(area) => kept_textarea := Some(textarea_snapshot(area))
    | None => ()
    }
  | _ =>
    let fell_to_nothing =
      switch (active) {
      | None => true
      | Some(el) =>
        let id = Js.to_string(el##.id);
        Js.to_string(el##.tagName) == "BODY"
        || id == "page"
        || id == "clipboard-shim";
      };
    switch (kept_textarea^) {
    | Some(TextAreaFocus(id, start, finish, direction))
        when fell_to_nothing && ! blur_expected^ =>
      switch (
        JsUtil.get_elem_by_id_opt(id)
        |> Option.bind(_, el =>
             Js.Opt.to_option(Dom_html.CoerceTo.textarea(el))
           )
      ) {
      | Some(area) =>
        area##focus;
        area##.selectionStart := start;
        area##.selectionEnd := finish;
        Js.Unsafe.set(area, "selectionDirection", Js.string(direction));
      | None => kept_textarea := None
      }
    | _ => kept_textarea := None
    };
  };
};

let keep_focus = (): unit => {
  keep_textarea_focus();
  let active = Js.Opt.to_option(Dom_html.document##.activeElement);
  let is_live_offside = (el: Js.t(Dom_html.element)): bool =>
    Js.to_bool(el##.classList##contains(Js.string("live-offside")));
  switch (active) {
  | Some(el) when is_live_offside(el) =>
    let id = Js.to_string(el##.id);
    kept := id == "" ? None : Some(id);
    blur_expected := false;
  | _ =>
    if (blur_expected^) {
      kept := None;
      blur_expected := false;
    } else {
      switch (kept^) {
      | None => ()
      | Some(id) =>
        let fell_to_nothing =
          switch (active) {
          | None => true
          | Some(el) =>
            Js.to_string(el##.tagName) == "BODY"
            || Js.to_string(el##.id) == "page"
          };
        if (fell_to_nothing && pressed_elsewhere^) {
          kept := None;
        } else if (fell_to_nothing) {
          switch (JsUtil.get_elem_by_id_opt(id)) {
          | Some(elem) => focus_no_scroll(elem)
          | None => kept := None /* probe gone: culled out or deleted */
          };
        } else {
          kept := None; /* something else took focus on purpose */
        };
      };
    }
  };
  pressed_elsewhere := false;
};
