/* Deferred DOM focus for probe elements: the new DOM lands after action
 * processing, so focus is stashed in a ref (not model state, which would
 * loop) and applied from Main.after_display once the DOM is in place. */
open Util;
open Js_of_ocaml;

type target =
  | Editor
  | Cell
  | CellTop /* Cell + align its stack entry to the viewport top */
  | CellCaretTop /* Cell + bring a caret out of view near the top */
  /* Cell if the keys are lost: undo and redo swap the model under them,
     and keys held outside the code stay there */
  | CellIfLost
  | Probe(Id.t);

let scheduled: ref(option(target)) = ref(None);

let schedule = (probe_id: Id.t): unit => {
  scheduled := Some(Probe(probe_id));
};

let schedule_editor = (): unit => {
  scheduled := Some(Editor);
};

/* don't downgrade a pending CellTop: several actions in one frame may
   each request focus */
let schedule_below_top = (target: target): unit =>
  switch (scheduled^) {
  | Some(CellTop) => ()
  | _ => scheduled := Some(target)
  };

/* a sidebar jump moves the model selection but not DOM focus; this restores it */
let schedule_cell = (): unit => schedule_below_top(Cell);

let schedule_cell_top = (): unit => {
  scheduled := Some(CellTop);
};

/* a jump: as schedule_cell, and a caret out of view comes near the top */
let schedule_cell_caret_top = (): unit => schedule_below_top(CellCaretTop);

let schedule_cell_if_lost = (): unit => schedule_below_top(CellIfLost);

/* Focus keeper: vdom's keyed reorder drops DOM focus from a moved element
 * under culling churn, so a focused probe goes dark on scroll. Each frame,
 * if focus has fallen to nothing but the remembered .live-offside still
 * exists, re-focus it. Deliberate blurs call `expect_blur` so this doesn't
 * fight them. */

let kept: ref(option(string)) = ref(None);
let blur_expected: ref(bool) = ref(false);

let expect_blur = (): unit => blur_expected := true;

/* A press outside the kept probe, or outside the kept splice, since the last
   frame (dev's #2649: text outside the editors is selectable, and a press on
   it, which may be starting a selection, leaves focus on #page, as a vdom
   drop does; only the press is deliberate, so the keepers let it be). */
let pressed_elsewhere: ref(bool) = ref(false);
let pressed_outside_splice: ref(bool) = ref(false);
let kept_splice: ref(option(string)) = ref(None);

let contains = (id: string, target: Js.t(Dom_html.element)): bool =>
  switch (JsUtil.get_elem_by_id_opt(id)) {
  | Some(el) =>
    Js.to_bool(
      Js.Unsafe.meth_call(el, "contains", [|Js.Unsafe.inject(target)|]),
    )
  | None => false
  };

/* Capture phase, since projectors stop pointerdown propagation. */
let install_press_tracking = (): unit => {
  let _: Dom_html.event_listener_id =
    Dom_html.addEventListenerWithOptions(
      Dom_html.document,
      Dom_html.Event.make("pointerdown"),
      ~capture=Js._true,
      Dom_html.handler((evt: Js.t(Dom_html.event)) => {
        switch (Js.Opt.to_option(evt##.target)) {
        | Some(target) =>
          switch (kept^) {
          | Some(id) => pressed_elsewhere := !contains(id, target)
          | None => ()
          };
          switch (kept_splice^) {
          | Some(id) => pressed_outside_splice := !contains(id, target)
          | None => ()
          };
        | None => ()
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

let active_element = (): option(Js.t(Dom_html.element)) =>
  Js.Opt.to_option(Dom_html.document##.activeElement);

/* focus fell to nothing: the body, or the page's own fallbacks */
let fell_to_nothing = (active: option(Js.t(Dom_html.element))): bool =>
  switch (active) {
  | None => true
  | Some(el) =>
    Js.to_string(el##.tagName) == "BODY" || Js.to_string(el##.id) == "page"
  };

/* the remembered probe, when the keeper puts the focus back on it */
let returning_probe = (): option(Js.t(Dom_html.element)) =>
  switch (kept^) {
  | Some(id)
      when
        ! blur_expected^
        && ! pressed_elsewhere^
        && fell_to_nothing(active_element()) =>
    JsUtil.get_elem_by_id_opt(id)
  | _ => None
  };

/* on a code pane the model doesn't select (the vdom can hand a focused
   pane's element to another), or on nothing with no probe to take it */
let keys_lost = (): bool => {
  let active = active_element();
  switch (active) {
  | Some(el) when !fell_to_nothing(active) =>
    Js.to_string(el##.id) != JsUtil.active_cell_id
    && Js.to_bool(
         Js.Unsafe.coerce(el)##matches(Js.string("#main .code-editor")),
       )
  | _ => Option.is_none(returning_probe())
  };
};

let execute = (): bool => {
  let target = scheduled^;
  scheduled := None;
  /* a jump from the outline keeps the keys there */
  let focus_cell = () =>
    JsUtil.outline_has_focus() || JsUtil.focus_active_cell();
  switch (target) {
  | Some(Editor) =>
    /* focus the editor itself: the caret CSS is gated on
       `.code-editor:focus` */
    JsUtil.focus_active_editor();
    true;
  | Some(Cell) => focus_cell()
  | Some(CellCaretTop) =>
    let focused = focus_cell();
    JsUtil.align_caret_near_top();
    focused;
  | Some(CellTop) =>
    let focused = focus_cell();
    JsUtil.align_active_cell_top();
    focused;
  | Some(CellIfLost) => keys_lost() && JsUtil.focus_active_cell()
  | Some(Probe(probe_id)) =>
    switch (JsUtil.get_elem_by_id_opt(Id.cls(probe_id))) {
    | Some(elem) =>
      elem##focus;
      true;
    | None => false
    }
  | None => false
  };
};

let keep_focus = (): unit => {
  let is_live_offside = (el: Js.t(Dom_html.element)): bool =>
    Js.to_bool(el##.classList##contains(Js.string("live-offside")));
  switch (active_element()) {
  | Some(el) when is_live_offside(el) =>
    let id = Js.to_string(el##.id);
    kept := id == "" ? None : Some(id);
    blur_expected := false;
  | _ =>
    switch (returning_probe()) {
    | Some(elem) => focus_no_scroll(elem)
    | None =>
      /* a deliberate blur or press elsewhere, the probe gone (culled out
         or deleted), or something else took focus on purpose */
      kept := None;
      blur_expected := false;
    }
  };
  pressed_elsewhere := false;
};

/* --- Splice focus -----------------------------------------------------
 *
 * Same problem as keep_focus above, different trigger. Typing inside a
 * splice edits the program, so the code view re-renders and the LINE
 * holding the projector is rebuilt -- which detaches the splice's
 * sub-editor and drops DOM focus to BODY. Measured: focus survived 1 of
 * 16 edits, and the one survivor was a keystroke that changed nothing.
 *
 * A vdom key on the splice does not help: keys only match siblings under
 * a surviving parent, and here an ancestor is replaced.
 *
 * Restoring focus to the main editor does NOT work either (verified, when it
 * still went through a clipboard shim: keystrokes then reach the editor but
 * never the splice). Focus has to go back to the splice's own sub-editor
 * element, which is what this does. */

/* The `.splice-editor` wrapper an element sits in, if any. */
let enclosing_splice = (el: Js.t(Dom_html.element)): option(string) => {
  let found =
    Js.Unsafe.meth_call(
      el,
      "closest",
      [|Js.Unsafe.inject(Js.string(".splice-editor"))|],
    );
  switch (Js.Opt.to_option(found)) {
  | None => None
  | Some(w) =>
    let id = Js.to_string(Js.Unsafe.get(w, "id"));
    id == "" ? None : Some(id);
  };
};

/* Focus the sub-editor inside a splice wrapper. The wrapper itself is not
   the focus target -- the inner `.sub-editor` is, which is what a click
   lands on. */
let focus_inside_splice = (wrapper: Js.t(Dom_html.element)): bool => {
  let inner =
    Js.Unsafe.meth_call(
      wrapper,
      "querySelector",
      [|Js.Unsafe.inject(Js.string(".sub-editor"))|],
    );
  let target =
    switch (Js.Opt.to_option(inner)) {
    | Some(el) => el
    | None => wrapper
    };
  focus_no_scroll(target);
  true;
};

/* Called from after_display, beside keep_focus. */
let keep_splice_focus = (): unit => {
  let active = Js.Opt.to_option(Dom_html.document##.activeElement);
  switch (active) {
  | Some(el) when enclosing_splice(el) != None =>
    /* still in a splice: remember which, in case the next render drops it */
    kept_splice := enclosing_splice(el)
  | _ =>
    switch (kept_splice^) {
    | None => ()
    | Some(id) =>
      if (fell_to_nothing(active) && pressed_outside_splice^) {
        kept_splice := None; /* a press elsewhere, deliberate */
      } else if (fell_to_nothing(active)) {
        switch (JsUtil.get_elem_by_id_opt(id)) {
        | Some(w) => ignore(focus_inside_splice(w))
        | None => kept_splice := None /* splice gone: deleted or culled */
        };
      } else {
        kept_splice := None; /* something else took focus on purpose */
      }
    }
  };
  pressed_outside_splice := false;
};
