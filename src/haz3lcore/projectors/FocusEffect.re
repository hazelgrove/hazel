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
    Js.to_string(el##.tagName) == "BODY"
    || List.mem(Js.to_string(el##.id), ["page", JsUtil.clipboard_shim_id])
  };

/* the remembered probe, when the keeper puts the focus back on it */
let returning_probe = (): option(Js.t(Dom_html.element)) =>
  switch (kept^) {
  | Some(id) when ! blur_expected^ && fell_to_nothing(active_element()) =>
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
    /* focus the editor itself, not the clipboard shim: the caret CSS
       is gated on `.code-editor:focus` */
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
      /* a deliberate blur, the probe gone (culled out or deleted), or
         something else took focus on purpose */
      kept := None;
      blur_expected := false;
    }
  };
};
