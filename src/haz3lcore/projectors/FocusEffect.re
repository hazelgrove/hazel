/* Deferred DOM focus restoration for probe elements. The DOM update lands
 * AFTER a frame's action processing, so focusing during action handling
 * targets an about-to-be-unmounted node. `schedule` stashes a target in a
 * ref (not model state, which would loop); Main.after_display calls
 * `execute` once the new DOM is in place. */
open Util;

type target =
  | Editor
  | Cell
  | Probe(Id.t);

let scheduled: ref(option(target)) = ref(None);

let schedule = (probe_id: Id.t): unit => {
  scheduled := Some(Probe(probe_id));
};

let schedule_editor = (): unit => {
  scheduled := Some(Editor);
};

/* Schedule DOM focus on the active code-editor cell (called after a
   sidebar jump, which moves the model selection to a different cell
   without moving DOM focus). */
let schedule_cell = (): unit => {
  scheduled := Some(Cell);
};

let execute = (): bool =>
  switch (scheduled^) {
  | Some(Editor) =>
    scheduled := None;
    /* Focus the editor element itself (not the page-level clipboard
       shim) so the caret is restored — the shim only captures keys,
       it does not satisfy the `.code-editor:focus` caret CSS. */
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
  | None => false
  };

/* Focus keeper. vdom's keyed reorder (removeChild + reinsert) silently
 * drops DOM focus from a moved element under viewport-culling churn, so a
 * keyboard-focused probe goes dark on scroll. Run each frame from
 * after_display: remember a focused .live-offside's id, and if focus has
 * since fallen to nothing (body/#page/clipboard-shim) while that element
 * still exists, re-focus it (preventScroll). Deliberate blurs call
 * `expect_blur` first so the keeper doesn't fight them. */

open Js_of_ocaml;

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

let keep_focus = (): unit => {
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
            let tag = Js.to_string(el##.tagName);
            let eid = Js.to_string(el##.id);
            tag == "BODY" || eid == "page" || eid == "clipboard-shim";
          };
        if (fell_to_nothing) {
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
};
