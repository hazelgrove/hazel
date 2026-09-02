/* Deferred DOM focus for probe elements: the new DOM lands after action
 * processing, so focus is stashed in a ref (not model state, which would
 * loop) and applied from Main.after_display once the DOM is in place. */
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

/* a sidebar jump moves the model selection but not DOM focus; this restores it */
let schedule_cell = (): unit => {
  scheduled := Some(Cell);
};

let execute = (): bool =>
  switch (scheduled^) {
  | Some(Editor) =>
    scheduled := None;
    /* focus the editor itself, not the clipboard shim: the caret CSS
       is gated on `.code-editor:focus` */
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

/* Focus keeper: vdom's keyed reorder drops DOM focus from a moved element
 * under culling churn, so a focused probe goes dark on scroll. Each frame,
 * if focus has fallen to nothing but the remembered .live-offside still
 * exists, re-focus it. Deliberate blurs call `expect_blur` so this doesn't
 * fight them. */

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
