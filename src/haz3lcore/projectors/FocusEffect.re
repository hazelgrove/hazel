/* Deferred DOM focus restoration for probe elements.
 *
 * The DOM update from a virtual-dom render happens AFTER all the
 * synchronous action processing for a frame. So if an action mutates
 * state that causes a focused DOM node to be unmounted (e.g. drawer-
 * mode toggle moves the focusable .live-offside between the offside
 * and `below` slots), calling `elem.focus()` during action handling
 * targets the about-to-be-removed element and the focus is lost when
 * the render finishes.
 *
 * `schedule` stashes a target in a ref; Main.re's `after_display`
 * hook calls `execute` once the new DOM is in place. We use a ref
 * (not model state) because dispatching actions from after_display
 * would loop.
 *
 * Extracted from ProbePerform so ProbeProj (which needs it for the
 * drawer-mode toggle) can call it without creating a dependency
 * cycle through the projector machinery.
 */
open Util;

type target =
  | Editor
  | Probe(Id.t);

let scheduled: ref(option(target)) = ref(None);

let schedule = (probe_id: Id.t): unit => {
  scheduled := Some(Probe(probe_id));
};

let schedule_editor = (): unit => {
  scheduled := Some(Editor);
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

/* ── Focus keeper ───────────────────────────────────────────────────
 *
 * virtual-dom's keyed child reorder (ORDER patches) implements moves as
 * removeChild + reinsert. The reorder is non-minimal: when one patch
 * both inserts and removes siblings — exactly what viewport-culling
 * churn produces on scroll — it relocates "stable" keyed children too.
 * A moved element silently loses DOM focus in every browser (Firefox
 * fires no focus events at all for it), so a keyboard-focused probe
 * (.live-offside, red sample outline) went dark on a two-line scroll.
 *
 * Keeper protocol, run from Main.after_display each frame:
 *   - while a .live-offside holds focus, remember its DOM id;
 *   - if focus has fallen to a non-target (body / #page / the clipboard
 *     shim — i.e. nothing meaningfully took it) while an element with
 *     that id still exists, re-focus it (preventScroll: the element may
 *     sit in the culling buffer just outside the viewport);
 *   - if focus moved to anything meaningful, or the probe element is
 *     gone (culled/deleted), stand down.
 * Deliberate blurs (the Escape paths in ProbeProj's key handler) call
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
