open Js_of_ocaml;
open Util;

/* Shift-drag on a projector's rendered code to choose how much of it to
 * show. Both a probe's sample values and a type probe's types are rendered
 * through Abbreviate, and what a drag sets is that abbreviation budget.
 *
 * Callers keep their own pointerdown attribute -- ProbeProj's carries pin,
 * focus and stop-propagation concerns that have nothing to do with dragging
 * -- and call `begin_drag` from inside it. What lives here is the part that
 * is the same either way, and that is easy to get subtly wrong: the pointer
 * capture dance, the guard for capture lost without a pointerup, and the
 * search for a budget that renders at the width the cursor is asking for. */

/* At most one drag at a time, so a single ref rather than a per-projector
 * one. Holds the element that captured the pointer. */
let dragging: ref(option(Js.t(Dom_html.element))) = ref(Option.None);

let begin_drag = (e: Js.t(Dom_html.pointerEvent)): unit => {
  let target = e##.currentTarget |> Js.Opt.get(_, _ => failwith("no target"));
  JsUtil.setPointerCapture(target, e##.pointerId);
  dragging := Some(target);
};

let end_drag = (e: Js.t(Dom_html.pointerEvent)): unit => {
  let target = e##.currentTarget |> Js.Opt.get(_, _ => failwith("no target"));
  if (JsUtil.hasPointerCapture(target, e##.pointerId)) {
    JsUtil.releasePointerCapture(target, e##.pointerId);
  };
  dragging := Option.None;
};

/* Cursor position in rows and columns of the `.code` child being dragged
 * over, which is what a budget is denominated in. */
let pos_rel_to_target = (e: Js.t(Dom_html.mouseEvent)): option(Point.t) => {
  open Float;
  let (col_width, row_height) = JsUtil.font_metrics_from_specimen();
  let text_box =
    e##.currentTarget
    |> Js.Opt.to_option
    |> Option.map(JsUtil.get_child_with_class(_, "code"))
    |> Option.join;
  switch (text_box) {
  | None => None
  | Some(text_box) =>
    let x_rel = of_int(e##.clientX) -. text_box##getBoundingClientRect##.left;
    let y_rel = of_int(e##.clientY) -. text_box##getBoundingClientRect##.top;
    Some({
      row: to_int(y_rel /. row_height),
      col: to_int(round(x_rel /. col_width)),
    });
  };
};

/* Width is not linear in budget -- Abbreviate is discrete, and one more
 * unit of budget can add several characters or none -- so the budget that
 * renders closest to `target_width` is found rather than computed. Doubling
 * to find an upper bound, then bisecting. */
let find_best_budget = (width_at: int => int, target_width: int): int => {
  let rec find_upper = (b: int): int =>
    if (b > 500 || width_at(b) > target_width) {
      b;
    } else {
      find_upper(b * 2 + 1);
    };
  let upper = find_upper(max(1, target_width));
  let rec bisect = (lo: int, hi: int): int =>
    if (lo >= hi) {
      lo;
    } else {
      let mid = (lo + hi + 1) / 2;
      if (width_at(mid) <= target_width) {
        bisect(mid, hi);
      } else {
        bisect(lo, mid - 1);
      };
    };
  bisect(target_width, upper);
};

/* Pass as `~measure` when the render width IS the budget, as it is for
 * block layout where the budget is the wrap width. */
let width_is_budget: int => int = b => b;

/* The move half of a drag. `measure` maps a budget to the width that budget
 * renders at; `set` receives the chosen budget. Returns None when this move
 * is not part of a drag, so the caller can decide what to do instead. */
let on_move =
    (~measure: int => int, e: Js.t(Dom_html.mouseEvent)): option(int) => {
  /* buttons > 0 guards a stale drag flag: capture can be lost without a
     pointerup, which would otherwise resize on shift-hover with no button. */
  let buttons: int = Js.Unsafe.get(e, "buttons");
  switch (
    dragging^,
    Js.to_bool(e##.shiftKey) && buttons > 0 ? pos_rel_to_target(e) : None,
  ) {
  | (Some(_), Some(goal)) =>
    Some(find_best_budget(measure, max(1, goal.col)))
  | _ => None
  };
};

/* A measured width as one of the discrete steps the stylesheets size cells
 * by. Kept here with the rest of the length concept: a drag chooses a
 * length, this is how the length reaches CSS. */
let length_cls = (length: int): string =>
  if (length > 10) {
    "extra";
  } else if (length > 4) {
    "s" ++ string_of_int(length - 4);
  } else {
    "s0";
  };
