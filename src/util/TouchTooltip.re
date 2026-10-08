open Js_of_ocaml;

/* Touch screens have no hover, so `title` tooltips never show there. This
 * shows them on a long press instead (see LongPress): holding a finger on
 * any element with a `title` (or inside one) opens a bubble with its text,
 * which the next touch anywhere dismisses. One document-level listener,
 * installed at startup, serves every `title` in the page with no change
 * at the sites; a long press an element handles itself takes precedence.
 *
 * The bubble is a manual popover where the browser supports it, so it sits
 * in the top layer, over a modal action sheet too. */

/* Gap between the bubble and the element it describes. */
let gap_px = 8.0;

let bubble_id = "touch-tooltip";

let titled_ancestor =
    (target: Js.Opt.t(Js.t(Dom_html.element)))
    : option(Js.t(Dom_html.element)) =>
  switch (Js.Opt.to_option(target)) {
  | None => None
  | Some(t) =>
    Js.Opt.to_option(
      Js.Unsafe.meth_call(
        t,
        "closest",
        [|Js.Unsafe.inject(Js.string("[title]:not([title=''])"))|],
      ),
    )
  };

let bubble = (): Js.t(Dom_html.element) =>
  switch (Dom_html.getElementById_opt(bubble_id)) {
  | Some(el) => el
  | None =>
    let el = Dom_html.createDiv(Dom_html.document);
    el##.id := Js.string(bubble_id);
    el##setAttribute(Js.string("role"), Js.string("tooltip"));
    el##setAttribute(Js.string("popover"), Js.string("manual"));
    Dom.appendChild(Dom_html.document##.body, el);
    el;
  };

let supports_popover = (el: Js.t(Dom_html.element)): bool =>
  Js.Optdef.test(Js.Unsafe.get(el, "showPopover"));

let hide = () =>
  switch (Dom_html.getElementById_opt(bubble_id)) {
  | Some(el) when Js.to_bool(el##.classList##contains(Js.string("shown"))) =>
    el##.classList##remove(Js.string("shown"));
    if (supports_popover(el)) {
      Js.Unsafe.meth_call(el, "hidePopover", [||]);
    };
  | _ => ()
  };

/* Above the element, or below it when there isn't room, kept on screen. */
let place = (el: Js.t(Dom_html.element), anchor: Js.t(Dom_html.element)) => {
  let rect = anchor##getBoundingClientRect;
  let width = float_of_int(el##.offsetWidth);
  let height = float_of_int(el##.offsetHeight);
  let viewport_width =
    float_of_int(Dom_html.document##.documentElement##.clientWidth);
  let centered = (rect##.left +. rect##.right -. width) /. 2.0;
  let left =
    Float.max(
      gap_px,
      Float.min(centered, viewport_width -. width -. gap_px),
    );
  let above = rect##.top -. gap_px -. height;
  let top = above >= gap_px ? above : rect##.bottom +. gap_px;
  el##.style##.left := Js.string(Printf.sprintf("%fpx", left));
  el##.style##.top := Js.string(Printf.sprintf("%fpx", top));
};

let show = (anchor: Js.t(Dom_html.element), text: string) => {
  let el = bubble();
  el##.textContent := Js.some(Js.string(text));
  el##.classList##add(Js.string("shown"));
  if (supports_popover(el)) {
    Js.Unsafe.meth_call(el, "showPopover", [||]);
  };
  place(el, anchor);
};

let on_pointerdown = (evt: Js.t(Dom_html.pointerEvent)) => {
  hide();
  switch (titled_ancestor(evt##.target)) {
  | Some(anchor) when LongPress.is_touch(evt) =>
    LongPress.arm(evt, () => show(anchor, Js.to_string(anchor##.title)))
  | _ => ()
  };
};

let install = (): unit => {
  let listen = (name: string, handler: Js.t('e) => unit) =>
    ignore(
      Dom_html.addEventListener(
        Dom_html.document,
        Dom.Event.make(name),
        Dom_html.handler(evt => {
          handler(evt);
          Js._true;
        }),
        Js._true,
      ),
    );
  listen("pointerdown", on_pointerdown);
  listen("scroll", _ => hide());
};
