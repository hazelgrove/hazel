open Js_of_ocaml;
open Virtual_dom.Vdom;
open Js_of_ocaml.Url;

let get_elem_by_id = id => {
  let doc = Dom_html.document;
  Js.Opt.get(doc##getElementById(Js.string(id)), () => {assert(false)});
};

let ids_with_prefix = (prefix: string): list(string) =>
  /* headless (node tests): no document, so no elements to animate */
  switch (
    Dom_html.document##querySelectorAll(
      Js.string("[id^=\"" ++ prefix ++ "\"]"),
    )
  ) {
  | exception _ => []
  | nodes =>
    List.init(nodes##.length, i => nodes##item(i))
    |> List.filter_map(n =>
         Js.Opt.case(
           n,
           () => None,
           n => Some(Js.to_string(Js.Unsafe.get(n, "id"))),
         )
       )
  };

let get_elem_by_id_opt = id =>
  switch (get_elem_by_id(id)) {
  | exception _ => None
  | e => Some(e)
  };

let get_elem_by_selector = selector => {
  let doc = Dom_html.document;
  Js.Opt.get(
    doc##querySelector(Js.string(selector)),
    () => {
      print_endline("Selector could not be found: " ++ selector);
      assert(false);
    },
  );
};

let query_selector = (sel: string): option(Js.t(Dom_html.element)) =>
  Js.Opt.to_option(Dom_html.document##querySelector(Js.string(sel)));

let rect_prop = (el, prop): float =>
  Js.Unsafe.get(
    Js.Unsafe.meth_call(el, "getBoundingClientRect", [||]),
    prop,
  );

let get_child_with_class = (element: Js.t(Dom_html.element), className) => {
  let rec loop = (sibling: option(Js.t(Dom_html.element))) =>
    switch (sibling) {
    | None => None
    | Some(s) =>
      if (Js.to_bool(s##.classList##contains(Js.string(className)))) {
        Some(s);
      } else {
        loop(
          Js.Opt.to_option(s##.nextSibling) |> Option.map(Js.Unsafe.coerce),
        );
      }
    };
  loop(
    Js.Opt.to_option(element##.firstChild) |> Option.map(Js.Unsafe.coerce),
  );
};

let date_now = () => {
  [%js new Js.date_now];
};

let timestamp = () => date_now()##valueOf;

let precise_timestamp = () => Js.Unsafe.global##.performance##now()##valueOf;

let print_timestamp = (ts: float): string => {
  let date =
    Js.Unsafe.new_obj(Js.date_fromTimeValue, [|Js.Unsafe.inject(ts)|]);
  let date_str = date##toLocaleString(Js.undefined, Js.undefined);
  date_str;
};

let download_string_file =
    (~filename: string, ~content_type: string, ~contents: string) => {
  let blob = File.blob_from_string(~contentType=content_type, contents);
  let url = Dom_html.window##._URL##createObjectURL(blob);

  let link = Dom_html.createA(Dom_html.document);
  link##.href := url;
  link##setAttribute(Js.string("download"), Js.string(filename));
  link##.onclick := Dom_html.handler(_ => {Js._true});
  link##click;
};

let download_json = (filename, contents): unit =>
  download_string_file(
    ~filename=filename ++ ".json",
    ~content_type="application/json",
    ~contents=contents |> Yojson.Safe.to_string,
  );

let read_file = (file, k) => {
  let reader = [%js new File.fileReader];
  reader##readAsText(file);
  reader##.onload :=
    Dom.handler(_ => {
      let result = reader##.result;
      let option = Js.Opt.to_option(File.CoerceTo.string(result));
      let data = Option.map(Js.to_string, option);
      k(data);
      Js._true;
    });
};

let reset_file_input = (input_id: string): unit => {
  switch (get_elem_by_id_opt(input_id)) {
  | Some(elem) => Js.Unsafe.set(elem, "value", Js.string(""))
  | None => ()
  };
};

let confirm = message => {
  Js.to_bool(Dom_html.window##confirm(Js.string(message)));
};

/* Where keyboard focus rests when nothing more specific holds it: incr_dom
   makes the app root focusable, so page-level key handlers still see keys. */
let focus_page = () =>
  switch (get_elem_by_id_opt("page")) {
  | Some(el) =>
    Js.Unsafe.coerce(el)##focus(
      Js.Unsafe.obj([|("preventScroll", Js.Unsafe.inject(Js._true))|]),
    )
  | None => ()
  };

/* Page text selection vs. presses, in the capture phase (projectors stop
   pointerdown propagation):
   - A press in an editable code editor hands selection over to the editor,
     so drop any page selection (the browser keeps it when the press lands
     on unselectable content).
   - A press that selected text (a drag, a double-click) doesn't also click,
     so selecting a row or section header doesn't jump or toggle.
   - Code views pad lines with U+200B; keep it out of copied text. */
let install_text_selection_guards = (): unit =>
  Js.Unsafe.fun_call(
    Js.Unsafe.pure_js_expr(
      {|(function(){
        var sig = function(s){
          return s.rangeCount ? [s.anchorNode, s.anchorOffset, s.focusNode, s.focusOffset] : [];
        };
        var at_press = [];
        var in_field = function(t){ return t.closest('input, textarea, [contenteditable]'); };
        document.addEventListener('pointerdown', function(e){
          var t = e.target, s = window.getSelection();
          if (t instanceof Element && !in_field(t)
              && t.closest('.code-editor:not(.read-only)') && !s.isCollapsed)
            s.removeAllRanges();
          at_press = sig(s);
        }, true);
        document.addEventListener('click', function(e){
          var t = e.target, s = window.getSelection();
          if (!(t instanceof Element) || in_field(t) || s.isCollapsed
              || !s.containsNode(t, true)) return;
          var now = sig(s);
          if (now.every(function(x, i){ return x === at_press[i]; })) return;
          e.preventDefault();
          e.stopPropagation();
        }, true);
        document.addEventListener('copy', function(e){
          var text = String(window.getSelection());
          if (!e.clipboardData || text.indexOf('\u200b') < 0) return;
          e.clipboardData.setData('text/plain', text.replace(/\u200b/g, ''));
          e.preventDefault();
        }, true);
      })|},
    ),
    [||],
  );

/* The caret is CSS-gated on `.code-editor:focus`, so the .code-editor element
   itself must hold DOM focus. preventScroll: don't fight an in-progress
   jump/scroll. */
let focus_active_editor = () =>
  switch (
    Js.Opt.to_option(
      Dom_html.document##querySelector(Js.string(".code-editor.selected")),
    )
  ) {
  | Some(el) =>
    Js.Unsafe.coerce(el)##focus(
      Js.Unsafe.obj([|("preventScroll", Js.Unsafe.inject(Js._true))|]),
    )
  | None => focus_page()
  };

/* keyboard focus into the outline sidebar (its row list) */
let focus_outline = (): unit =>
  switch (query_selector("#outline-sidebar .outline-body")) {
  | Some(el) => Js.Unsafe.coerce(el)##focus
  | None => ()
  };

/* where a keyboard-opened outline menu anchors: under the cursor's row */
let outline_cursor_anchor = (): option((float, float)) =>
  query_selector("#outline-sidebar .outline-label.outline-cursor")
  |> Option.map(el => {
       let rect = el##getBoundingClientRect;
       (rect##.left +. 16., rect##.bottom);
     });

/* how many outline rows one screenful holds, for PageUp/PageDown */
let outline_page_rows = (): int =>
  switch (
    query_selector("#outline-sidebar .outline-body"),
    query_selector("#outline-sidebar .outline-label"),
  ) {
  | (Some(body), Some(row)) when row##.offsetHeight > 0 =>
    max(1, body##.clientHeight / row##.offsetHeight - 1)
  | _ => 10
  };

/* the outline's row list holds the keys: focus is on it or inside it (a
   name's field). Only where the list takes keys (a deck's: it has a
   tabindex); elsewhere a <summary> focused by a click would keep a jump's
   focus from the editor */
let outline_has_focus = (): bool =>
  switch (Js.Opt.to_option(Dom_html.document##.activeElement)) {
  | Some(el) =>
    Js.Opt.test(
      Js.Unsafe.coerce(el)##closest(
        Js.string("#outline-sidebar .outline-body[tabindex]"),
      ),
    )
  | None => false
  };

/* The id carried by whichever code-editor cell is currently the active
   (model-selected) one. Used to move DOM focus to a cell after a sidebar
   jump, so the editor receives keystrokes and the caret (gated on :focus)
   shows there. */
let active_cell_id = "active-code-editor";

/* Focus the active cell without scrolling it into view — scroll is handled
   separately (CaretReveal), and the browser's default focus scroll would
   fight it. */
let focus_active_cell = (): bool =>
  switch (get_elem_by_id_opt(active_cell_id)) {
  | Some(elem) =>
    let _: unit =
      Js.Unsafe.meth_call(
        elem,
        "focus",
        [|
          Js.Unsafe.obj([|("preventScroll", Js.Unsafe.inject(Js._true))|]),
        |],
      );
    true;
  | None => false
  };

/* scroll the active cell's stack entry to the viewport top (trailing
   slack makes this reachable for the last entry too), targeting the
   entry's header band for a stack body so its name stays visible */
let align_active_cell_top = (): unit =>
  switch (get_elem_by_id_opt(active_cell_id)) {
  | None => ()
  | Some(elem) =>
    let target = {
      let closest = sel =>
        Js.Opt.to_option(
          Js.Unsafe.meth_call(
            elem,
            "closest",
            [|Js.Unsafe.inject(Js.string(sel))|],
          ),
        );
      switch (closest(".focus-body")) {
      | Some(body) =>
        switch (Js.Opt.to_option(body##.previousElementSibling)) {
        | Some(prev) => Some(prev)
        | None => Some(body)
        }
      | None => closest(".focus-header")
      };
    };
    switch (target) {
    | None => ()
    | Some(t) =>
      /* skip when the header is already in view with some body room
         below it: aligning a visible cell to the top is jarring */
      let top = rect_prop(t, "top");
      let vh: float = Js.Unsafe.coerce(Dom_html.window)##.innerHeight;
      let top_chrome = 48.; /* fixed top bar */
      let body_context = 140.; /* ~4 rows of body visible below */
      let visible_enough = top >= top_chrome && top <= vh -. body_context;
      if (!visible_enough) {
        let _: unit =
          Js.Unsafe.meth_call(
            t,
            "scrollIntoView",
            [|
              Js.Unsafe.obj([|
                ("block", Js.Unsafe.inject(Js.string("start"))),
              |]),
            |],
          );
        ();
      };
    };
  };

/* Without the async Clipboard API (insecure contexts), copy through a
   throwaway textarea and hand focus back. */
let copy_text = (str: string): unit =>
  Js.Unsafe.fun_call(
    Js.Unsafe.pure_js_expr(
      {|(function(s){
        if (typeof navigator.clipboard !== 'undefined') {
          navigator.clipboard.writeText(s);
          return;
        }
        var prev = document.activeElement;
        var ta = document.createElement('textarea');
        ta.value = s;
        ta.setAttribute('readonly', '');
        ta.style.cssText = 'position:fixed;top:-100px;opacity:0';
        document.body.appendChild(ta);
        ta.focus({preventScroll: true});
        ta.select();
        try { document.execCommand('copy'); } catch (e) {}
        document.body.removeChild(ta);
        if (prev && prev.focus) prev.focus({preventScroll: true});
      })|},
    ),
    [|Js.Unsafe.inject(Js.string(str))|],
  );

let show_copy_toast = (): unit => {
  Js.Opt.iter(
    Dom_html.document##getElementById(Js.string("copy-toast")),
    toast => {
      toast##.classList##add(Js.string("show"));
      ignore(
        Dom_html.window##setTimeout(
          Js.wrap_callback(() => {
            toast##.classList##remove(Js.string("show"))
          }),
          2000.0,
        ),
      );
    },
  );
};
/* Clipboard access as Effects. Both directions go through the async
   Clipboard API, because the editor's key handlers run with focus on a
   non-editable div and Firefox refuses to dispatch native copy/paste
   events there.

   Defined with Ui_effect.Define1 — the same mechanism Bonsai builds
   Effect.of_deferred_fun from — so callers compose these like any other
   Effect rather than side-effecting on their own and scheduling the
   result by hand. Both must be dispatched from an event handler: the
   Clipboard API only grants access under a user gesture. */
let has_clipboard_api = (): bool =>
  Js.to_bool(
    Js.Unsafe.fun_call(
      Js.Unsafe.pure_js_expr(
        "(function(){return typeof navigator.clipboard !== 'undefined';})",
      ),
      [||],
    ),
  );

module ClipboardHandler = {
  module Action = {
    type t(_) =
      | Read_text: t(string)
      | Write_text(string): t(unit);
  };
  let handle = (type a, action: Action.t(a), ~on_response: a => unit) =>
    switch (action) {
    | Read_text =>
      let cb = Js.wrap_callback(text => on_response(Js.to_string(text)));
      Js.Unsafe.fun_call(
        Js.Unsafe.pure_js_expr(
          "(function(cb){navigator.clipboard.readText().then(cb);})",
        ),
        [|Js.Unsafe.inject(cb)|],
      );
    | Write_text(str) =>
      copy_text(str);
      on_response();
    };
};
module Clipboard = Ui_effect.Define1(ClipboardHandler);

let write_clipboard = (str: string): Effect.t(unit) =>
  Clipboard.inject(Write_text(str));

/* Never completes when the browser has no Clipboard API — there is no
   text to deliver, so there is no Paste to dispatch. */
let read_clipboard = (): Effect.t(string) =>
  has_clipboard_api() ? Clipboard.inject(Read_text) : Effect.never;

/* Maintains an `at-bottom` class on a scroll container from its scroll
 * events. Direct classList mutation, no vdom round-trip; used by the probe
 * drawer's scroll-affordance fade (proj-probe.css). */
let sync_at_bottom_class = (evt: Js.t(Dom_html.event)): unit =>
  switch (Js.Opt.to_option(evt##.currentTarget)) {
  | None => ()
  | Some(el) =>
    /* Tolerance: scrollTop is truncated from a sub-pixel position. */
    let at_bottom =
      el##.scrollTop + el##.clientHeight >= el##.scrollHeight - 2;
    if (at_bottom) {
      el##.classList##add(Js.string("at-bottom"));
    } else {
      el##.classList##remove(Js.string("at-bottom"));
    };
  };
/* Play a short tone via Web Audio: one oscillator through a gain envelope
   (short attack/release so start/stop don't click). One AudioContext is
   created lazily and shared; resume() every call since browsers keep the
   context suspended until the first user gesture — tones fired before any
   gesture (e.g. from a timer) are silently dropped by the browser. */
let play_tone = (~freq: float, ~ms: float): unit =>
  Js.Unsafe.fun_call(
    Js.Unsafe.pure_js_expr(
      "(function(freq, ms){
         var w = window;
         var C = w.AudioContext || w.webkitAudioContext;
         if (!C) return;
         var ctx = w.__hazelAudioCtx || (w.__hazelAudioCtx = new C());
         if (ctx.state === 'suspended') ctx.resume();
         var t = ctx.currentTime;
         var dur = Math.max(ms, 1) / 1000;
         var osc = ctx.createOscillator();
         var gain = ctx.createGain();
         osc.frequency.value = freq;
         gain.gain.setValueAtTime(0, t);
         gain.gain.linearRampToValueAtTime(0.2, t + 0.01);
         gain.gain.setValueAtTime(0.2, t + Math.max(dur - 0.03, 0.01));
         gain.gain.linearRampToValueAtTime(0, t + dur);
         osc.connect(gain);
         gain.connect(ctx.destination);
         osc.start(t);
         osc.stop(t + dur + 0.01);
       })",
    ),
    [|Js.Unsafe.inject(freq), Js.Unsafe.inject(ms)|],
  );

let say = (text: string): unit =>
  Js.Unsafe.fun_call(
    Js.Unsafe.pure_js_expr(
      "(function(s){
         if (typeof speechSynthesis === 'undefined') return;
         speechSynthesis.speak(new SpeechSynthesisUtterance(s));
       })",
    ),
    [|Js.Unsafe.inject(Js.string(text))|],
  );

let element_to_node = (element: Js.t(Dom_html.element)): Js.t(Dom.node) =>
  Js.Unsafe.coerce(element);

/* overflow auto or scroll, with content past its height. Content past
   the height alone isn't enough: a box with visible overflow reports it
   without scrolling (a code container does, by decorations below its
   last row). A pixel's overflow is rounding */
let scrolls_vertically = (el: Js.t(Dom_html.element)): bool => {
  let overflow = el##.scrollHeight - el##.clientHeight;
  overflow > 1
  && {
    let style =
      Js.Unsafe.meth_call(
        Dom_html.window,
        "getComputedStyle",
        [|Js.Unsafe.inject(el)|],
      );
    let oy = Js.to_string(Js.Unsafe.get(style, "overflowY"));
    oy == "auto" || oy == "scroll";
  };
};

let rec find_scroll_container_node =
        (node: Js.t(Dom.node)): option(Js.t(Dom_html.element)) =>
  switch (Js.Opt.to_option(node##.parentNode)) {
  | None => None
  | Some(parent_node) =>
    switch (Dom_html.CoerceTo.element(parent_node) |> Js.Opt.to_option) {
    | Some(parent_element) when scrolls_vertically(parent_element) =>
      Some(parent_element)
    | _ => find_scroll_container_node(parent_node)
    }
  };

let find_scroll_container =
    (element: Js.t(Dom_html.element)): option(Js.t(Dom_html.element)) =>
  find_scroll_container_node(element_to_node(element));

/* Viewport-culling geometry for the active code editor: (scroll_top,
 * client_height), scroll_top measured from the editor's OWN row 0 (feed into
 * VisibleRows.compute) so it's correct however the editor is nested. None if
 * not mounted / no scroll container. Measures the first cell that has not
 * opted out of culling (CodeWithStatics `cull-scope`, CellEditor ~cull). */
let code_viewport_geometry = (): option((float, float)) => {
  switch (query_selector(".code-container.cull-scope")) {
  | None => None
  | Some(code) =>
    switch (find_scroll_container(code)) {
    | None => None
    | Some(container) =>
      let scroll_top =
        Float.max(
          0.,
          rect_prop(container, "top") -. rect_prop(code, "top"),
        );
      Some((scroll_top, rect_prop(container, "height")));
    }
  };
};

/* Find the nearest ancestor element with the given class */
let find_ancestor_with_class =
    (el: Js.t(Dom_html.element), class_name: string)
    : option(Js.t(Dom_html.element)) => {
  let class_js = Js.string(class_name);
  let rec loop = (node: Js.t(Dom.node)): option(Js.t(Dom_html.element)) =>
    switch (Js.Opt.to_option(node##.parentNode)) {
    | None => None
    | Some(parent_node) =>
      switch (Dom_html.CoerceTo.element(parent_node) |> Js.Opt.to_option) {
      | None => loop(parent_node)
      | Some(parent_el) =>
        if (Js.to_bool(parent_el##.classList##contains(class_js))) {
          Some(parent_el);
        } else {
          loop(parent_node);
        }
      }
    };
  loop(element_to_node(el));
};

/* clientHeight forces layout on a dirty tree and scroll handlers run
   every scrolled frame; a container's height changes only on resize,
   which clears this one-element cache */
let client_height_cache: Slot.t(Js.t(Dom_html.element), float) = Slot.mk();
let client_height_listener = ref(false);
let cached_client_height = (el: Js.t(Dom_html.element)): float => {
  if (! client_height_listener^) {
    client_height_listener := true;
    let clear = Js.wrap_callback(_ => client_height_cache := None);
    let _ =
      Js.Unsafe.meth_call(
        Dom_html.window,
        "addEventListener",
        [|Js.Unsafe.inject(Js.string("resize")), Js.Unsafe.inject(clear)|],
      );
    ();
  };
  Slot.get(client_height_cache, el, () => float_of_int(el##.clientHeight));
};

let adjust_scroll = (container: Js.t(Dom_html.element), delta: float) =>
  if (delta != 0.) {
    let current = float_of_int(container##.scrollTop);
    let target = current +. delta;
    container##.scrollTop := int_of_float(target);
  };

/* Scroll vertically so that el_rect is visible within the container,
 * with a 10% margin. Only adjusts scrollTop, never scrollLeft. */
let scroll_vertically_into_view =
    (container: Js.t(Dom_html.element), el: Js.t(Dom_html.element)) => {
  let el_rect = el##getBoundingClientRect;
  let container_rect = container##getBoundingClientRect;
  let margin_ratio = 0.10;
  let margin_px =
    Js.Optdef.get(container_rect##.height, _ => 0.) *. margin_ratio;
  let top_gap = el_rect##.top -. (container_rect##.top +. margin_px);
  if (top_gap < 0.) {
    adjust_scroll(container, top_gap);
  } else {
    let bottom_gap =
      el_rect##.bottom -. (container_rect##.bottom -. margin_px);
    if (bottom_gap > 0.) {
      adjust_scroll(container, bottom_gap);
    };
  };
};

/* Scroll every vertical-scroll ancestor of `el`, not just the nearest: a
 * drawer-mode `.live-offside` sits inside `.below-wrapper` (overflow-y:auto),
 * which would otherwise swallow the scroll and leave #main unmoved. Vertical-
 * only, to avoid the horizontal jumps that motivated dropping scrollIntoView. */
let scroll_vertically_into_view_ancestors =
    (el: Js.t(Dom_html.element)): unit => {
  let rec go = (node: Js.t(Dom.node)): unit =>
    switch (find_scroll_container_node(node)) {
    | None => ()
    | Some(container) =>
      scroll_vertically_into_view(container, el);
      go(element_to_node(container));
    };
  go(element_to_node(el));
};

/* find_scroll_container reads scrollHeight/clientHeight up the parent
   chain, forcing layout on dirty frames after every action; reuse the
   found container while it is connected, still an ancestor and still
   scrolls */
let scroll_container_cache: ref(option(Js.t(Dom_html.element))) =
  ref(None);
let connected = (el: Js.t(Dom_html.element)): bool =>
  try(Js.to_bool(Js.Unsafe.get(el, "isConnected"))) {
  | _ => false
  };
let find_scroll_container_cached =
    (element: Js.t(Dom_html.element)): option(Js.t(Dom_html.element)) => {
  let valid = (el: Js.t(Dom_html.element)): bool =>
    connected(el)
    /* the caret may move to an editor in another scroll container */
    && Js.to_bool(
         Js.Unsafe.meth_call(el, "contains", [|Js.Unsafe.inject(element)|]),
       )
    && scrolls_vertically(el);
  switch (scroll_container_cache^) {
  | Some(el) when valid(el) => Some(el)
  | _ =>
    let found = find_scroll_container(element);
    scroll_container_cache := found;
    found;
  };
};

/* after a jump: a caret out of comfortable view comes to a fifth of the
   way down its scroll container, with the lines above it in sight */
let align_caret_near_top = (): unit =>
  try({
    let caret = get_elem_by_id("caret");
    switch (find_scroll_container(caret)) {
    | Some(container) =>
      let c = caret##getBoundingClientRect;
      let r = container##getBoundingClientRect;
      let h = Js.Optdef.get(r##.height, _ => 0.);
      let top = c##.top -. r##.top;
      if (top < h *. 0.1 || top > h *. 0.75) {
        adjust_scroll(container, top -. h *. 0.2);
      };
    | None => ()
    };
  }) {
  | Assert_failure(_) => ()
  };

/* main editor container scrollTop (read/write) — tutorial per-slide scroll memory */
let main_scroll_top = (): float =>
  try({
    let main = get_elem_by_id("main");
    float_of_int(main##.scrollTop);
  }) {
  | Assert_failure(_) => 0.
  };

let set_main_scroll_top = (top: float) =>
  try({
    let main = get_elem_by_id("main");
    main##.scrollTop := int_of_float(top);
  }) {
  | Assert_failure(_) => ()
  };

/* scroll #main the least that shows the last element matching sel, its
   top first if it's taller than the view; vertical only */
let reveal_last = (sel: string): unit =>
  try({
    let main = get_elem_by_id("main");
    let nodes = Dom_html.document##querySelectorAll(Js.string(sel));
    switch (Js.Opt.to_option(nodes##item(nodes##.length - 1))) {
    | None => ()
    | Some(el) =>
      let r = el##getBoundingClientRect;
      let m = main##getBoundingClientRect;
      let below = r##.bottom -. m##.bottom;
      let above = r##.top -. m##.top;
      let by = below > 0. ? min(below, above) : min(above, 0.);
      if (by != 0.) {
        main##.scrollTop :=  main##.scrollTop + int_of_float(Float.ceil(by));
      };
    };
  }) {
  | Assert_failure(_) => ()
  };

module Fragment = {
  let get_current = () => {
    let fragment_of_url = (url: Url.url): string =>
      switch (url) {
      | Http({hu_fragment: str, _})
      | Https({hu_fragment: str, _})
      | File({fu_fragment: str, _}) => str
      };
    Url.Current.get() |> Option.map(fragment_of_url);
  };
};

let setPointerCapture = (e: Js.t(Dom_html.element), pointerId: int): unit =>
  Js.Unsafe.meth_call(
    e,
    "setPointerCapture",
    [|Js.Unsafe.inject(pointerId)|],
  );

let releasePointerCapture = (e: Js.t(Dom_html.element), pointerId: int) =>
  Js.Unsafe.meth_call(
    e,
    "releasePointerCapture",
    [|Js.Unsafe.inject(pointerId)|],
  );

let hasPointerCapture = (e: Js.t(Dom_html.element), pointerId: int) =>
  Js.Unsafe.meth_call(
    e,
    "hasPointerCapture",
    [|Js.Unsafe.inject(pointerId)|],
  );

let set_css_custom_property = (name: string, value: string): unit =>
  Js.Unsafe.meth_call(
    Dom_html.document##.documentElement##.style,
    "setProperty",
    [|
      Js.Unsafe.inject(Js.string(name)),
      Js.Unsafe.inject(Js.string(value)),
    |],
  );

let delay = (delay: float, callback: unit => unit) => {
  let _ =
    Js_of_ocaml.Dom_html.window##setTimeout(
      Js.wrap_callback(callback),
      delay,
    );
  ();
};

/* a live collection: the browser keeps it, so asking again costs nothing
   while the DOM under [el] is unchanged */
let by_class =
    (el: Js.t(Dom_html.element), cls: string)
    : Js.t(Dom.nodeList(Dom_html.element)) =>
  Js.Unsafe.meth_call(
    el,
    "getElementsByClassName",
    [|Js.Unsafe.inject(Js.string(cls))|],
  );

/* Scroll compensation for sample focus bar:
 * When the bar's height changes (appearing/disappearing), adjust #main's
 * scrollTop so visible code doesn't shift. Only compensates when scrolled
 * down (at scroll 0, the shift is unavoidable).
 *
 * Uses float arithmetic throughout: scrollTop is sub-pixel (especially with
 * trackpad scrolling), and OCaml int ops compile to JS `| 0` which truncates
 * the fractional part, causing visible drift on each toggle. */
let focus_bar_observer_installed = ref(false);
let get_height = el =>
  Js.Unsafe.get(
    Js.Unsafe.meth_call(el, "getBoundingClientRect", [||]),
    "height",
  );
let setup_focus_bar_scroll_compensation = () =>
  if (! focus_bar_observer_installed^) {
    let bar =
      try(Some(get_elem_by_id("sample-focus-bar"))) {
      | _ => None
      };
    let main =
      try(Some(get_elem_by_id("main"))) {
      | _ => None
      };
    switch (bar, main) {
    | (Some(bar_el), Some(main_el)) =>
      focus_bar_observer_installed := true;
      let bar = Js.Unsafe.coerce(bar_el);
      let main = Js.Unsafe.coerce(main_el);
      let last_height: ref(float) = ref(get_height(bar));
      let callback =
        Js.wrap_callback(_entries => {
          let new_height: float = get_height(bar);
          let delta = new_height -. last_height^;
          last_height := new_height;
          let scroll_top: float =
            Js.Unsafe.get(main, Js.string("scrollTop"));
          if (delta != 0.0 && scroll_top > 0.0) {
            Js.Unsafe.set(main, Js.string("scrollTop"), scroll_top +. delta);
          };
        });
      let observer =
        Js.Unsafe.new_obj(
          Js.Unsafe.global##._ResizeObserver,
          [|Js.Unsafe.inject(callback)|],
        );
      Js.Unsafe.meth_call(observer, "observe", [|Js.Unsafe.inject(bar)|]);
    | _ => ()
    };
  };

/* localStorage rather than the IndexedDB store: the color theme has to be
   readable synchronously from an inline <head> script, before the first
   paint, and IndexedDB only opens asynchronously. */
let set_local_storage = (key: string, value: string): unit =>
  try({
    let store =
      Dom_html.window##.localStorage |> Js.Optdef.get(_, () => assert(false));
    store##setItem(Js.string(key), Js.string(value));
  }) {
  | _ => ()
  };

let get_local_storage = (key: string): option(string) =>
  try({
    let store =
      Dom_html.window##.localStorage |> Js.Optdef.get(_, () => assert(false));
    store##getItem(Js.string(key))
    |> (x => Js.Opt.to_option(x) |> Option.map(Js.to_string));
  }) {
  | _ => None
  };

let set_css_variable = (name: string, value: string) => {
  let doc = Dom_html.document;
  let root = doc##.documentElement;
  let style: Js.t(Dom_html.cssStyleDeclaration) = root##.style;

  let _ =
    style##setProperty(Js.string(name), Js.string(value), Js.undefined);
  ();
};
let prompt = (message: string, default: string): option(string) => {
  Js.Opt.to_option(
    Dom_html.window##prompt(Js.string(message), Js.string(default)),
  )
  |> Option.map(Js.to_string);
};

/* Measure actual font metrics from the #font-specimen element.
 * Falls back to 10.0 if the element isn't available. */
let font_metrics_from_specimen = (): (float, float) =>
  switch (get_elem_by_id_opt("font-specimen")) {
  | Some(specimen) =>
    let rect = specimen##getBoundingClientRect;
    let col_width = max(1.0, rect##.right -. rect##.left);
    let row_height = max(1.0, rect##.bottom -. rect##.top);
    (col_width, row_height);
  | None => (10.0, 10.0)
  };

/* Listen for devicePixelRatio changes (triggered by browser zoom).
 * Uses matchMedia to detect when the current DPR no longer matches,
 * then re-registers for the next change. */
let on_dpr_change = (callback: unit => unit): unit => {
  let rec listen = () => {
    let dpr: float =
      Js.Unsafe.get(Dom_html.window, "devicePixelRatio")
      |> Js.float_of_number
      |> Js.to_float;
    let query = Printf.sprintf("(resolution: %fdppx)", dpr);
    let mql =
      Js.Unsafe.meth_call(
        Dom_html.window,
        "matchMedia",
        [|Js.Unsafe.inject(Js.string(query))|],
      );
    let handler =
      Js.wrap_callback((_: Js.t({..})) => {
        callback();
        listen();
      });
    ignore(
      Js.Unsafe.meth_call(
        mql,
        "addEventListener",
        [|
          Js.Unsafe.inject(Js.string("change")),
          Js.Unsafe.inject(handler),
        |],
      ),
    );
  };
  listen();
};

module QueryParams = {
  let get_arguments = (url: Url.url): list((string, string)) =>
    switch (url) {
    | Http({hu_arguments, _}) => hu_arguments
    | Https({hu_arguments, _}) => hu_arguments
    | File({fu_arguments, _}) => fu_arguments
    };

  let set_arguments = (url: Url.url, args: list((string, string))): Url.url =>
    switch (url) {
    | Http(u) =>
      Http({
        ...u,
        hu_arguments: args,
      })
    | Https(u) =>
      Https({
        ...u,
        hu_arguments: args,
      })
    | File(u) =>
      File({
        ...u,
        fu_arguments: args,
      })
    };

  let get_param = (name: string): option(string) => {
    let q_opt =
      Url.Current.get()
      |> Option.map(url =>
           url |> get_arguments |> List.find_opt(((k, _)) => k == name)
         );
    switch (q_opt) {
    | Some(Some((_, v))) => Some(v)
    | _ => None
    };
  };

  let set_param = (name: string, value: string) => {
    Url.Current.get()
    |> Option.iter(url => {
         let args =
           get_arguments(url)
           |> List.filter(((k, _)) => k != name)
           |> List.cons((name, value));

         let new_url = set_arguments(url, args);
         let href = Url.string_of_url(new_url);

         Dom_html.window##.history##pushState(
           Js.null,
           Js.string(""),
           Js.some(Js.string(href)),
         );
       });
  };
};

/* Navigate between probe elements in document order.
   Finds all .live-offside[tabindex] elements, sorts by visual position,
   and focuses the next/previous one relative to current_id.
   When ~skip_unaligned is true, skips probes whose data-cursor-aligned
   attribute is not "true" (i.e. probes with no samples related to
   the current cursor).
   Returns the target probe's Id.t (from data-probe-id attribute)
   and gives it DOM focus. */
let navigate_probes =
    (
      ~skip_unaligned: bool=false,
      current_id: string,
      direction: [
        | `Up
        | `Down
      ],
    )
    : option(Id.t) => {
  let elements =
    Dom_html.document##querySelectorAll(
      Js.string(".live-offside[tabindex]"),
    );
  let len = elements##.length;
  /* Collect elements with their bounding rects */
  let items = ref([]);
  for (i in 0 to len - 1) {
    switch (elements##item(i) |> Js.Opt.to_option) {
    | Some(el) =>
      let el = Js.Unsafe.coerce(el);
      let rect = el##getBoundingClientRect;
      items := [(el, rect##.top, rect##.left), ...items^];
    | None => ()
    };
  };
  /* Sort by top, then left */
  let sorted =
    List.sort(
      ((_, t1, l1), (_, t2, l2)) => {
        let c = compare(t1, t2);
        if (c != 0) {
          c;
        } else {
          compare(l1, l2);
        };
      },
      items^,
    );
  /* Find current index */
  let current_idx = ref(-1);
  List.iteri(
    (i, (el, _, _)) => {
      let id: string = Js.to_string(el##.id);
      if (id == current_id) {
        current_idx := i;
      };
    },
    sorted,
  );
  /* Find target, optionally skipping unaligned probes */
  let offset =
    switch (direction) {
    | `Down => 1
    | `Up => (-1)
    };
  let n = List.length(sorted);
  let rec find_target = idx =>
    if (idx < 0 || idx >= n) {
      None;
    } else {
      let (el, _, _) = List.nth(sorted, idx);
      let dominated =
        skip_unaligned
        && {
          let attr =
            el##getAttribute(Js.string("data-cursor-aligned"))
            |> Js.Opt.to_option;
          switch (attr) {
          | Some(s) => Js.to_string(s) != "true"
          | None => true
          };
        };
      dominated ? find_target(idx + offset) : Some(el);
    };
  switch (find_target(current_idx^ + offset)) {
  | Some(el) =>
    el##focus(
      Js.Unsafe.obj([|("preventScroll", Js.Unsafe.inject(Js._true))|]),
    );
    scroll_vertically_into_view_ancestors(Js.Unsafe.coerce(el));
    let probe_id_str =
      el##getAttribute(Js.string("data-probe-id")) |> Js.Opt.to_option;
    switch (probe_id_str) {
    | Some(s) => Id.of_string(Js.to_string(s))
    | None => None
    };
  | None => None
  };
};

/* Drawers print values at the editor's visible width: once #main exists,
   report its width in columns (from the main code's left edge, less a
   two-column margin) whenever it resizes, sidebars included. */
let width_observed = ref(false);
let observe_drawer_width = (~report: int => unit): unit =>
  if (! width_observed^) {
    switch (get_elem_by_id_opt("main")) {
    | None => ()
    | Some(main) =>
      width_observed := true;
      let _ =
        ResizeObserver.observe(
          ~node=main,
          ~f=
            (_, _) =>
              switch (query_selector(".code-container")) {
              | None => ()
              | Some(code) =>
                let (col_width, _) = font_metrics_from_specimen();
                if (col_width > 0.) {
                  let px =
                    rect_prop(main, "right") -. rect_prop(code, "left");
                  report(max(20, int_of_float(px /. col_width) - 2));
                };
              },
          (),
        );
      ();
    };
  };
