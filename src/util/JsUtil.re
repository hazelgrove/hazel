open Js_of_ocaml;
open Virtual_dom.Vdom;
open Js_of_ocaml.Url;

let get_elem_by_id = id => {
  let doc = Dom_html.document;
  Js.Opt.get(doc##getElementById(Js.string(id)), () => {assert(false)});
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

/* The id carried by whichever code-editor cell is currently the active
   (model-selected) one. Used to move DOM focus to a cell after a sidebar
   jump, so the editor receives keystrokes and the caret (gated on :focus)
   shows there. */
let active_cell_id = "active-code-editor";

/* Focus the active cell without scrolling it into view — scroll is handled
   separately (scroll_cursor_into_view_if_needed), and the browser's default
   focus scroll would fight it. */
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

let rec find_scroll_container_node =
        (node: Js.t(Dom.node)): option(Js.t(Dom_html.element)) =>
  switch (Js.Opt.to_option(node##.parentNode)) {
  | None => None
  | Some(parent_node) =>
    switch (Dom_html.CoerceTo.element(parent_node) |> Js.Opt.to_option) {
    | Some(parent_element) =>
      let scroll_height = parent_element##.scrollHeight;
      let client_height = parent_element##.clientHeight;
      if (scroll_height - client_height > 1) {
        Some(parent_element);
      } else {
        find_scroll_container_node(parent_node);
      };
    | None => find_scroll_container_node(parent_node)
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
  let rect_prop = (el, prop): float =>
    Js.Unsafe.get(
      Js.Unsafe.meth_call(el, "getBoundingClientRect", [||]),
      prop,
    );
  switch (
    Js.Opt.to_option(
      Dom_html.document##querySelector(
        Js.string(".code-container.cull-scope"),
      ),
    )
  ) {
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

let scroll_cursor_into_view_if_needed = () =>
  try({
    let caret_elem = get_elem_by_id("caret");
    switch (find_scroll_container(caret_elem)) {
    | Some(container) => scroll_vertically_into_view(container, caret_elem)
    | None =>
      caret_elem##scrollIntoView(
        Js.Unsafe.obj([|
          ("block", Js.Unsafe.inject(Js.string("nearest"))),
          ("inline", Js.Unsafe.inject(Js.string("nearest"))),
        |]),
      )
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

/* Publish #main's content width as `--main-scroll-width` (read by the cell
 * width rule). Two passes: reset the var first so the cell's own prior width
 * doesn't inflate the measurement, force layout, then read scrollWidth. */
let update_main_scroll_width = () =>
  Js.Opt.iter(
    Dom_html.document##getElementById(Js.string("main")),
    main => {
      set_css_custom_property("--main-scroll-width", "max-content");
      let _: int = Js.Unsafe.get(main, "offsetWidth");
      let sw: int = Js.Unsafe.get(main, "scrollWidth");
      set_css_custom_property(
        "--main-scroll-width",
        string_of_int(sw) ++ "px",
      );
    },
  );

/* Scroll #main by dy to cancel a layout shift. scrollTop lands on whole px
 * and x.5 rounds up, so the remainder carries into the next call; dropping
 * it left code a pixel low under the focus bar and crept the page across
 * drawer round trips. */
let main_scroll_carry = ref(0.);
let scroll_main_by = (dy: float): unit =>
  Js.Opt.iter(
    Dom_html.document##getElementById(Js.string("main")),
    main => {
      let st: float = Js.Unsafe.get(main, Js.string("scrollTop"));
      let target = st +. dy +. main_scroll_carry^;
      Js.Unsafe.set(main, Js.string("scrollTop"), target);
      let landed: float = Js.Unsafe.get(main, Js.string("scrollTop"));
      /* rounding only; a clamp at either scroll end isn't carried */
      main_scroll_carry :=
        Float.abs(target -. landed) < 1. ? target -. landed : 0.;
    },
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
            scroll_main_by(delta);
          };
        });
      let observer =
        Js.Unsafe.new_obj(
          Js.Unsafe.global##._ResizeObserver,
          [|Js.Unsafe.inject(callback)|],
        );
      /* border-box: the bar's border appears before its height starts to
         animate, and get_height measures the border box */
      Js.Unsafe.meth_call(
        observer,
        "observe",
        [|
          Js.Unsafe.inject(bar),
          Js.Unsafe.inject(
            Js.Unsafe.obj([|
              ("box", Js.Unsafe.inject(Js.string("border-box"))),
            |]),
          ),
        |],
      );
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
