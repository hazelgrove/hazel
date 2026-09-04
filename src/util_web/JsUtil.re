open Util;
open Js_of_ocaml;
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
  let rec loop = (sibling: Js.t(Dom_html.element)) =>
    if (Js.to_bool(sibling##.classList##contains(Js.string(className)))) {
      Some(sibling);
    } else {
      loop(
        Js.Opt.get(sibling##.nextSibling, () => failwith("no sibling"))
        |> Js.Unsafe.coerce,
      );
    };
  loop(
    Js.Opt.get(element##.firstChild, () => failwith("no child"))
    |> Js.Unsafe.coerce,
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

let clipboard_shim_id = "clipboard-shim";

let focus_clipboard_shim = () => get_elem_by_id(clipboard_shim_id)##focus;

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

let copy = (str: string) => {
  focus_clipboard_shim();
  Dom_html.document##execCommand(
    Js.string("selectAll"),
    Js.bool(false),
    Js.Opt.empty,
  );
  Dom_html.document##execCommand(
    Js.string("insertText"),
    Js.bool(false),
    Js.Opt.option(Some(Js.string(str))),
  );
  Dom_html.document##execCommand(
    Js.string("selectAll"),
    Js.bool(false),
    Js.Opt.empty,
  );
};

/** Copy [str] using the hidden textarea shim + [document.execCommand("copy")]. */
let copy_via_shim = (str: string): unit => {
  focus_clipboard_shim();
  Js.Opt.iter(
    Dom_html.document##getElementById(Js.string(clipboard_shim_id)),
    clipboard_shim_el => {
      let clipboard_shim = Js.Unsafe.coerce(clipboard_shim_el);
      clipboard_shim##.value := Js.string(str);
      ignore(clipboard_shim##select);
      ignore(
        Dom_html.document##execCommand(
          Js.string("copy"),
          Js.bool(false),
          Js.Opt.empty,
        ),
      );
    },
  );
};

let show_copy_toast = (): unit => {
  Js.Opt.iter(
    Dom_html.document##getElementById(Js.string("copy-toast")),
    toast => {
      toast##.classList##add(Js.string("show"));
      /* SPIKE (wasm-eval-bench): Js.Unsafe rather than the typed binding --
         js_of_ocaml 6 changed setTimeout's signature from
         (Js.callback, Js.number_t) to (Js.meth_callback, float). */
      ignore(
        Js.Unsafe.meth_call(
          Js.Unsafe.global,
          "setTimeout",
          [|
            Js.Unsafe.inject(
              Js.wrap_callback(() =>
                toast##.classList##remove(Js.string("show"))
              ),
            ),
            Js.Unsafe.inject(2000.0),
          |],
        ),
      );
    },
  );
};

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
    switch (find_scroll_container(Js.Unsafe.coerce(el))) {
    | Some(container) =>
      scroll_vertically_into_view(container, Js.Unsafe.coerce(el))
    | None => ()
    };
    /* Extract the full probe Id from data-probe-id attribute */
    let probe_id_str =
      el##getAttribute(Js.string("data-probe-id")) |> Js.Opt.to_option;
    switch (probe_id_str) {
    | Some(s) => Id.of_string(Js.to_string(s))
    | None => None
    };
  | None => None
  };
};
