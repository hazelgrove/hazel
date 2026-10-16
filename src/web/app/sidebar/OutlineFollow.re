open Js_of_ocaml;

/* the outline row holding the editor's caret, marked by a style rule
   rather than a class so the memoized outline needn't re-render per caret
   move. it scrolls into view when the mark moves, unless the outline is
   focused: then the keyboard cursor's row does */

let mark: ref(option(Language.Id.t)) = ref(None);
let last_scrolled: ref(option(Language.Id.t)) = ref(None);

let css = (id: Language.Id.t): string =>
  "#outline-sidebar .outline-body:not(:focus-within) .outline-label[data-ol-id=\""
  ++ Language.Id.to_string(id)
  ++ "\"] { background: var(--ol-mark-bg); }";

let scroll_nearest = el =>
  Js.Unsafe.coerce(el)##scrollIntoView(
    Js.Unsafe.obj([|("block", Js.Unsafe.inject(Js.string("nearest")))|]),
  );

let last_cursor: ref(option(string)) = ref(None);

let key_of = el =>
  Js.Opt.case(
    el##getAttribute(Js.string("data-ol-id")),
    () =>
      Js.to_string(el##.textContent |> Js.Opt.get(_, () => Js.string(""))),
    Js.to_string,
  );

let follow_cursor = (): unit => {
  let find = sel =>
    Js.Opt.to_option(
      Dom_html.document##querySelector(
        Js.string("#outline-sidebar .outline-body:focus-within" ++ sel),
      ),
    );
  let follow = (key, scroll) =>
    if (last_cursor^ != Some(key)) {
      let moved = last_cursor^ != None;
      last_cursor := Some(key);
      scroll(moved);
    };
  let cursor = find(" .outline-label.outline-cursor");
  switch (find(" .outline-label.outline-new"), cursor, find("")) {
  /* a new definition being named, below the cursor's row */
  | (Some(el), _, _) =>
    follow("+" ++ Option.fold(~none="", ~some=key_of, cursor), _ =>
      scroll_nearest(el)
    )
  | (None, Some(el), _) => follow(key_of(el), _ => scroll_nearest(el))
  /* up onto the header: the list back to its top */
  | (None, None, Some(body)) =>
    follow("", moved =>
      if (moved) {
        body##.scrollTop := 0;
      }
    )
  | (None, None, None) => last_cursor := None
  };
};

/* text mode's caret shows its item in the editor, the editor's own caret
   left alone: [wanted] until the outline next draws, which finds its
   [spot], then shown after that render */
let wanted: ref(option(Language.Id.t)) = ref(None);

type spot =
  /* lines of the program's editor, or of the nth open cell's body: the
     item's first and last */
  | Lines(option(int), (int, int))
  /* the nth open cell, from its header */
  | Cell(int);

let spot: ref(option(spot)) = ref(None);

let rect = (el: Js.t(Dom_html.element)) => el##getBoundingClientRect;

/* [top, bottom] (viewport px) brought into the view of [el]'s scroll
   container, the least it takes, its top first if it doesn't fit */
let show_span = (el: Js.t(Dom_html.element), top: float, bottom: float) =>
  switch (Util.JsUtil.find_scroll_container(el)) {
  | None => ()
  | Some(sc) =>
    let r = rect(sc);
    let margin = Js.Optdef.get(r##.height, () => 0.) *. 0.1;
    let above = top -. (r##.top +. margin);
    let below = bottom -. (r##.bottom -. margin);
    let by = above < 0. ? above : below > 0. ? min(below, above) : 0.;
    Util.JsUtil.adjust_scroll(sc, by);
  };

let reveal = (s: spot): unit => {
  let all = sel =>
    Dom.list_of_nodeList(
      Dom_html.document##querySelectorAll(Js.string("#main " ++ sel)),
    );
  let nth = (sel, i) => List.nth_opt(all(sel), i);
  let code_in = (el: Js.t(Dom_html.element)) =>
    Js.Opt.to_option(el##querySelector(Js.string(".code-container")));
  let (_, row_h) = Util.JsUtil.font_metrics_from_specimen();
  let lines = ((first, last), code: Js.t(Dom_html.element)) => {
    let top = rect(code)##.top;
    show_span(
      code,
      top +. float_of_int(first) *. row_h,
      top +. float_of_int(last + 1) *. row_h,
    );
  };
  switch (s) {
  | Lines(None, rows) =>
    Option.iter(lines(rows), nth(".code-editor .code-container", 0))
  | Lines(Some(i), rows) =>
    Option.iter(lines(rows), Option.bind(nth(".focus-body", i), code_in))
  | Cell(i) =>
    Option.iter(
      (body: Js.t(Dom_html.element)) => {
        /* its header, and a few of its lines */
        let head =
          Js.Opt.case(
            Js.Unsafe.coerce(body)##.previousElementSibling,
            () => body,
            (h: Js.t(Dom_html.element)) =>
              Js.to_bool(h##.classList##contains(Js.string("focus-header")))
                ? h : body,
          );
        let b = rect(body);
        show_span(
          body,
          rect(head)##.top,
          min(b##.bottom, b##.top +. 4. *. row_h),
        );
      },
      nth(".focus-body", i),
    )
  };
};

let update = (): unit => {
  follow_cursor();
  switch (spot^) {
  | Some(s) =>
    spot := None;
    reveal(s);
  | None => ()
  };
  if (mark^ != last_scrolled^) {
    last_scrolled := mark^;
    switch (mark^) {
    | None => ()
    | Some(id) =>
      let sel =
        Js.string(
          "#outline-sidebar .outline-body:not(:focus-within) [data-ol-id=\""
          ++ Language.Id.to_string(id)
          ++ "\"]",
        );
      Js.Opt.iter(Dom_html.document##querySelector(sel), scroll_nearest);
    };
  };
};
