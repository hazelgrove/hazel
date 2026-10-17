open Js_of_ocaml;

/* the outline row holding the editor's caret, marked by a style rule
   rather than a class so the memoized outline needn't re-render per caret
   move. it scrolls into view when the mark moves, unless the outline is
   focused: then the keyboard cursor's row does */

let mark: ref(option(OutlineEdit.key)) = ref(None);
let last_scrolled: ref(option(OutlineEdit.key)) = ref(None);

/* the marked row's label, unless the outline is focused: a run's by its
   first test */
let marked = (k: OutlineEdit.key): string =>
  "#outline-sidebar .outline-body:not(:focus-within) "
  ++ (
    switch (k) {
    | Item(id) =>
      ".outline-label[data-ol-id=\"" ++ Language.Id.to_string(id) ++ "\"]"
    | Run(id) =>
      ".outline-label[data-ol-run=\"" ++ Language.Id.to_string(id) ++ "\"]"
    }
  );

let css = (k: OutlineEdit.key): string =>
  marked(k) ++ " { background: var(--ol-mark-bg); }";

let scroll_nearest = el =>
  Js.Unsafe.coerce(el)##scrollIntoView(
    Js.Unsafe.obj([|("block", Js.Unsafe.inject(Js.string("nearest")))|]),
  );

let last_cursor: ref(option(string)) = ref(None);

let key_of = el =>
  Js.Opt.case(
    el##getAttribute(Js.string("data-ol-id")),
    () =>
      Js.Opt.case(
        el##getAttribute(Js.string("data-ol-run")),
        () =>
          Js.to_string(
            el##.textContent |> Js.Opt.get(_, () => Js.string("")),
          ),
        id => "run:" ++ Js.to_string(id),
      ),
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

/* keys typed on a new definition's line before it's in the program
   (they can come in the frame of the Enter that started it): held, with
   the line's key, and typed again once that line is drawn; dropped if it
   never is (the caret moved on, or the Enter was refused) */
let held: ref(option((string, list(Js.Unsafe.any)))) = ref(None);

let hold = (line: string, evt: Js.t(Dom_html.keyboardEvent)): unit => {
  let e = Js.Unsafe.coerce(evt);
  let init =
    Js.Unsafe.obj([|
      ("key", Js.Unsafe.inject(e##.key)),
      ("code", Js.Unsafe.inject(e##.code)),
      ("metaKey", Js.Unsafe.inject(e##.metaKey)),
      ("ctrlKey", Js.Unsafe.inject(e##.ctrlKey)),
      ("altKey", Js.Unsafe.inject(e##.altKey)),
      ("shiftKey", Js.Unsafe.inject(e##.shiftKey)),
      ("bubbles", Js.Unsafe.inject(Js._true)),
      ("cancelable", Js.Unsafe.inject(Js._true)),
    |]);
  held :=
    Some((
      line,
      switch (held^) {
      | Some((l, keys)) when l == line => keys @ [init]
      | _ => [init]
      },
    ));
};

let replay = (): unit =>
  switch (held^) {
  | None => ()
  | Some((line, keys)) =>
    held := None;
    let drawn =
      Js.Opt.to_option(
        Dom_html.document##querySelector(
          Js.string(
            "#outline-sidebar .outline-new[data-line=\"" ++ line ++ "\"]",
          ),
        ),
      );
    let body =
      Js.Opt.to_option(
        Dom_html.document##querySelector(
          Js.string("#outline-sidebar .outline-body"),
        ),
      );
    switch (drawn, body) {
    | (Some(_), Some(body)) =>
      List.iter(
        init =>
          ignore(
            Js.Unsafe.coerce(body)##dispatchEvent(
              Js.Unsafe.new_obj(
                Js.Unsafe.global##._KeyboardEvent,
                [|Js.Unsafe.inject(Js.string("keydown")), init|],
              ),
            ),
          ),
        keys,
      )
    | _ => ()
    };
  };

let update = (): unit => {
  replay();
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
    | Some(k) =>
      Js.Opt.iter(
        Dom_html.document##querySelector(Js.string(marked(k))),
        scroll_nearest,
      )
    };
  };
};
