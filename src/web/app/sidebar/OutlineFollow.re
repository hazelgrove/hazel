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
  ++ "\"] { box-shadow: inset 2px 0 0 var(--ol-mark); }";

let scroll_nearest = el =>
  Js.Unsafe.meth_call(
    el,
    "scrollIntoView",
    [|
      Js.Unsafe.inject(
        Js.Unsafe.obj([|
          ("block", Js.Unsafe.inject(Js.string("nearest"))),
        |]),
      ),
    |],
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
        Js.string("#outline-sidebar .outline-body:focus" ++ sel),
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

let update = (): unit => {
  follow_cursor();
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
