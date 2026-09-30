open Js_of_ocaml;

/* The outline row holding the editor's caret. Marked by a style rule
   (see Page) rather than a class, so the memoized outline needn't
   re-render on every caret move; after display the row scrolls into
   view (nearest) when the mark moves and the outline isn't focused. */

let mark: ref(option(Language.Id.t)) = ref(None);
let last_scrolled: ref(option(Language.Id.t)) = ref(None);

let css = (id: Language.Id.t): string =>
  "#outline-sidebar .outline-body:not(:focus-within) .outline-label[data-ol-id=\""
  ++ Language.Id.to_string(id)
  ++ "\"] { box-shadow: inset 2px 0 0 var(--ol-mark); }";

let update = (): unit =>
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
      Js.Opt.iter(Dom_html.document##querySelector(sel), el =>
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
        )
      );
    };
  };
