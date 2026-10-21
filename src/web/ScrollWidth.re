/* Publishes #main's natural scroll width as `--main-scroll-width`, so the
 * cell's background and the drawer stripes stretch under absolutely-
 * positioned probe overlays and drawers, which CSS intrinsic sizing can't
 * see. The property doesn't inherit (cell.css): it is set on each element
 * that reads it, so a change restyles those few, not the page.
 *
 * A measure puts them back at their natural widths, reads scrollWidth once
 * the browser has laid that out (AfterLayout), then sets the value, so it
 * forces no layout. It runs only when an input changes. */

open Js_of_ocaml;
open Haz3lcore;

let prop = Js.string("--main-scroll-width");

type key = (
  Measured.t,
  Id.Map.t(int),
  int, /* ProbeProj.Settings.version */
  Language.Sample.Focus.t,
  FontMetrics.t,
  int, /* #main's width changes */
  option(Globals.VisibleRows.t) /* culling range: changes what renders */,
);

let last_key: ref(option(key)) = ref(None);

/* the published value, for elements the view adds or restyles */
let value: ref(option(string)) = ref(None);
let measuring = ref(false);

/* the elements whose CSS reads the property (cell.css, proj-probe.css) */
let set_all = (main: Js.t(Dom_html.element), v: string): unit => {
  let v = Js.string(v);
  let set = els =>
    for (i in 0 to els##.length - 1) {
      Js.Opt.iter(
        els##item(i),
        (el: Js.t(Dom_html.element)) => {
          let style = Js.Unsafe.coerce(el##.style);
          if (style##getPropertyValue(prop) !== v) {
            style##setProperty(prop, v);
          };
        },
      );
    };
  set(Util.JsUtil.by_class(main, "cell"));
  set(
    Dom_html.document##getElementsByClassName(Js.string("below-wrapper")),
  );
};

/* #main's content width changes: the viewport, the sidebars, zoom, its
   scrollbar. A new #main is a change. */
let width_changes = ref(0);
let watched:
  ref(
    option((Js.t(Dom_html.element), Js.t(ResizeObserver.resizeObserver))),
  ) =
  ref(None);
let watch = (main: Js.t(Dom_html.element)): unit =>
  switch (watched^) {
  | Some((m, _)) when m === main => ()
  | old =>
    Option.iter(((_, o)) => o##disconnect, old);
    let last = ref(-1.);
    let o =
      ResizeObserver.observe(
        ~node=main,
        ~f=
          (entries, _) =>
            for (i in 0 to entries##.length - 1) {
              Js.Optdef.iter(Js.array_get(entries, i), e =>
                Js.Optdef.iter(Js.array_get(e##.contentBoxSize, 0), s =>
                  if (s##.inlineSize != last^) {
                    last :=  s##.inlineSize;
                    incr(width_changes);
                  }
                )
              );
            },
        (),
      );
    watched := Some((main, o));
  };

let update =
    (
      ~measured: Measured.t,
      ~refractor_rows: Id.Map.t(int),
      ~sample_focus: Language.Sample.Focus.t,
      ~font_metrics: FontMetrics.t,
      ~visible_rows: option(Globals.VisibleRows.t),
    )
    : unit =>
  switch (Util.JsUtil.get_elem_by_id_opt("main")) {
  | None => ()
  | Some(main) =>
    watch(main);
    let version = ProbeProj.Settings.version^;
    let stale =
      switch (last_key^) {
      | Some((m, rsm, v, sf, fm, w, vr)) =>
        m !== measured
        || rsm !== refractor_rows
        || v != version
        || sf != sample_focus
        || fm != font_metrics
        || w != width_changes^
        || vr != visible_rows
      | None => true
      };
    if (stale) {
      last_key :=
        Some((
          measured,
          refractor_rows,
          version,
          sample_focus,
          font_metrics,
          width_changes^,
          visible_rows,
        ));
      set_all(main, "max-content");
      if (! measuring^) {
        measuring := true;
        Util.AfterLayout.schedule(() => {
          measuring := false;
          let v = string_of_int(main##.scrollWidth) ++ "px";
          (
            () => {
              value := Some(v);
              set_all(main, v);
            }
          );
        });
      };
    } else if (! measuring^) {
      Option.iter(set_all(main), value^);
    };
  };
