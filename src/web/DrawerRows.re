/* Steppers and proofs reserve at least the rows they rendered, which
   catches what a count misses: no drawer runs into the code. A drawer's
   height is read once it first shows and again when AfterLayout's observer
   reports it changed; every frame applies the rows known, so a count reset
   on a new stepper (DrawerFit.forget) gets them back. */

open Js_of_ocaml;
open Util;

/* each drawer seen, with its id and rows as last read */
let seen:
  ref(list((Js.t(Dom_html.element), option((Haz3lcore.Id.t, int))))) =
  ref([]);
let row_height = ref(0.);
let relayout: ref(unit => unit) = ref(() => ());
/* a drawer's rows changed in this frame */
let resized = ref(false);

/* true when one changed */
let apply = (): bool =>
  List.fold_left(
    (changed, (_, rows)) =>
      switch (rows) {
      | Some((id, n)) => Haz3lcore.DrawerFit.set_measured(id, n) || changed
      | None => changed
      },
    false,
    seen^,
  );

/* a read: the rows [el] takes as rendered */
let rows_of = (el: Js.t(Dom_html.element)): option((Haz3lcore.Id.t, int)) =>
  switch (el##getAttribute(Js.string("data-drawer-id")) |> Js.Opt.to_option) {
  | Some(id) when row_height^ > 0. =>
    Haz3lcore.Id.of_string(Js.to_string(id))
    |> Option.map(id => {
         let h = float_of_int(el##.offsetHeight);
         (id, max(1, int_of_float(Float.ceil(h /. row_height^ -. 0.05))));
       })
  | _ => None
  };

let measure = (els: list(Js.t(Dom_html.element))): AfterLayout.write => {
  List.iter(
    el =>
      if (JsUtil.connected(el)) {
        seen := [(el, rows_of(el)), ...List.remove_assq(el, seen^)];
      },
    els,
  );
  let changed = apply();
  resized := resized^ || changed;
  () =>
    if (changed) {
      relayout^();
    };
};

let watcher = lazy(AfterLayout.watcher(measure));

/* from after_display. [reveal] runs after layout, if no drawer changed
   rows in the frame */
let update =
    (
      ~row_height as rh: float,
      ~relayout as r: unit => unit,
      ~reveal: option(unit => unit),
    )
    : unit => {
  /* rows count in rows of the font: a new size re-reads every drawer */
  let remeasure = rh != row_height^;
  row_height := rh;
  relayout := r;
  resized := false;
  seen :=
    List.filter(
      ((el, _)) =>
        JsUtil.connected(el)
        || {
          AfterLayout.unwatch(el);
          false;
        },
      seen^,
    );
  if (apply()) {
    resized := true;
    r();
  };
  let nodes =
    Dom_html.document##querySelectorAll(Js.string("[data-drawer-id]"));
  let fresh = ref([]);
  for (i in 0 to nodes##.length - 1) {
    Js.Opt.iter(
      nodes##item(i),
      el => {
        let el: Js.t(Dom_html.element) = Js.Unsafe.coerce(el);
        if (!List.mem_assq(el, seen^)) {
          fresh := [el, ...fresh^];
        };
      },
    );
  };
  List.iter(AfterLayout.watch(Lazy.force(watcher)), fresh^);
  switch (remeasure ? List.map(fst, seen^) @ fresh^ : fresh^) {
  | [] => ()
  | els => AfterLayout.schedule(() => measure(els))
  };
  Option.iter(
    reveal =>
      AfterLayout.schedule(() => {
        if (! resized^) {
          reveal();
        };
        () => ();
      }),
    reveal,
  );
};
