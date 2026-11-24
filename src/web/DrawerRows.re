/* Steppers and proofs reserve at least the rows they rendered, which
   catches what a count misses: no drawer runs into the code. A drawer's
   height is read once it first shows and again when AfterLayout's observer
   reports it changed; the rows of every drawer on the page go to the
   model (Globals.drawer_rows) whenever they differ from what it has. What
   stays here is the DOM bookkeeping: the elements watched. */

open Js_of_ocaml;
open Util;

/* each drawer seen, with its id and rows as last read */
let seen:
  ref(list((Js.t(Dom_html.element), option((Haz3lcore.Id.t, int))))) =
  ref([]);
let row_height = ref(0.);
/* the model's rows as of the last frame, and how to send it new ones */
let known: ref(Haz3lcore.Id.Map.t(int)) = ref(Haz3lcore.Id.Map.empty);
let report: ref(Haz3lcore.Id.Map.t(int) => unit) = ref(_ => ());
/* a drawer's rows changed in this frame */
let resized = ref(false);

let current = (): Haz3lcore.Id.Map.t(int) =>
  List.fold(seen^, ~init=Haz3lcore.Id.Map.empty, ~f=(acc, (_, rows)) =>
    switch (rows) {
    | Some((id, n)) =>
      Haz3lcore.Id.Map.update(
        id,
        fun
        | Some(m) => Some(max(m, n))
        | None => Some(n),
        acc,
      )
    | None => acc
    }
  );

/* the rows to send, when they differ from the model's */
let changed = (): option(Haz3lcore.Id.Map.t(int)) => {
  let rows = current();
  Haz3lcore.Id.Map.equal(Int.equal, rows, known^)
    ? None
    : {
      known := rows;
      Some(rows);
    };
};

/* a read: the rows [el] takes as rendered */
let rows_of = (el: Js.t(Dom_html.element)): option((Haz3lcore.Id.t, int)) =>
  switch (el##getAttribute(Js.string("data-drawer-id")) |> Js.Opt.to_option) {
  | Some(id) when Float.(row_height^ > 0.) =>
    Haz3lcore.Id.of_string(Js.to_string(id))
    |> Option.map(~f=id => {
         let h = Float.of_int(el##.offsetHeight);
         (id, max(1, Float.iround_up_exn(h /. row_height^ -. 0.05)));
       })
  | _ => None
  };

let measure = (els: list(Js.t(Dom_html.element))): AfterLayout.write => {
  List.iter(els, ~f=el =>
    if (JsUtil.connected(el)) {
      seen :=
        [
          (el, rows_of(el)),
          ...List.filter(seen^, ~f=((el', _)) => !phys_equal(el', el)),
        ];
    }
  );
  let rows = changed();
  resized := resized^ || Option.is_some(rows);
  () => Option.iter(rows, ~f=report^);
};

let watcher = lazy(AfterLayout.watcher(measure));

/* drawers print values at the editor's visible width: #main's, in
   columns from the main code's left edge, less a two-column margin,
   reported whenever #main resizes (sidebars included). #main's visible
   width also goes to the `--main-client-width` CSS var, which a drawer's
   title bar spans */
let width_watched = ref(false);
let watch_width = (~report: int => unit): unit =>
  if (! width_watched^) {
    Option.iter(
      JsUtil.get_elem_by_id_opt("main"),
      ~f=main => {
        width_watched := true;
        let read = _ => {
          let cols =
            switch (
              Dom_html.document##querySelector(Js.string(".code-container"))
              |> Js.Opt.to_option
            ) {
            | None => None
            | Some(code) =>
              let (col_width, _) = JsUtil.font_metrics_from_specimen();
              let px =
                main##getBoundingClientRect##.right
                -.
                code##getBoundingClientRect##.left;
              Float.(col_width > 0.)
                ? Some(max(20, Float.to_int(px /. col_width) - 2)) : None;
            };
          let client = main##.clientWidth;
          () => {
            ignore(
              main##.style##setProperty(
                Js.string("--main-client-width"),
                Js.string(sprintf("%dpx", client)),
                Js.undefined,
              ),
            );
            Option.iter(cols, ~f=report);
          };
        };
        AfterLayout.watch(AfterLayout.watcher(read), main);
      },
    );
  };

/* from after_display. [reveal] runs after layout, if no drawer changed
   rows in the frame */
let update =
    (
      ~row_height as rh: float,
      ~known as k: Haz3lcore.Id.Map.t(int),
      ~report as r: Haz3lcore.Id.Map.t(int) => unit,
      ~reveal: option(unit => unit),
    )
    : unit => {
  /* rows count in rows of the font: a new size re-reads every drawer */
  let remeasure = Float.(rh != row_height^);
  row_height := rh;
  known := k;
  report := r;
  resized := false;
  seen :=
    List.filter(seen^, ~f=((el, _)) =>
      JsUtil.connected(el)
      || {
        AfterLayout.unwatch(el);
        false;
      }
    );
  /* a drawer gone takes its rows with it */
  Option.iter(
    changed(),
    ~f=rows => {
      resized := true;
      r(rows);
    },
  );
  let nodes =
    Dom_html.document##querySelectorAll(Js.string("[data-drawer-id]"));
  let fresh: list(Js.t(Dom_html.element)) =
    List.init(nodes##.length, ~f=i =>
      Js.Opt.to_option(nodes##item(i)) |> Option.map(~f=Js.Unsafe.coerce)
    )
    |> List.filter_opt
    |> List.filter(~f=el =>
         !List.exists(seen^, ~f=((el', _)) => phys_equal(el', el))
       );
  List.iter(fresh, ~f=AfterLayout.watch(Lazy.force(watcher)));
  switch (remeasure ? List.map(seen^, ~f=fst) @ fresh : fresh) {
  | [] => ()
  | els => AfterLayout.schedule(() => measure(els))
  };
  Option.iter(reveal, ~f=reveal =>
    AfterLayout.schedule(() => {
      if (! resized^) {
        reveal();
      };
      () => ();
    })
  );
};
