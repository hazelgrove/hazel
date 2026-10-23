open Util;
open Js_of_ocaml;

/* A proof drag appends a row instead of replacing its expression. Animate
   that actual new row from the preview's place, revealing the retained
   original underneath. No cloned editors, temporary proof steps or ids. */
type request = {
  source: Js.t(Dom_html.element),
  mutable accepted: bool,
};

type flight = {
  source: Js.t(Dom_html.element),
  target: Js.t(Dom_html.element),
  animations: list(Js.Unsafe.any),
};

let pending: ref(option(request)) = ref(None);
let active: ref(option(flight)) = ref(None);

let query =
    (el: Js.t(Dom_html.element), selector: string)
    : option(Js.t(Dom_html.element)) =>
  Js.Opt.to_option(
    Js.Unsafe.meth_call(
      el,
      "querySelector",
      [|Js.Unsafe.inject(Js.string(selector))|],
    ),
  );

let next = (el: Js.t(Dom_html.element)): option(Js.t(Dom_html.element)) =>
  Js.Opt.to_option(Js.Unsafe.get(el, "nextElementSibling"));

let connected = (el: Js.t(Dom_html.element)): bool =>
  Js.to_bool(Js.Unsafe.get(el, "isConnected"));

let kind = (el: Js.t(Dom_html.element)): option(string) =>
  Js.Opt.to_option(el##getAttribute(Js.string("data-proof-step")))
  |> Option.map(Js.to_string);

let stop = () => {
  let previous = active^;
  active := None;
  previous
  |> Option.iter(f => {
       f.target##.classList##remove(Js.string("proof-step-landing"));
       f.animations
       |> List.iter(a => {
            switch (Js.Unsafe.meth_call(a, "cancel", [||])) {
            | exception _ => ()
            | _ => ()
            }
          });
     });
};

let cancel = () => {
  pending := None;
  stop();
};

/* Capture the row before the update, while CodeDrag still owns the preview.
   The update confirms only a successfully revalidated theorem application. */
let request = (~enabled, from: Js.t(Dom_html.element)) => {
  cancel();
  if (enabled) {
    let source: option(Js.t(Dom_html.element)) =
      Js.Opt.to_option(
        Js.Unsafe.meth_call(
          from,
          "closest",
          [|Js.Unsafe.inject(Js.string(".step-border"))|],
        ),
      );
    source
    |> Option.iter(source =>
         if (kind(source) == Some("open") && next(source) == None) {
           let r = {
             source,
             accepted: false,
           };
           pending := Some(r);
           /* A rejected/stale action may cause no render at all. Nothing is
              hidden while pending, and its DOM reference is bounded. */
           ignore(
             Dom_html.setTimeout(
               () => {
                 switch (pending^) {
                 | Some(current) when current === r => pending := None
                 | _ => ()
                 }
               },
               1500.,
             ),
           );
         }
       );
  };
};

let accept = () => pending^ |> Option.iter(r => r.accepted = true);

let animate = (el: Js.t(Dom_html.element), frames) => {
  let frames =
    frames
    |> List.map(((offset, properties)) =>
         Js.Unsafe.obj(
           Array.of_list(
             [("offset", Js.Unsafe.inject(Js.number_of_float(offset)))]
             @ (
               properties
               |> List.map(((k, v)) =>
                    (k, Js.Unsafe.inject(Js.string(v)))
                  )
             ),
           ),
         )
       )
    |> Array.of_list
    |> Js.array;
  let options =
    Js.Unsafe.obj([|
      ("duration", Js.Unsafe.inject(CodeFlip.dur(460))),
      ("easing", Js.Unsafe.inject(Js.string("linear"))),
      ("fill", Js.Unsafe.inject(Js.string("both"))),
    |]);
  switch (
    Js.Unsafe.meth_call(
      el,
      "animate",
      [|Js.Unsafe.inject(frames), Js.Unsafe.inject(options)|],
    )
  ) {
  | exception _ => None
  | animation => Some(animation)
  };
};

/* After the new proof row exists, before paint. Redo or unrelated rows have
   no request; undo during a flight cancels it and restores normal styles. */
let go = (~enabled) => {
  active^
  |> Option.iter(f =>
       if (!enabled
           || !connected(f.source)
           || !connected(f.target)
           || kind(f.source) != Some("axiom")) {
         stop();
       }
     );
  switch (pending^) {
  | None => ()
  | Some(r) when !enabled || !connected(r.source) => pending := None
  | Some(r) when r.accepted && kind(r.source) == Some("axiom") =>
    let destination = {
      open OptUtil.Syntax;
      let* row = next(r.source);
      let* _ = kind(row);
      let* source = query(r.source, ":scope > .step-display");
      let+ target = query(row, ":scope > .step-display");
      (source, target);
    };
    destination
    |> Option.iter(((source, target)) => {
         pending := None;
         stop();
         let before = source##getBoundingClientRect;
         let after = target##getBoundingClientRect;
         let dx = before##.left -. after##.left;
         let dy = before##.top -. after##.top;
         let translate = (x, y) =>
           Printf.sprintf("translate(%fpx, %fpx)", x, y);
         switch (
           animate(
             target,
             [
               (
                 0.,
                 [
                   ("transform", translate(dx, dy)),
                   ("easing", "cubic-bezier(.2,.65,.3,1)"),
                 ],
               ),
               (0.72, [("transform", translate(0., 6.))]),
               (0.88, [("transform", translate(0., -2.))]),
               (1., [("transform", translate(0., 0.))]),
             ],
           )
         ) {
         | None => ()
         | Some(drop) =>
           target##.classList##add(Js.string("proof-step-landing"));
           let reveal =
             animate(
               source,
               [
                 (0., [("opacity", "0")]),
                 (0.2, [("opacity", "0")]),
                 (0.75, [("opacity", "1")]),
                 (1., [("opacity", "1")]),
               ],
             );
           let f = {
             source: r.source,
             target,
             animations: [drop] @ Option.to_list(reveal),
           };
           active := Some(f);
           Js.Unsafe.set(
             drop,
             "onfinish",
             Js.wrap_callback(_ => {
               switch (active^) {
               | Some(current) when current === f => stop()
               | _ => ()
               }
             }),
           );
         };
       });
  | Some(_) => ()
  };
};
