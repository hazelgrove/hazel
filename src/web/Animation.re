open Util;
open Js_of_ocaml;

/* This implements arbitrary gpu-accelerated css position
 * and scale transition animations via the the FLIP technique
 * (https://aerotwist.com/blog/flip-your-animations/).
 *
 * From the client perspective, it suffices to call the request
 * method with a list of the DOM element ids to animate, as well
 * as some animation settings (keyframes, duration, easing).
 *
 * Some common keyframes are provided in the module at the bottom */

/* This is an extremely partial implementation of the Web Animations
 * API, which currently does not have Js_of_ocaml wrappers */
module Js = {
  /* CSS property-value pairs */
  type keyframe = (string, string);

  type options = {
    duration: int,
    easing: string,
    /* ms before the animation starts; the first keyframe holds meanwhile
       (fill: backwards), so a delayed move/grow-in stays put, then goes */
    delay: int,
  };

  /* Options for CSS Animations API animate method */
  type animation = {
    options,
    keyframes: list(keyframe),
  };

  /* Position & dimensions for a DOM element */
  type box = {
    top: float,
    left: float,
    height: float,
    width: float,
  };

  let box_of = (elem: Js.t(Dom_html.element)): box => {
    let container_rect = elem##getBoundingClientRect;
    {
      top: container_rect##.top,
      left: container_rect##.left,
      height: Js.Optdef.get(container_rect##.height, _ => 0.0),
      width: Js.Optdef.get(container_rect##.width, _ => 0.0),
    };
  };

  let client_height = (): float =>
    Js.Optdef.get(
      Js.Unsafe.get(Dom_html.document, "documentElement")##.clientHeight, _ =>
      0.0
    );

  let inner_height = (): float =>
    Js.Optdef.get(Js.Unsafe.get(Dom_html.window, "innerHeight"), _ => 0.0);

  let check_visible = (client_height, inner_height, box: box): bool => {
    let viewHeight = max(client_height, inner_height);
    !(box.top +. box.height < 0.0 || box.top -. viewHeight >= 0.0);
  };

  let keyframes_unsafe = (keyframes: list(keyframe)): Js.t(Js.js_array('a)) =>
    keyframes
    |> List.map(((prop: string, value: string)) =>
         Js.Unsafe.obj([|(prop, Js.Unsafe.inject(Js.string(value)))|])
       )
    |> Array.of_list
    |> Js.array;

  let options_unsafe =
      ({duration, easing, delay}: options): Js.t(Js.js_array('a)) =>
    [
      ("duration", Js.Unsafe.inject(duration)),
      ("easing", Js.Unsafe.inject(Js.string(easing))),
    ]
    @ (
      delay > 0
        ? [
          ("delay", Js.Unsafe.inject(delay)),
          ("fill", Js.Unsafe.inject(Js.string("backwards"))),
        ]
        : []
    )
    |> Array.of_list
    |> Js.Unsafe.obj;

  /* keyframes with several properties each (the single-property form
     above encodes one property per keyframe) */
  let animate_multi =
      (
        frames: list(list((string, string))),
        options: options,
        elem: Js.t(Dom_html.element),
      )
      : unit => {
    let kf =
      frames
      |> List.map(props =>
           Js.Unsafe.obj(
             props
             |> List.map(((p, v)) => (p, Js.Unsafe.inject(Js.string(v))))
             |> Array.of_list,
           )
         )
      |> Array.of_list
      |> Js.array;
    ignore(
      Js.Unsafe.meth_call(
        elem,
        "animate",
        [|
          Js.Unsafe.inject(kf),
          Js.Unsafe.inject(options_unsafe(options)),
        |],
      ),
    );
  };

  let animate_unsafe =
      (
        keyframes: list(keyframe),
        options: options,
        elem: Js.t(Dom_html.element),
      ) =>
    Js.Unsafe.meth_call(
      elem,
      "animate",
      [|
        Js.Unsafe.inject(keyframes_unsafe(keyframes)),
        Js.Unsafe.inject(options_unsafe(options)),
      |],
    );

  let animate = ({options, keyframes}, elem: Js.t(Dom_html.element)) =>
    if (keyframes != []) {
      switch (animate_unsafe(keyframes, options, elem)) {
      | exception exn =>
        print_endline("Animation: " ++ Printexc.to_string(exn))
      | () => ()
      };
    };
};

open Js;

/* If an element is new, report its new metrics.
 * Otherwise, report both new & old metrics */
type change =
  | New(box)
  | Existing(box, box);

/* Specify a transition for an element */
type transition = {
  /* A unique id used as attribute for
   * the relevant DOM element */
  id: string,
  /* The animation function recieves the diffs
   * for the element's position and scale across a
   * change, which it may use to calculate the
   * parameters for a resulting animation */
  animate: change => animation,
};

/* Internally, transitions must track the initial
 * metrics for an element, gathered in the `Request ` phase */
type transition_internal = {
  id: string,
  animate: change => animation,
  box: option(box),
};

/* Elements and their corresponding animations are tracked
 * here between when the action is used (`request`) and
 * when the animation is executed (`go`) */
let tracked_elems: ref(list(transition_internal)) = ref([]);

let animate_elem = (({box, animate, _}, elem, new_box)): unit =>
  switch (box, new_box) {
  | (Some(init), Some(final)) =>
    Js.animate(animate(Existing(init, final)), elem)
  | (None, Some(final)) => Js.animate(animate(New(final)), elem)
  | (Some(_init), None) =>
    //TODO: Removed case (requires retaining old element somehow)
    ()
  | (None, None) => ()
  };

let filter_visible_elements = (tracked_elems: list(transition_internal)) => {
  let client_height = client_height();
  let inner_height = inner_height();
  List.filter_map(
    (tr: transition_internal) => {
      switch (JsUtil.get_elem_by_id_opt(tr.id)) {
      | None => None
      | Some(elem) =>
        let new_box = box_of(elem);
        check_visible(client_height, inner_height, new_box)
          ? Some((tr, elem, Some(new_box))) : None;
      }
    },
    tracked_elems,
  );
};

/* Stagger bookkeeping for one `go` pass: NEW elements get ascending
   indices (in tracked order) so arrivals can be spread out in time, and
   movers can wait until the arrivals are done. Read inside `animate`
   closures via Actions.move. */
let stagger_index: ref(int) = ref(0);
let stagger_total: ref(int) = ref(0);
/* arrivals never spread over more than this, however many there are */
let stagger_span_cap = 2400;
let stagger_step = (per: int): int =>
  min(per, stagger_span_cap / max(1, stagger_total^));
let stagger_span = (per: int): int =>
  min(stagger_span_cap, stagger_total^ * stagger_step(per));

/* Request animations. Call this during the MVU update */
let request = (transitions: list(transition)): unit => {
  tracked_elems :=
    List.map(
      ({id, animate}: transition) =>
        {
          id,
          box: Option.map(box_of, JsUtil.get_elem_by_id_opt(id)),
          animate,
        },
      transitions,
    )
    @ tracked_elems^;
};

module Keyframes = {
  let transform_translate = (top: float, left: float): keyframe => (
    "transform",
    Printf.sprintf("translate(%fpx, %fpx)", left, top),
  );

  let translate = (~scale=1., init: box, final: box): list(keyframe) => {
    [
      /* rects are measured in screen px; the transform plays inside the
         element's local space. When an ancestor applies CSS zoom, both
         differ from local px by that factor — divide once so the visual
         glide matches the measured hop (else motion plays at zoom^2). */
      transform_translate(
        (init.top -. final.top) /. scale,
        (init.left -. final.left) /. scale,
      ),
      transform_translate(0., 0.),
    ];
  };

  let transform_scale_uniform = (scale: float): keyframe => (
    "transform",
    Printf.sprintf("scale(%f, %f)", scale, scale),
  );

  let scale_from_zero: list(keyframe) = [
    transform_scale_uniform(0.0),
    transform_scale_uniform(1.0),
  ];
};

let easeOutExpo = "cubic-bezier(0.16, 1, 0.3, 1)";
let easeInOutBack = "cubic-bezier(0.68, -0.6, 0.32, 1.6)";
let easeInOutExpo = "cubic-bezier(0.87, 0, 0.13, 1)";

/* (beats and `go` live below Keyframes/easings, which they use) */
/* ---- beats: arrivals + SVG geometry ----
   Elements that don't exist yet when a beat is staged can't be tracked
   by box, so the beat records which ids exist per prefix; at `go`, ids
   that appeared are ARRIVALS and grow in one by one (staggered), and
   tracked SVG geometry (edge paths, formation lines, orbit rings) MORPHS
   from its old attributes to the new ones on the movers' timing instead
   of snapping while the nodes glide. */
type beat_stage = {
  arrival_prefixes: list(string),
  existing: list(string),
  geom_prefixes: list(string),
  /* id -> (attribute, old value) for the geometry we morph */
  geom_old: list((string, list((string, string)))),
  b_delay: int,
  b_stagger: int,
  b_move_dur: int,
};
let beat: ref(option(beat_stage)) = ref(None: option(beat_stage));
/* the last `go`'s arrivals as (id, delay ms), for choreography that runs
   after the render (the avatar touring the new nodes as they bloom) */
let last_arrivals: ref(list((string, int))) = ref([]);
let geom_attrs = ["d", "cx", "cy"];
/* (module Js above shadows Js_of_ocaml.Js: qualify explicitly) */
let attr_of =
    (el: Js_of_ocaml.Js.t(Dom_html.element), name: string): option(string) =>
  Js_of_ocaml.Js.Opt.to_option(
    el##getAttribute(Js_of_ocaml.Js.string(name)),
  )
  |> Option.map(Js_of_ocaml.Js.to_string);
let request_beat =
    (
      ~arrival_prefixes: list(string),
      ~geom_prefixes: list(string),
      ~delay: int,
      ~stagger: int,
      ~move_dur: int,
    )
    : unit => {
  let ids = prefixes => List.concat_map(JsUtil.ids_with_prefix, prefixes);
  beat :=
    Some({
      arrival_prefixes,
      existing: ids(arrival_prefixes),
      geom_prefixes,
      geom_old:
        ids(geom_prefixes)
        |> List.filter_map(id =>
             JsUtil.get_elem_by_id_opt(id)
             |> Option.map(el =>
                  (
                    id,
                    geom_attrs
                    |> List.filter_map(a =>
                         attr_of(el, a) |> Option.map(v => (a, v))
                       ),
                  )
                )
           ),
      b_delay: delay,
      b_stagger: stagger,
      b_move_dur: move_dur,
    });
};
/* CSS value for an SVG geometry attribute */
let geom_css = (attr: string, v: string): string =>
  attr == "d" ? "path(\"" ++ v ++ "\")" : v ++ "px";

/* Execute animations. This is called during the
 * render phase, after recalc but before repaint */
let go = (): unit => {
  let visible =
    tracked_elems^ == [] ? [] : tracked_elems^ |> filter_visible_elements;
  let arrivals =
    switch (beat^) {
    | None => []
    | Some(b) =>
      List.concat_map(JsUtil.ids_with_prefix, b.arrival_prefixes)
      |> List.filter(id => !List.mem(id, b.existing))
      |> List.filter_map(id =>
           JsUtil.get_elem_by_id_opt(id) |> Option.map(el => (id, el))
         )
    };
  /* movers wait for ALL arrivals, tracked or not */
  stagger_total :=
    List.length(arrivals)
    + List.length(List.filter(((tr, _, _)) => tr.box == None, visible));
  stagger_index := 0;
  switch (beat^) {
  | Some(b) when arrivals != [] =>
    let step = stagger_step(b.b_stagger);
    last_arrivals := [];
    List.iter(
      ((id, el)) => {
        let delay = b.b_delay + stagger_index^ * step;
        last_arrivals := last_arrivals^ @ [(id, delay)];
        Js.animate(
          {
            options: {
              duration: 220,
              easing: easeOutExpo,
              delay,
            },
            keyframes: Keyframes.scale_from_zero,
          },
          el,
        );
        incr(stagger_index);
      },
      arrivals,
    );
  | _ => ()
  };
  if (visible != []) {
    visible
    |> List.iter(((tr, _, _) as x) => {
         animate_elem(x);
         if (tr.box == None) {
           incr(stagger_index);
         };
       });
    tracked_elems := [];
  };
  switch (beat^) {
  | None => ()
  | Some(b) =>
    let wait =
      b.b_stagger > 0 && stagger_total^ > 0
        ? stagger_span(b.b_stagger) + 120 : 0;
    /* morph tracked geometry on the movers' timing */
    List.iter(
      ((id, olds)) =>
        switch (JsUtil.get_elem_by_id_opt(id)) {
        | None => ()
        | Some(el) =>
          let frames =
            olds
            |> List.filter_map(((a, old)) =>
                 switch (attr_of(el, a)) {
                 | Some(nw) when nw != old => Some((a, old, nw))
                 | _ => None
                 }
               );
          if (frames != []) {
            let kf = side =>
              List.map(
                ((a, old, nw)) =>
                  (a, geom_css(a, side == `From ? old : nw)),
                frames,
              );
            Js.animate_multi(
              [kf(`From), kf(`To)],
              {
                duration: b.b_move_dur,
                easing: easeOutExpo,
                delay: b.b_delay + wait,
              },
              el,
            );
          };
        },
      b.geom_old,
    );
    /* new geometry (fresh formation lines, orbit rings, edges) DRAWS ON
       from its source end once the arrivals it belongs to have grown
       in — a dash reveal along the stroke, arrowhead held until the end.
       (The canvas's enactment re-stages new function edges with the
       avatar riding them; it cancels this first.) */
    let known = List.map(fst, b.geom_old);
    let set_attr = (el, a: string, v: string) =>
      ignore(
        Js_of_ocaml.Js.Unsafe.meth_call(
          el,
          "setAttribute",
          [|
            Js_of_ocaml.Js.Unsafe.inject(Js_of_ocaml.Js.string(a)),
            Js_of_ocaml.Js.Unsafe.inject(Js_of_ocaml.Js.string(v)),
          |],
        ),
      );
    List.concat_map(JsUtil.ids_with_prefix, b.geom_prefixes)
    |> List.filter(id => !List.mem(id, known))
    |> List.iter(id =>
         switch (JsUtil.get_elem_by_id_opt(id)) {
         | Some(el) =>
           let total: float =
             Js_of_ocaml.Js.Unsafe.meth_call(el, "getTotalLength", [||]);
           if (total > 4.) {
             let marker = attr_of(el, "marker-end");
             set_attr(
               el,
               "stroke-dasharray",
               Printf.sprintf("%.1f %.1f", total, total),
             );
             switch (marker) {
             | Some(_) => set_attr(el, "marker-end", "none")
             | None => ()
             };
             Js.animate_multi(
               [
                 [("strokeDashoffset", Printf.sprintf("%.1f", total))],
                 [("strokeDashoffset", "0")],
               ],
               {
                 duration: b.b_move_dur,
                 easing: "cubic-bezier(0.65, 0, 0.35, 1)",
                 delay: b.b_delay + wait,
               },
               el,
             );
             switch (marker) {
             | Some(m) =>
               ignore(
                 Js_of_ocaml.Js.Unsafe.global##setTimeout(
                   Js_of_ocaml.Js.Unsafe.callback(() =>
                     set_attr(el, "marker-end", m)
                   ),
                   b.b_delay + wait + b.b_move_dur,
                 ),
               )
             | None => ()
             };
           };
         | None => ()
         }
       );
    beat := None;
  };
};

module Actions = {
  /* ~stagger: ms between successive NEW elements' arrivals (0 = all at
     once); movers then wait for the arrivals and take ~move_dur, so a
     beat reads "new things appear, then the rest makes room" */
  let move = (~scale=1., ~delay=0, ~stagger=0, ~move_dur=125, id) => {
    id,
    animate: change =>
      switch (change) {
      | New(_) => {
          options: {
            duration: 125,
            easing: easeOutExpo,
            delay: delay + stagger_index^ * stagger_step(stagger),
          },
          keyframes: Keyframes.scale_from_zero,
        }
      | Existing(init, final) => {
          options: {
            duration: move_dur,
            easing: easeOutExpo,
            delay:
              delay
              + (
                stagger > 0 && stagger_total^ > 0
                  ? stagger_span(stagger) + 120 : 0
              ),
          },
          keyframes: Keyframes.translate(~scale, init, final),
        }
      },
  };
  /* slower, symmetric travel — used for the agent avatar, whose hops
     across the canvas should read as movement, not teleporting */
  let move_slow = (~scale=1., id) => {
    id,
    animate: change => {
      options: {
        duration: 450,
        easing: "cubic-bezier(0.65, 0, 0.35, 1)",
        delay: 0,
      },
      keyframes:
        switch (change) {
        | New(_) => Keyframes.scale_from_zero
        | Existing(init, final) => Keyframes.translate(~scale, init, final)
        },
    },
  };
};
