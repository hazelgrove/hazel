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
  };

  /* Options for CSS Animations API animate method */
  type animation = {
    options,
    keyframes: list(keyframe),
  };

  /* Position & dimensions for a DOM element */
  type box = {
    top: int,
    left: int,
    height: float,
    width: float,
  };

  let box_of = (elem: Js.t(Dom_html.element)): box => {
    let container_rect = elem##getBoundingClientRect;
    {
      top: int_of_float(container_rect##.top),
      left: int_of_float(container_rect##.left),
      height: Js.Optdef.get(container_rect##.height, _ => (-1.0)),
      width: Js.Optdef.get(container_rect##.width, _ => (-1.0)),
    };
  };

  let get_elem_box = (id: string): option(box) =>
    Option.map(box_of, JsUtil.get_elem_by_id_opt(id));

  let keyframes_unsafe = (keyframes: list(keyframe)): Js.t(Js.js_array('a)) =>
    keyframes
    |> List.map(((prop: string, value: string)) =>
         Js.Unsafe.obj([|(prop, Js.Unsafe.inject(Js.string(value)))|])
       )
    |> Array.of_list
    |> Js.array;

  let options_unsafe = ({duration, easing}: options): Js.t(Js.js_array('a)) =>
    [
      ("duration", Js.Unsafe.inject(duration)),
      ("easing", Js.Unsafe.inject(Js.string(easing))),
    ]
    |> Array.of_list
    |> Js.Unsafe.obj;

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
  //| Removed(box)
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

let animate_elem = ({id, box, animate}, elem): unit =>
  switch (box, get_elem_box(id)) {
  | (Some(init), Some(final)) =>
    Js.animate(animate(Existing(init, final)), elem)
  | (None, Some(final)) => Js.animate(animate(New(final)), elem)
  | (Some(_init), None) =>
    //TODO: Removed case (requires retaining old element somehow)
    ()
  | (None, None) => ()
  };

/* Execute animations. This is called during the
 * render phase, after recalc but before repaint */
let go = (): unit =>
  if (tracked_elems^ != []) {
    tracked_elems^
    |> List.iter(animation =>
         JsUtil.get_elem_by_id_opt(animation.id)
         |> Option.iter(animate_elem(animation))
       );
    tracked_elems := [];
  };

/* Request animations. Call this during the MVU update */
let request = (transitions: list(transition)): unit => {
  tracked_elems :=
    List.map(
      ({id, animate}: transition) => {id, box: get_elem_box(id), animate},
      transitions,
    );
};

module Keyframes = {
  let transform_translate = (top: int, left: int): keyframe => (
    "transform",
    Printf.sprintf("translate(%dpx, %dpx)", left, top),
  );

  let translate = (init: box, final: box): list(keyframe) => {
    [
      transform_translate(init.top - final.top, init.left - final.left),
      transform_translate(0, 0),
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

module Actions = {
  let move = id => {
    id,
    animate: change => {
      options: {
        duration: 125,
        easing: "cubic-bezier(0.16, 1, 0.3, 1)",
      },
      keyframes:
        switch (change) {
        | New(_) => Keyframes.scale_from_zero
        | Existing(init, final) => Keyframes.translate(init, final)
        },
    },
  };
};
