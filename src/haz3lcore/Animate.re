open Util;
open Js_of_ocaml;

/* Position & dimensions for a DOM element */
[@deriving (show({with_path: false}), sexp, yojson)]
type box = {
  top: int,
  left: int,
  height: float,
  width: float,
};

/* Options for CSS Animations API */
type options = {
  duration: int,
  easing: string,
};

/* CSS property-value pairs as strings */
type keyframe = (string, string);

/* Specify a transition for an element */
type transition = {
  id: string,
  options,
  animation: option(box) => list(keyframe),
};

type transition_internal = {
  id: string,
  options,
  animation: option(box) => list(keyframe),
  box: option(box),
};

let tracked_elems: ref(list(transition_internal)) = ref([]);

let animate =
    (id: string, keyframes: list((string, string)), options: options): unit => {
  let elem = JsUtil.get_elem_by_id(id);

  let keyframe_objs =
    keyframes
    |> List.map(((prop, value)) =>
         Js.Unsafe.obj([|(prop, Js.Unsafe.inject(Js.string(value)))|])
       )
    |> Array.of_list
    |> Js.array;

  let options_obj =
    [
      ("duration", Js.Unsafe.inject(options.duration)),
      ("easing", Js.Unsafe.inject(Js.string(options.easing))),
    ]
    |> Array.of_list
    |> Js.Unsafe.obj;

  Js.Unsafe.meth_call(
    elem,
    "animate",
    [|Js.Unsafe.inject(keyframe_objs), Js.Unsafe.inject(options_obj)|],
  );
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

let get_box = (id: string): option(box) =>
  switch (JsUtil.get_elem_by_id_opt(id)) {
  | Some(elem) => Some(box_of(elem))
  | None => None
  };

let delta_box = (init: box, final: box): box => {
  left: final.left - init.left,
  top: final.top - init.top,
  width: final.width -. init.width,
  height: final.height -. init.height,
};

let delta_box_opt = (init: option(box), final: option(box)): option(box) =>
  switch (final, init) {
  | (Some(final), Some(init)) => Some(delta_box(init, final))
  | _ => None
  };

let go = (): unit =>
  if (tracked_elems^ != []) {
    tracked_elems^
    |> List.iter(({id, box, options, animation}) =>
         animate(id, animation(delta_box_opt(get_box(id), box)), options)
       );
    tracked_elems := [];
  };

let setup = (transitions: list(transition)): unit => {
  tracked_elems :=
    List.map(
      ({id, options, animation}: transition) =>
        {id, box: get_box(id), options, animation},
      transitions,
    );
};

module Keyframes = {
  let transform_translate = (top: int, left: int) => (
    "transform",
    Printf.sprintf("translate(%dpx, %dpx)", left, top),
  );
  let transform_scale_uniform = (scale: float) => (
    "transform",
    Printf.sprintf("scale(%f, %f)", scale, scale),
  );
  let translate = (delta: option(box)): list(keyframe) =>
    switch (delta) {
    | None =>
      // Scale up newly inserted elements
      [transform_scale_uniform(0.0), transform_scale_uniform(1.0)]
    | Some({left, top, _}) =>
      // Translate elements that exist in both states
      [transform_translate(top, left), transform_translate(0, 0)]
    };
};
