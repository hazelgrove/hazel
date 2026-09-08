open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;
open ChartCore;

let error_message = "Elaborated syntax is not a chart: apply BarChart, GroupedBarChart, LineChart, ScatterChart, or PieChart to a list of data.";

let chart_of = (any: Any.t): option(chart_spec) =>
  switch (any) {
  | Exp(exp) => parse_chart(exp)
  | _ => None
  };

let get = (info: info): option(chart_spec) =>
  switch (info.elaborated) {
  | Some(elab) => parse_chart(elab)
  | None =>
    switch (info.syntax |> info.utility.seg_to_term) {
    | Some(any) => chart_of(any)
    | None => None
    }
  };

/* --- Rendering: d3 owns the DOM ---
 *
 * Every chart kind is drawn by d3 itself. The selection / axis / shape code
 * lives in JS (window.HazelD3.render, see src/web/www/prebundle.js); here we
 * only marshal the chart_spec to a plain JS object and drive d3 through a
 * single Attr hook. The vdom node declares no children for the container
 * <div>, so vdom leaves d3's subtree untouched across re-renders. */

module Js = Js_of_ocaml.Js;

let obj = Js.Unsafe.obj;
let inj = Js.Unsafe.inject;
let jstr = (s: string) => inj(Js.string(s));
let jnum = (f: float) => inj(f);
let jarray = (xs: list(Js.Unsafe.any)) =>
  inj(Js.array(Array.of_list(xs)));

let js_points = (points: list(point)): Js.Unsafe.any =>
  jarray(
    List.map(
      (p: point) => obj([|("x", jnum(p.x)), ("y", jnum(p.y))|]),
      points,
    ),
  );

/* { kind: "bar"|"line"|"scatter"|"pie", ...kind-specific fields } */
let js_of_spec = (spec: chart_spec): Js.Unsafe.any =>
  switch (spec) {
  | Bar({categories, series}) =>
    obj([|
      ("kind", jstr("bar")),
      ("categories", jarray(List.map(jstr, categories))),
      (
        "series",
        jarray(
          List.map(
            (s: series) =>
              obj([|
                ("name", jstr(s.name)),
                (
                  /* None (this series has no value at that category) crosses
                     as null; renderBar skips those bars, keeping the rest
                     aligned to their own categories. */
                  "values",
                  jarray(
                    List.map(
                      v =>
                        Option.fold(
                          ~none=Js.Unsafe.inject(Js.null),
                          ~some=jnum,
                          v,
                        ),
                      s.values,
                    ),
                  ),
                ),
              |]),
            series,
          ),
        ),
      ),
    |])
  | Line(points) =>
    obj([|("kind", jstr("line")), ("points", js_points(points))|])
  | Scatter(points) =>
    obj([|("kind", jstr("scatter")), ("points", js_points(points))|])
  | Pie(slices) =>
    obj([|
      ("kind", jstr("pie")),
      (
        "slices",
        jarray(
          List.map(
            ((label, value)) =>
              obj([|("label", jstr(label)), ("value", jnum(value))|]),
            slices,
          ),
        ),
      ),
    |])
  };

let render_into = (el, spec: chart_spec): unit =>
  Js.Unsafe.meth_call(
    Js.Unsafe.global##.HazelD3,
    "render",
    [|inj(el), js_of_spec(spec)|],
  )
  |> ignore;

/* One hook for all kinds: hands the container to d3 on mount and re-renders on
 * update (d3 clears + redraws, so this is idempotent). */
module ChartHook =
  Attr.Hooks.Make({
    module State = Unit;
    module Input = {
      type t = chart_spec;
      let sexp_of_t = sexp_of_chart_spec;
      let combine = (_a, b) => b;
    };
    let init = (_input, _el) => ();
    let on_mount = (input, (), el) => render_into(el, input);
    let update = (~old_input as _, ~new_input, (), el) =>
      render_into(el, new_input);
    let destroy = (_input, (), _el) => ();
  });

let chart_view = (spec: chart_spec): Node.t =>
  Node.div(
    ~attrs=[
      Attr.classes(["chart-d3"]),
      Attr.create_hook("hazel-chart", ChartHook.create(spec)),
    ],
    [],
  );

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = (any: Any.t) =>
    switch (chart_of(any)) {
    | Some(_) => Some()
    | None => None
    };

  let focusable = Focusable.non;
  let dynamics = false;
  let elaborate_syntax = true;

  /* Block reservation roughly matching the SVG's CSS height/width. */
  let placeholder = (_, _info) =>
    ProjectorCore.Shape.{
      vertical: Block(11),
      horizontal: 46,
    };

  let update = (model, _, _) => model;

  let error = (_, info) =>
    switch (get(info)) {
    | Some(_) => None
    | None => Some(ProjectorBase.{message: error_message})
    };

  let view = ({info, view_seg, _}: View.args(model, action)): View.t =>
    switch (get(info)) {
    | None =>
      let seg = Segment.unparenthesize(info.syntax);
      let sort = Segment.sort_of(Segment.skel(seg), seg);
      let banner =
        Node.div(
          ~attrs=[Attr.classes(["chart-error-banner"])],
          [Node.text(error_message)],
        );
      View.mk(
        ~error=true,
        Node.div(
          ~attrs=[Attr.classes(["chart-inner"])],
          [banner, view_seg(sort, seg)],
        ),
      );
    | Some(spec) =>
      View.mk(
        Node.div(
          ~attrs=[Attr.classes(["chart-inner"])],
          [chart_view(spec)],
        ),
      )
    };
};
