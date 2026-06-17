open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;
open ChartCore;

let error_message = "Elaborated syntax is not a chart: apply BarChart, LineChart, ScatterChart, or PieChart to a list of data.";

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

/* --- SVG geometry (viewBox coordinate space; CSS sizes the element) --- */

let width = 320.0;
let height = 220.0;
let m_top = 12.0;
let m_right = 14.0;
let m_bottom = 30.0;
let m_left = 38.0;
let inner_w = width -. m_left -. m_right;
let inner_h = height -. m_top -. m_bottom;
let baseline = m_top +. inner_h;
let plot_right = m_left +. inner_w;

let palette = [|
  "#4e79a7",
  "#f28e2b",
  "#59a14f",
  "#e15759",
  "#edc948",
  "#b07aa1",
  "#76b7b2",
  "#ff9da7",
|];
let color = (i: int): string => palette[i mod Array.length(palette)];

let px = (f: float): string => Printf.sprintf("%.3f", f);
let fa = (name, f) => Attr.create(name, px(f));

let fmt_tick = (t: float): string =>
  if (Float.equal(Float.round(t), t) && Float.abs(t) < 1e15) {
    Printf.sprintf("%.0f", t);
  } else {
    Printf.sprintf("%.4g", t);
  };

let rect_el = (~x, ~y, ~w, ~h, ~fill): Node.t =>
  Node.create_svg(
    "rect",
    ~attrs=[
      fa("x", x),
      fa("y", y),
      fa("width", w),
      fa("height", h),
      Attr.create("fill", fill),
    ],
    [],
  );

let circle_el = (~cx, ~cy, ~r, ~fill): Node.t =>
  Node.create_svg(
    "circle",
    ~attrs=[
      fa("cx", cx),
      fa("cy", cy),
      fa("r", r),
      Attr.create("fill", fill),
    ],
    [],
  );

let line_el = (~x1, ~y1, ~x2, ~y2, ~cls): Node.t =>
  Node.create_svg(
    "line",
    ~attrs=[
      fa("x1", x1),
      fa("y1", y1),
      fa("x2", x2),
      fa("y2", y2),
      Attr.classes(cls),
    ],
    [],
  );

let text_el = (~x, ~y, ~cls, s): Node.t =>
  Node.create_svg(
    "text",
    ~attrs=[fa("x", x), fa("y", y), Attr.classes(cls)],
    [Node.text(s)],
  );

let path_el = (~d, ~fill): Node.t =>
  Node.create_svg(
    "path",
    ~attrs=[Attr.create("d", d), Attr.create("fill", fill)],
    [],
  );

let svg = (~kind: string, children: list(Node.t)): Node.t =>
  Node.create_svg(
    "svg",
    ~attrs=[
      Attr.create(
        "viewBox",
        Printf.sprintf("0 0 %s %s", px(width), px(height)),
      ),
      Attr.create("preserveAspectRatio", "xMidYMid meet"),
      Attr.classes(["chart-svg", "chart-" ++ kind]),
    ],
    children,
  );

let flat_mapi = (f, xs) => List.concat(List.mapi(f, xs));

let no_data: list(Node.t) = [
  text_el(
    ~x=width /. 2.0,
    ~y=height /. 2.0,
    ~cls=["chart-empty"],
    "no data",
  ),
];

let axis_frame: list(Node.t) = [
  line_el(
    ~x1=m_left,
    ~y1=m_top,
    ~x2=m_left,
    ~y2=baseline,
    ~cls=["chart-axis"],
  ),
  line_el(
    ~x1=m_left,
    ~y1=baseline,
    ~x2=plot_right,
    ~y2=baseline,
    ~cls=["chart-axis"],
  ),
];

/* Compute a (lo, hi) domain from values, padding a zero-width span. */
let domain_of = (vs: list(float)): (float, float) =>
  switch (vs) {
  | [] => (0.0, 1.0)
  | [v0, ...rest] =>
    let lo = List.fold_left(min, v0, rest);
    let hi = List.fold_left(max, v0, rest);
    Float.equal(lo, hi) ? (lo -. 1.0, hi +. 1.0) : (lo, hi);
  };

/* Horizontal gridlines + left-margin labels for a vertical (y) scale. */
let y_ticks = (~lo, ~hi, sy: float => float): list(Node.t) =>
  D3.ticks(~lo, ~hi, ~count=4)
  |> List.concat_map(t => {
       let y = sy(t);
       [
         line_el(
           ~x1=m_left,
           ~y1=y,
           ~x2=plot_right,
           ~y2=y,
           ~cls=["chart-grid"],
         ),
         text_el(
           ~x=m_left -. 4.0,
           ~y=y +. 3.0,
           ~cls=["chart-tick", "y-tick"],
           fmt_tick(t),
         ),
       ];
     });

let x_ticks = (~lo, ~hi, sx: float => float): list(Node.t) =>
  D3.ticks(~lo, ~hi, ~count=4)
  |> List.map(t =>
       text_el(
         ~x=sx(t),
         ~y=baseline +. 12.0,
         ~cls=["chart-tick", "x-tick"],
         fmt_tick(t),
       )
     );

let bar_view =
    (categories: list(string), series: list(series)): list(Node.t) => {
  let n = List.length(categories);
  let k = List.length(series);
  if (n == 0 || k == 0) {
    no_data;
  } else {
    let all_values = List.concat_map((s: series) => s.values, series);
    let (lo, hi) = domain_of([0.0, ...all_values]);
    let sy = D3.scale_linear(~domain=(lo, hi), ~range=(baseline, m_top));
    let (xpos, bw) =
      D3.scale_band(~count=n, ~range=(m_left, plot_right), ~padding=0.2);
    let subw = bw /. float_of_int(k);
    let y0 = sy(0.0);
    let bars =
      flat_mapi(
        (si, s: series) =>
          List.mapi(
            (ci, v) => {
              let bx = xpos(ci) +. subw *. float_of_int(si);
              let yv = sy(v);
              rect_el(
                ~x=bx,
                ~y=min(yv, y0),
                ~w=max(subw -. 1.0, 1.0),
                ~h=max(Float.abs(yv -. y0), 0.0),
                ~fill=color(si),
              );
            },
            s.values,
          ),
        series,
      );
    let labels =
      List.mapi(
        (ci, cat) =>
          text_el(
            ~x=xpos(ci) +. bw /. 2.0,
            ~y=baseline +. 12.0,
            ~cls=["chart-tick", "x-label"],
            cat,
          ),
        categories,
      );
    y_ticks(~lo, ~hi, sy) @ axis_frame @ bars @ labels;
  };
};

let xy_view = (~connect: bool, points: list(point)): list(Node.t) =>
  switch (points) {
  | [] => no_data
  | _ =>
    let (lo_x, hi_x) = domain_of(List.map(p => p.x, points));
    let (lo_y, hi_y) = domain_of(List.map(p => p.y, points));
    let sx =
      D3.scale_linear(~domain=(lo_x, hi_x), ~range=(m_left, plot_right));
    let sy =
      D3.scale_linear(~domain=(lo_y, hi_y), ~range=(baseline, m_top));
    let dots =
      List.map(
        p => circle_el(~cx=sx(p.x), ~cy=sy(p.y), ~r=2.5, ~fill=color(0)),
        points,
      );
    let line =
      if (connect) {
        let pts =
          points
          |> List.map(p => px(sx(p.x)) ++ "," ++ px(sy(p.y)))
          |> String.concat(" ");
        [
          Node.create_svg(
            "polyline",
            ~attrs=[
              Attr.create("points", pts),
              Attr.create("fill", "none"),
              Attr.create("stroke", color(0)),
              Attr.classes(["chart-line"]),
            ],
            [],
          ),
        ];
      } else {
        [];
      };
    y_ticks(~lo=lo_y, ~hi=hi_y, sy)
    @ x_ticks(~lo=lo_x, ~hi=hi_x, sx)
    @ axis_frame
    @ line
    @ dots;
  };

let pie_view = (slices: list((string, float))): list(Node.t) => {
  let slices = List.map(((l, v)) => (l, max(0.0, v)), slices);
  let total = List.fold_left((acc, (_, v)) => acc +. v, 0.0, slices);
  if (total <= 0.0) {
    no_data;
  } else {
    /* Pie on the left, legend (label: value) on the right — a pie has no
     * axis, so the legend is the only place the categories/values appear. */
    let r = min(inner_h /. 2.0 -. 2.0, 74.0);
    let cx = 8.0 +. r;
    let cy = m_top +. inner_h /. 2.0;
    let tau = 2.0 *. Float.pi;
    let (_, wedges) =
      List.fold_left(
        ((start, acc), ((_, v), i)) => {
          let sweep = tau *. (v /. total);
          let a0 = start -. Float.pi /. 2.0;
          let a1 = a0 +. sweep;
          let wedge =
            if (sweep >= tau -. 1e-6) {
              circle_el(~cx, ~cy, ~r, ~fill=color(i));
            } else {
              let x0 = cx +. r *. cos(a0);
              let y0 = cy +. r *. sin(a0);
              let x1 = cx +. r *. cos(a1);
              let y1 = cy +. r *. sin(a1);
              let large = sweep > Float.pi ? 1 : 0;
              let d =
                Printf.sprintf(
                  "M %s %s L %s %s A %s %s 0 %d 1 %s %s Z",
                  px(cx),
                  px(cy),
                  px(x0),
                  px(y0),
                  px(r),
                  px(r),
                  large,
                  px(x1),
                  px(y1),
                );
              path_el(~d, ~fill=color(i));
            };
          (start +. sweep, [wedge, ...acc]);
        },
        (0.0, []),
        List.mapi((i, s) => (s, i), slices),
      );
    let legend_x = cx +. r +. 12.0;
    let row_h = 14.0;
    let n = List.length(slices);
    let legend_y0 = cy -. row_h *. float_of_int(n) /. 2.0 +. row_h /. 2.0;
    let legend =
      List.concat(
        List.mapi(
          (i, (label, v)) => {
            let ly = legend_y0 +. row_h *. float_of_int(i);
            [
              rect_el(
                ~x=legend_x,
                ~y=ly -. 6.0,
                ~w=8.0,
                ~h=8.0,
                ~fill=color(i),
              ),
              text_el(
                ~x=legend_x +. 12.0,
                ~y=ly +. 2.0,
                ~cls=["chart-legend"],
                label ++ ": " ++ fmt_tick(v),
              ),
            ];
          },
          slices,
        ),
      );
    List.rev(wedges) @ legend;
  };
};

let render = (spec: chart_spec): Node.t =>
  switch (spec) {
  | Bar({categories, series}) =>
    svg(~kind="bar", bar_view(categories, series))
  | Pie(slices) => svg(~kind="pie", pie_view(slices))
  | Line(points) => svg(~kind="line", xy_view(~connect=true, points))
  | Scatter(points) => svg(~kind="scatter", xy_view(~connect=false, points))
  };

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
        Node.div(~attrs=[Attr.classes(["chart-inner"])], [render(spec)]),
      )
    };
};
