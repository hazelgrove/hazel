open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Language;

module GraphData = {
  type point = {
    x: float,
    y: float,
  };

  type series = {
    label: string,
    points: list(point),
  };

  type t = {
    series: list(series),
    title: option(string),
    x_label: option(string),
    y_label: option(string),
    x_bounds: (float, float),
    y_bounds: (float, float),
  };
};

type decode_error = string;

let float_of_number = (value: DHExp.t): option(float) =>
  switch (DHExp.term_of(value)) {
  | Atom(Float(f)) => Some(f)
  | Atom(Int(i)) => Some(Bigint.to_float(i))
  | Atom(Nat(i)) => Some(Bigint.to_float(i))
  | Atom(SInt(i)) => Some(Float.of_int(i))
  | _ => None
  };

let string_of_value = (value: DHExp.t): option(string) =>
  switch (DHExp.term_of(value)) {
  | Atom(String(s)) => Some(s)
  | _ => None
  };

let decode_point = (value: DHExp.t): result(GraphData.point, decode_error) =>
  switch (DHExp.term_of(value)) {
  | Tuple([x_exp, y_exp]) =>
    switch (float_of_number(x_exp), float_of_number(y_exp)) {
    | (Some(x), Some(y)) =>
      Ok({
        x,
        y,
      })
    | _ => Error("Expected numeric (x, y) pair in data points")
    }
  | _ => Error("Expected tuple (x, y) for each point")
  };

let rec decode_point_list =
        (values: list(DHExp.t), acc: list(GraphData.point))
        : result(list(GraphData.point), decode_error) =>
  switch (values) {
  | [] => Ok(List.rev(acc))
  | [value, ...tail] =>
    switch (decode_point(value)) {
    | Ok(point) => decode_point_list(tail, [point, ...acc])
    | Error(message) => Error(message)
    }
  };

let decode_points =
    (value: DHExp.t): result(list(GraphData.point), decode_error) =>
  switch (DHExp.term_of(value)) {
  | ListLit(point_values) => decode_point_list(point_values, [])
  | _ => Error("Series data must be a list of (x, y) tuples")
  };

let decode_series_entry =
    (value: DHExp.t): result(GraphData.series, decode_error) =>
  switch (DHExp.term_of(value)) {
  | Tuple([label_exp, points_exp]) =>
    switch (string_of_value(label_exp), decode_points(points_exp)) {
    | (Some(label), Ok(points)) =>
      Ok({
        label,
        points,
      })
    | (None, _) => Error("Series entry must have a string label")
    | (_, Error(message)) => Error(message)
    }
  | _ => Error("Expected (label, points) tuple for each series")
  };

let rec decode_series_list =
        (values: list(DHExp.t), acc: list(GraphData.series))
        : result(list(GraphData.series), decode_error) =>
  switch (values) {
  | [] => Ok(List.rev(acc))
  | [value, ...tail] =>
    switch (decode_series_entry(value)) {
    | Ok(series) => decode_series_list(tail, [series, ...acc])
    | Error(message) => Error(message)
    }
  };

let extract_series =
    (value: DHExp.t): result(list(GraphData.series), decode_error) =>
  switch (DHExp.term_of(value)) {
  | ListLit(entries) => decode_series_list(entries, [])
  | _ => Error("Graph data must be a list of series")
  };

let interpret_tuple_payload =
    (elements: list(DHExp.t)): result(GraphData.t, decode_error) => {
  let rec split_last =
          (items: list(DHExp.t), acc: list(DHExp.t))
          : result((list(DHExp.t), DHExp.t), decode_error) =>
    switch (items) {
    | [] => Error("Tuple payload requires at least one element for series")
    | [last] => Ok((List.rev(acc), last))
    | [head, ...tail] => split_last(tail, [head, ...acc])
    };

  switch (split_last(elements, [])) {
  | Error(message) => Error(message)
  | Ok((meta_fields, series_raw)) =>
    switch (extract_series(series_raw)) {
    | Error(message) => Error(message)
    | Ok(series) =>
      let rec collect_meta =
              (
                fields: list(DHExp.t),
                current: (option(string), option(string), option(string)),
              )
              : result(
                  (option(string), option(string), option(string)),
                  decode_error,
                ) =>
        switch (fields, current) {
        | ([], (title, x_label, y_label)) => Ok((title, x_label, y_label))
        | ([field, ...rest], (title, x_label, y_label)) =>
          switch (string_of_value(field)) {
          | Some(text) =>
            if (title == None) {
              collect_meta(rest, (Some(text), x_label, y_label));
            } else if (x_label == None) {
              collect_meta(rest, (title, Some(text), y_label));
            } else if (y_label == None) {
              collect_meta(rest, (title, x_label, Some(text)));
            } else {
              Error("Too many metadata fields in tuple payload");
            }
          | None => Error("Tuple metadata must be strings")
          }
        };

      switch (collect_meta(meta_fields, (None, None, None))) {
      | Error(message) => Error(message)
      | Ok((title, x_label, y_label)) =>
        let rec compute_ranges =
                (
                  remaining: list(GraphData.series),
                  x_range: (float, float),
                  y_range: (float, float),
                )
                : (float, float, float, float) =>
          switch (remaining) {
          | [] => (fst(x_range), snd(x_range), fst(y_range), snd(y_range))
          | [series_entry, ...series_tail] =>
            let rec fold_points =
                    (
                      points: list(GraphData.point),
                      current_x: (float, float),
                      current_y: (float, float),
                    )
                    : (float, float, float, float) =>
              switch (points) {
              | [] => compute_ranges(series_tail, current_x, current_y)
              | [point, ...rest_points] =>
                let (min_x, max_x) = current_x;
                let (min_y, max_y) = current_y;
                let next_x = (
                  Float.min(min_x, point.x),
                  Float.max(max_x, point.x),
                );
                let next_y = (
                  Float.min(min_y, point.y),
                  Float.max(max_y, point.y),
                );
                fold_points(rest_points, next_x, next_y);
              };

            fold_points(series_entry.points, x_range, y_range);
          };

        let initial_range = (Float.infinity, Float.neg_infinity);
        let (min_x, max_x, min_y, max_y) =
          compute_ranges(series, initial_range, initial_range);

        let normalize_range =
            ((min_value, max_value): (float, float)): (float, float) =>
          if (Float.is_finite(min_value)
              && Float.is_finite(max_value)
              && min_value <= max_value) {
            if (min_value == max_value) {
              let padding =
                if (min_value == 0.) {
                  1.;
                } else {
                  Float.abs(min_value) *. 0.05;
                };
              (min_value -. padding, max_value +. padding);
            } else {
              let span = max_value -. min_value;
              let pad = span *. 0.05;
              (min_value -. pad, max_value +. pad);
            };
          } else {
            (0., 1.);
          };

        let x_bounds = normalize_range((min_x, max_x));
        let y_bounds = normalize_range((min_y, max_y));

        Ok({
          series,
          title,
          x_label,
          y_label,
          x_bounds,
          y_bounds,
        });
      };
    }
  };
};

let decode_graph_data = (value: DHExp.t): result(GraphData.t, decode_error) =>
  switch (DHExp.term_of(value)) {
  | ListLit(_) => interpret_tuple_payload([value])
  | Tuple(elements) => interpret_tuple_payload(elements)
  | _ =>
    Error("Graph projector expects a tuple or list describing series data")
  };

let select_sample = (info: info): option(Dynamics.Probe.Closure.t) =>
  switch (info.dynamics) {
  | Some([sample, ..._]) => Some(sample)
  | _ => None
  };

module Palette = {
  let colors: array(string) = [|
    "#1f77b4",
    "#ff7f0e",
    "#2ca02c",
    "#d62728",
    "#9467bd",
    "#8c564b",
    "#e377c2",
    "#7f7f7f",
    "#bcbd22",
    "#17becf",
  |];

  let color_for = (index: int): string => {
    let palette_length = Array.length(colors);
    if (palette_length == 0) {
      "#000";
    } else {
      Array.unsafe_get(colors, index mod palette_length);
    };
  };
};

module Rendering = {
  let svg_width: float = 240.;
  let svg_height: float = 140.;
  let margin_left: float = 56.;
  let margin_right: float = 16.;
  let margin_top: float = 26.;
  let margin_bottom: float = 36.;

  let inner_width = (): float => svg_width -. margin_left -. margin_right;
  let inner_height = (): float => svg_height -. margin_top -. margin_bottom;

  let scale_value =
      (
        ~value: float,
        ~min_value: float,
        ~max_value: float,
        ~pixel_min: float,
        ~pixel_max: float,
      )
      : float =>
    if (max_value == min_value) {
      (pixel_min +. pixel_max) /. 2.;
    } else {
      let ratio = (value -. min_value) /. (max_value -. min_value);
      pixel_min +. ratio *. (pixel_max -. pixel_min);
    };

  let y_scale = (~value: float, ~bounds: (float, float)): float => {
    let (min_value, max_value) = bounds;
    let bottom = svg_height -. margin_bottom;
    let top = margin_top;
    scale_value(
      ~value,
      ~min_value,
      ~max_value,
      ~pixel_min=bottom,
      ~pixel_max=top,
    );
  };

  let x_scale = (~value: float, ~bounds: (float, float)): float => {
    let (min_value, max_value) = bounds;
    let left = margin_left;
    let right = svg_width -. margin_right;
    scale_value(
      ~value,
      ~min_value,
      ~max_value,
      ~pixel_min=left,
      ~pixel_max=right,
    );
  };

  let polyline_points_attribute =
      (
        series: GraphData.series,
        bounds: (float, float),
        y_bounds: (float, float),
      )
      : string => {
    let buffer: Stdlib.Buffer.t = Stdlib.Buffer.create(128);
    let rec add_point = (points: list(GraphData.point)): unit =>
      switch (points) {
      | [] => ()
      | [point, ...rest] =>
        let x_pixel = x_scale(~value=point.x, ~bounds);
        let y_pixel = y_scale(~value=point.y, ~bounds=y_bounds);
        Stdlib.Buffer.add_string(
          buffer,
          Printf.sprintf("%f,%f", x_pixel, y_pixel),
        );
        if (rest != []) {
          Stdlib.Buffer.add_char(buffer, ' ');
        };
        add_point(rest);
      };
    add_point(series.points);
    Stdlib.Buffer.contents(buffer);
  };

  let axis_ticks = (~bounds: (float, float), ~count: int): list(float) =>
    if (count <= 1) {
      [fst(bounds)];
    } else {
      let (min_value, max_value) = bounds;
      let step = (max_value -. min_value) /. Float.of_int(count - 1);
      let rec loop = (index: int, acc: list(float)): list(float) =>
        if (index >= count) {
          List.rev(acc);
        } else {
          let value = min_value +. Float.of_int(index) *. step;
          loop(index + 1, [value, ...acc]);
        };
      loop(0, []);
    };

  let tick_label = (~value: float): string =>
    if (Float.abs(value) >= 1000.) {
      Printf.sprintf("%.1e", value);
    } else if (Float.abs(value) < 1e-2 && value != 0.) {
      Printf.sprintf("%.2e", value);
    } else {
      Printf.sprintf("%.2f", value);
    };

  let render_axes = (data: GraphData.t): Node.t => {
    let x_bounds = data.x_bounds;
    let y_bounds = data.y_bounds;
    let ticks_x = axis_ticks(~bounds=x_bounds, ~count=5);
    let ticks_y = axis_ticks(~bounds=y_bounds, ~count=5);

    let render_x_tick = (tick_value: float): Node.t =>
      Node.create_svg(
        "g",
        ~attrs=[],
        [
          Node.create_svg(
            "line",
            ~attrs=
              Attr.[
                create(
                  "x1",
                  Printf.sprintf(
                    "%f",
                    x_scale(~value=tick_value, ~bounds=x_bounds),
                  ),
                ),
                create(
                  "y1",
                  Printf.sprintf("%f", svg_height -. margin_bottom),
                ),
                create(
                  "x2",
                  Printf.sprintf(
                    "%f",
                    x_scale(~value=tick_value, ~bounds=x_bounds),
                  ),
                ),
                create(
                  "y2",
                  Printf.sprintf("%f", svg_height -. margin_bottom +. 4.),
                ),
                create("class", "graph-axis-tick"),
              ],
            [],
          ),
          Node.create_svg(
            "text",
            ~attrs=
              Attr.[
                create(
                  "x",
                  Printf.sprintf(
                    "%f",
                    x_scale(~value=tick_value, ~bounds=x_bounds),
                  ),
                ),
                create(
                  "y",
                  Printf.sprintf("%f", svg_height -. margin_bottom +. 16.),
                ),
                create("text-anchor", "middle"),
                create("class", "graph-axis-label"),
              ],
            [Node.text(tick_label(~value=tick_value))],
          ),
        ],
      );

    let render_y_tick = (tick_value: float): Node.t =>
      Node.create_svg(
        "g",
        ~attrs=[],
        [
          Node.create_svg(
            "line",
            ~attrs=
              Attr.[
                create("x1", Printf.sprintf("%f", margin_left)),
                create(
                  "y1",
                  Printf.sprintf(
                    "%f",
                    y_scale(~value=tick_value, ~bounds=y_bounds),
                  ),
                ),
                create("x2", Printf.sprintf("%f", margin_left -. 4.)),
                create(
                  "y2",
                  Printf.sprintf(
                    "%f",
                    y_scale(~value=tick_value, ~bounds=y_bounds),
                  ),
                ),
                create("class", "graph-axis-tick"),
              ],
            [],
          ),
          Node.create_svg(
            "text",
            ~attrs=
              Attr.[
                create("x", Printf.sprintf("%f", margin_left -. 8.)),
                create(
                  "y",
                  Printf.sprintf(
                    "%f",
                    y_scale(~value=tick_value, ~bounds=y_bounds) +. 4.,
                  ),
                ),
                create("text-anchor", "end"),
                create("class", "graph-axis-label"),
              ],
            [Node.text(tick_label(~value=tick_value))],
          ),
        ],
      );

    let x_axis =
      Node.create_svg(
        "line",
        ~attrs=
          Attr.[
            create("x1", Printf.sprintf("%f", margin_left)),
            create("y1", Printf.sprintf("%f", svg_height -. margin_bottom)),
            create("x2", Printf.sprintf("%f", svg_width -. margin_right)),
            create("y2", Printf.sprintf("%f", svg_height -. margin_bottom)),
            create("class", "graph-axis"),
          ],
        [],
      );

    let y_axis =
      Node.create_svg(
        "line",
        ~attrs=
          Attr.[
            create("x1", Printf.sprintf("%f", margin_left)),
            create("y1", Printf.sprintf("%f", margin_top)),
            create("x2", Printf.sprintf("%f", margin_left)),
            create("y2", Printf.sprintf("%f", svg_height -. margin_bottom)),
            create("class", "graph-axis"),
          ],
        [],
      );

    Node.create_svg(
      "g",
      ~attrs=[],
      [
        x_axis,
        y_axis,
        Node.create_svg("g", ~attrs=[], List.map(render_x_tick, ticks_x)),
        Node.create_svg("g", ~attrs=[], List.map(render_y_tick, ticks_y)),
      ],
    );
  };

  let render_series_group = (data: GraphData.t): list(Node.t) => {
    let series_list = data.series;
    let rec loop =
            (entries: list(GraphData.series), index: int, acc: list(Node.t))
            : list(Node.t) =>
      switch (entries) {
      | [] => List.rev(acc)
      | [series, ...rest] =>
        let color = Palette.color_for(index);
        let attrs =
          Attr.[
            create("fill", "none"),
            create("stroke", color),
            create("stroke-width", "2"),
            create("class", "graph-series-line"),
            create(
              "points",
              polyline_points_attribute(series, data.x_bounds, data.y_bounds),
            ),
          ];
        let polyline = Node.create_svg("polyline", ~attrs, []);
        loop(rest, index + 1, [polyline, ...acc]);
      };
    loop(series_list, 0, []);
  };

  let render_legend = (data: GraphData.t): option(Node.t) => {
    let series_list = data.series;
    let rec entries_with_index =
            (entries: list(GraphData.series), index: int, acc: list(Node.t))
            : list(Node.t) =>
      switch (entries) {
      | [] => List.rev(acc)
      | [series, ...rest] =>
        let color = Palette.color_for(index);
        let swatch =
          Node.create_svg(
            "rect",
            ~attrs=
              Attr.[
                create("x", "0"),
                create(
                  "y",
                  Printf.sprintf("%f", Float.of_int(index) *. 16.),
                ),
                create("width", "12"),
                create("height", "12"),
                create("fill", color),
                create("class", "graph-legend-swatch"),
              ],
            [],
          );
        let label =
          Node.create_svg(
            "text",
            ~attrs=
              Attr.[
                create("x", "18"),
                create(
                  "y",
                  Printf.sprintf("%f", Float.of_int(index) *. 16. +. 10.),
                ),
                create("class", "graph-legend-label"),
              ],
            [Node.text(series.label)],
          );
        entries_with_index(rest, index + 1, [label, swatch, ...acc]);
      };

    switch (series_list) {
    | [] => None
    | _ =>
      Some(
        Node.create_svg(
          "g",
          ~attrs=
            Attr.[
              create(
                "transform",
                Printf.sprintf(
                  "translate(%f,%f)",
                  svg_width -. margin_right -. 100.,
                  margin_top,
                ),
              ),
              create("class", "graph-legend"),
            ],
          entries_with_index(series_list, 0, []),
        ),
      )
    };
  };

  let render_labels = (data: GraphData.t): list(Node.t) => {
    let title_node =
      switch (data.title) {
      | None => None
      | Some(text) =>
        Some(
          Node.create_svg(
            "text",
            ~attrs=
              Attr.[
                create("x", Printf.sprintf("%f", svg_width /. 2.)),
                create("y", Printf.sprintf("%f", margin_top -. 6.)),
                create("text-anchor", "middle"),
                create("class", "graph-title"),
              ],
            [Node.text(text)],
          ),
        )
      };

    let x_label_node =
      switch (data.x_label) {
      | None => None
      | Some(text) =>
        Some(
          Node.create_svg(
            "text",
            ~attrs=
              Attr.[
                create("x", Printf.sprintf("%f", svg_width /. 2.)),
                create(
                  "y",
                  Printf.sprintf("%f", svg_height -. margin_bottom +. 24.),
                ),
                create("text-anchor", "middle"),
                create("class", "graph-axis-caption"),
              ],
            [Node.text(text)],
          ),
        )
      };

    let y_label_node =
      switch (data.y_label) {
      | None => None
      | Some(text) =>
        let x_pos = margin_left -. 36.;
        let y_pos = margin_top +. inner_height() /. 2.;
        Some(
          Node.create_svg(
            "text",
            ~attrs=
              Attr.[
                create("x", Printf.sprintf("%f", x_pos)),
                create("y", Printf.sprintf("%f", y_pos)),
                create("text-anchor", "middle"),
                create(
                  "transform",
                  Printf.sprintf("rotate(-90 %f %f)", x_pos, y_pos),
                ),
                create("class", "graph-axis-caption"),
              ],
            [Node.text(text)],
          ),
        );
      };

    let rec gather =
            (remaining: list(option(Node.t)), acc: list(Node.t))
            : list(Node.t) =>
      switch (remaining) {
      | [] => List.rev(acc)
      | [Some(node), ...rest] => gather(rest, [node, ...acc])
      | [None, ...rest] => gather(rest, acc)
      };

    gather([title_node, x_label_node, y_label_node], []);
  };

  let chart = (data: GraphData.t): Node.t => {
    let base_children: list(Node.t) = [
      Node.create_svg(
        "rect",
        ~attrs=
          Attr.[
            create("x", "0"),
            create("y", "0"),
            create("width", Printf.sprintf("%f", svg_width)),
            create("height", Printf.sprintf("%f", svg_height)),
            create("class", "graph-background"),
          ],
        [],
      ),
      render_axes(data),
      Node.create_svg("g", ~attrs=[], render_series_group(data)),
      Node.create_svg("g", ~attrs=[], render_labels(data)),
    ];

    let legend_children: list(Node.t) =
      switch (render_legend(data)) {
      | None => []
      | Some(legend_node) => [legend_node]
      };

    Node.create_svg(
      "svg",
      ~attrs=
        Attr.[
          create(
            "viewBox",
            Printf.sprintf("0 0 %f %f", svg_width, svg_height),
          ),
          create("class", "graph-chart"),
        ],
      base_children @ legend_children,
    );
  };
};

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = (_: Language.Any.t): option(model) => Some();

  let focusable = Focusable.non;

  let dynamics = true;

  let placeholder = (_model: model, _info: info): ProjectorCore.Shape.t =>
    ProjectorCore.Shape.{
      vertical: Block(12),
      horizontal: 56,
    };

  let update = (model: model, _info: info, _action: action): model => model;

  let build_error_view = (message: string): Node.t =>
    Node.div(
      ~attrs=[Attr.classes(["graph-projector", "graph-error"])],
      [Node.text(message)],
    );

  let build_view = (data: GraphData.t): Node.t =>
    Node.div(
      ~attrs=[Attr.classes(["graph-projector", "graph-has-data"])],
      [Rendering.chart(data)],
    );

  let interpret_sample =
      (sample: Dynamics.Probe.Closure.t): result(GraphData.t, decode_error) =>
    decode_graph_data(sample.value);

  let view = ({info, status, _}: View.args(model, action)): View.t => {
    let indicated_class: list(string) =
      switch (status.indication) {
      | Some(_) => ["indicated"]
      | None => []
      };
    let class_list: list(string) = ["projector", "graph"] @ indicated_class;

    let node =
      switch (select_sample(info)) {
      | None => build_error_view("Data loading")
      | Some(sample) =>
        switch (interpret_sample(sample)) {
        | Ok(data) => build_view(data)
        | Error(message) => build_error_view(message)
        }
      };

    View.mk(Node.div(~attrs=[Attr.classes(class_list)], [node]));
  };
};
