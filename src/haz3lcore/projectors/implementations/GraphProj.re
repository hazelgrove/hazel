open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Language;

module GraphData = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type point = {
    x: float,
    y: float,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type series = {
    label: string,
    points: list(point),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    series: list(series),
    title: option(string),
    x_label: option(string),
    y_label: option(string),
    x_bounds: (float, float),
    y_bounds: (float, float),
  };
};

type decode_warning = string;

type decoded_graph = {
  graph: option(GraphData.t),
  warnings: list(decode_warning),
};

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

let decode_graph_data = (value: DHExp.t): decoded_graph => {
  let warnings = ref([]);
  let warn = msg => warnings := [msg, ...warnings^];

  let decode_point =
      (~series_label: string, ~point_index: int, point_value: DHExp.t)
      : option(GraphData.point) =>
    switch (DHExp.term_of(point_value)) {
    | Tuple([x_exp, y_exp]) =>
      switch (float_of_number(x_exp), float_of_number(y_exp)) {
      | (Some(x), Some(y)) =>
        Some({
          x,
          y,
        })
      | _ =>
        warn(
          Printf.sprintf(
            "Series \"%s\" point #%d must contain numeric x and y values.",
            series_label,
            point_index + 1,
          ),
        );
        None;
      }
    | _ =>
      warn(
        Printf.sprintf(
          "Series \"%s\" point #%d is not a two-element tuple; ignoring.",
          series_label,
          point_index + 1,
        ),
      );
      None;
    };

  let decode_points =
      (~series_label: string, points_value: DHExp.t): list(GraphData.point) =>
    switch (DHExp.term_of(points_value)) {
    | ListLit(point_values) =>
      let rec loop = (values, index, acc) =>
        switch (values) {
        | [] => List.rev(acc)
        | [value, ...rest] =>
          let acc =
            switch (decode_point(~series_label, ~point_index=index, value)) {
            | Some(point) => [point, ...acc]
            | None => acc
            };
          loop(rest, index + 1, acc);
        };
      loop(point_values, 0, []);
    | _ =>
      warn(
        Printf.sprintf(
          "Series \"%s\" data must be a list of (x, y) tuples.",
          series_label,
        ),
      );
      [];
    };

  let decode_series_entry =
      (~series_index: int, entry_value: DHExp.t): option(GraphData.series) =>
    switch (DHExp.term_of(entry_value)) {
    | Tuple([label_exp, points_exp]) =>
      let fallback_label = Printf.sprintf("Series %d", series_index + 1);
      let label =
        switch (string_of_value(label_exp)) {
        | Some(text) when String.trim(text) != "" => text
        | Some(_) =>
          warn(
            Printf.sprintf(
              "Series #%d has an empty label; defaulting to \"%s\".",
              series_index + 1,
              fallback_label,
            ),
          );
          fallback_label;
        | None =>
          warn(
            Printf.sprintf(
              "Series #%d label must be a string; defaulting to \"%s\".",
              series_index + 1,
              fallback_label,
            ),
          );
          fallback_label;
        };
      let points = decode_points(~series_label=label, points_exp);
      switch (points) {
      | [] =>
        warn(
          Printf.sprintf(
            "Series \"%s\" does not contain any valid points and was omitted.",
            label,
          ),
        );
        None;
      | _ =>
        Some(
          GraphData.{
            label,
            points,
          },
        )
      };
    | _ =>
      warn(
        Printf.sprintf(
          "Series #%d is not a (label, points) tuple; ignoring entry.",
          series_index + 1,
        ),
      );
      None;
    };

  let decode_series_list = (series_value: DHExp.t): list(GraphData.series) =>
    switch (DHExp.term_of(series_value)) {
    | ListLit(entries) =>
      let rec loop = (values, index, acc) =>
        switch (values) {
        | [] => List.rev(acc)
        | [value, ...rest] =>
          let acc =
            switch (decode_series_entry(~series_index=index, value)) {
            | Some(series) => [series, ...acc]
            | None => acc
            };
          loop(rest, index + 1, acc);
        };
      loop(entries, 0, []);
    | _ =>
      warn("Graph data must end with a list of (label, points) tuples.");
      [];
    };

  let parse_metadata = (fields: list(DHExp.t)) => {
    let rec loop = (remaining, index, assigned, title, x_label, y_label) =>
      switch (remaining) {
      | [] => (title, x_label, y_label)
      | [field, ...rest] =>
        switch (string_of_value(field)) {
        | Some(text) =>
          switch (assigned) {
          | 0 => loop(rest, index + 1, 1, Some(text), x_label, y_label)
          | 1 => loop(rest, index + 1, 2, title, Some(text), y_label)
          | 2 => loop(rest, index + 1, 3, title, x_label, Some(text))
          | _ =>
            warn(
              Printf.sprintf(
                "Ignoring extra metadata field #%d; only title, x label, and y label are supported.",
                index + 1,
              ),
            );
            loop(rest, index + 1, assigned, title, x_label, y_label);
          }
        | None =>
          warn(
            Printf.sprintf(
              "Metadata field #%d is not a string and was ignored.",
              index + 1,
            ),
          );
          loop(rest, index + 1, assigned, title, x_label, y_label);
        }
      };
    loop(fields, 0, 0, None, None, None);
  };

  let (meta_fields, series_value_opt) =
    switch (DHExp.term_of(value)) {
    | Tuple(elements) =>
      let rec split_last = (items, acc) =>
        switch (items) {
        | [] => None
        | [last] => Some((List.rev(acc), last))
        | [head, ...tail] => split_last(tail, [head, ...acc])
        };
      switch (split_last(elements, [])) {
      | Some((meta, series_value)) => (meta, Some(series_value))
      | None =>
        warn(
          "Graph tuple must contain at least one element for the series list.",
        );
        ([], None);
      };
    | ListLit(_) => ([], Some(value))
    | _ =>
      warn(
        "Graph projector expects a tuple or list describing the series data.",
      );
      ([], None);
    };

  let (title, x_label, y_label) = parse_metadata(meta_fields);
  let series =
    switch (series_value_opt) {
    | Some(series_value) => decode_series_list(series_value)
    | None => []
    };

  let graph =
    switch (series) {
    | [] =>
      warn("No valid series found to render.");
      None;
    | _ =>
      let update_bounds =
          ((min_x, max_x, min_y, max_y), point: GraphData.point) => (
        Float.min(min_x, point.x),
        Float.max(max_x, point.x),
        Float.min(min_y, point.y),
        Float.max(max_y, point.y),
      );
      let initial = (
        Float.infinity,
        Float.neg_infinity,
        Float.infinity,
        Float.neg_infinity,
      );
      let (min_x, max_x, min_y, max_y) =
        List.fold_left(
          (bounds, series_entry: GraphData.series) =>
            List.fold_left(update_bounds, bounds, series_entry.points),
          initial,
          series,
        );

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
          warn(
            "Could not determine numeric bounds for the provided data; using a default range.",
          );
          (0., 1.);
        };

      Some(
        GraphData.{
          series,
          title,
          x_label,
          y_label,
          x_bounds: normalize_range((min_x, max_x)),
          y_bounds: normalize_range((min_y, max_y)),
        },
      );
    };

  {
    graph,
    warnings: List.rev(warnings^),
  };
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
  let svg_width: float = 500.;
  let svg_height: float = 250.;
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

let warning_indicator = (warnings: list(string)): option(Node.t) =>
  switch (warnings) {
  | [] => None
  | _ =>
    let tooltip = String.concat("\n", warnings);
    Some(
      Node.div(
        ~attrs=
          Attr.[
            classes(["graph-warning-indicator"]),
            title(tooltip),
            create("role", "img"),
            create("aria-label", tooltip),
          ],
        [Node.text("⚠")],
      ),
    );
  };

let wrap_with_warnings =
    (~classes: list(string), content: list(Node.t), warnings: list(string))
    : Node.t => {
  let indicator_child =
    switch (warning_indicator(warnings)) {
    | None => []
    | Some(node) => [node]
    };
  Node.div(~attrs=[Attr.classes(classes)], indicator_child @ content);
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

  let build_error_view = (~message: string, ~warnings: list(string)): Node.t =>
    wrap_with_warnings(
      ~classes=["graph-projector", "graph-error"],
      [Node.text(message)],
      warnings,
    );

  let build_view = (data: GraphData.t, warnings: list(string)): Node.t =>
    wrap_with_warnings(
      ~classes=["graph-projector", "graph-has-data"],
      [Rendering.chart(data)],
      warnings,
    );

  let interpret_sample = (sample: Dynamics.Probe.Closure.t): decoded_graph =>
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
      | None =>
        build_error_view(~message="Awaiting runtime data", ~warnings=[])
      | Some(sample) =>
        let decoded = interpret_sample(sample);
        switch (decoded.graph) {
        | Some(data) => build_view(data, decoded.warnings)
        | None =>
          build_error_view(
            ~message="Unable to render graph data",
            ~warnings=decoded.warnings,
          )
        };
      };

    View.mk(Node.div(~attrs=[Attr.classes(class_list)], [node]));
  };
};
