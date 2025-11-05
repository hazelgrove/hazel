open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Js_of_ocaml;

module GraphData = GraphProj.GraphData;

type plot_row = {
  series_label: string,
  x: float,
  y: float,
};

let plot_runtime_available = (): bool =>
  Js.to_bool(
    Js.Unsafe.coerce(Js.Unsafe.eval_string("typeof Plot !== 'undefined'")),
  );

let plot_row_to_js = (row: plot_row): Js.Unsafe.any =>
  Js.Unsafe.obj([|
    ("series", Js.Unsafe.inject(Js.string(row.series_label))),
    ("x", Js.Unsafe.inject(Js.number_of_float(row.x))),
    ("y", Js.Unsafe.inject(Js.number_of_float(row.y))),
  |]);

let rec plot_accumulate_point_rows =
        (label: string, points: list(GraphData.point), acc: list(plot_row))
        : list(plot_row) =>
  switch (points) {
  | [] => acc
  | [point, ...rest] =>
    let next_row: plot_row = {
      series_label: label,
      x: point.x,
      y: point.y,
    };
    plot_accumulate_point_rows(label, rest, [next_row, ...acc]);
  };

let rec plot_accumulate_series_rows =
        (series_list: list(GraphData.series), acc: list(plot_row))
        : list(plot_row) =>
  switch (series_list) {
  | [] => acc
  | [series_entry, ...rest] =>
    let updated: list(plot_row) =
      plot_accumulate_point_rows(
        series_entry.label,
        series_entry.points,
        acc,
      );
    plot_accumulate_series_rows(rest, updated);
  };

let plot_rows_to_js_array =
    (rows: list(plot_row)): Js.t(Js.js_array(Js.Unsafe.any)) => {
  let rec to_list = (remaining: list(plot_row)): list(Js.Unsafe.any) =>
    switch (remaining) {
    | [] => []
    | [item, ...rest] => [plot_row_to_js(item), ...to_list(rest)]
    };
  Js.array(Array.of_list(to_list(rows)));
};

let plot_clear_children = (element: Js.t(Dom_html.element)): unit => {
  let rec loop = (): unit =>
    switch (Js.Opt.to_option(element##.firstChild)) {
    | None => ()
    | Some(child) =>
      ignore(element##removeChild(child));
      loop();
    };
  loop();
  element##.textContent := Js.Opt.empty;
};

let plot_inject_float = (value: float): Js.Unsafe.any =>
  Js.Unsafe.inject(Js.number_of_float(value));

let plot_inject_string = (text: string): Js.Unsafe.any =>
  Js.Unsafe.inject(Js.string(text));

let plot_inject_bool = (flag: bool): Js.Unsafe.any =>
  Js.Unsafe.inject(Js.bool(flag));

let plot_set_optional_string =
    (config: Js.Unsafe.any, key: string, text_opt: option(string)): unit =>
  switch (text_opt) {
  | None => ()
  | Some(text) => Js.Unsafe.set(config, key, plot_inject_string(text))
  };

let plot_render = (data: GraphData.t, element: Js.t(Dom_html.element)): unit => {
  plot_clear_children(element);
  if (plot_runtime_available()) {
    let plot_global = Js.Unsafe.get(Js.Unsafe.global, "Plot");
    let plot_fn = (name: string): Js.Unsafe.any =>
      Js.Unsafe.get(plot_global, name);

    let reversed_rows = plot_accumulate_series_rows(data.series, []);
    let rows = List.rev(reversed_rows);
    let data_array = plot_rows_to_js_array(rows);

    let config =
      Js.Unsafe.obj([|
        ("width", plot_inject_float(500.)),
        ("height", plot_inject_float(250.)),
        ("marginLeft", plot_inject_float(56.)),
        ("marginRight", plot_inject_float(16.)),
        ("marginTop", plot_inject_float(26.)),
        ("marginBottom", plot_inject_float(36.)),
      |]);

    let (min_x, max_x) = data.x_bounds;
    let x_domain =
      Js.array([|Js.number_of_float(min_x), Js.number_of_float(max_x)|]);
    let x_config =
      Js.Unsafe.obj([|("domain", Js.Unsafe.inject(x_domain))|]);
    plot_set_optional_string(x_config, "label", data.x_label);
    Js.Unsafe.set(config, "x", x_config);

    let (min_y, max_y) = data.y_bounds;
    let y_domain =
      Js.array([|Js.number_of_float(min_y), Js.number_of_float(max_y)|]);
    let y_config =
      Js.Unsafe.obj([|("domain", Js.Unsafe.inject(y_domain))|]);
    plot_set_optional_string(y_config, "label", data.y_label);
    Js.Unsafe.set(config, "y", y_config);

    let color_config =
      Js.Unsafe.obj([|("legend", plot_inject_bool(true))|]);
    Js.Unsafe.set(config, "color", color_config);

    plot_set_optional_string(config, "title", data.title);

    let line_options =
      Js.Unsafe.obj([|
        ("x", plot_inject_string("x")),
        ("y", plot_inject_string("y")),
        ("stroke", plot_inject_string("series")),
        ("title", plot_inject_string("series")),
      |]);

    let line_mark =
      Js.Unsafe.fun_call(
        plot_fn("lineY"),
        [|Js.Unsafe.inject(data_array), Js.Unsafe.inject(line_options)|],
      );

    let marks = Js.array([|line_mark|]);
    Js.Unsafe.set(config, "marks", Js.Unsafe.inject(marks));

    let chart =
      Js.Unsafe.fun_call(plot_fn("plot"), [|Js.Unsafe.inject(config)|]);
    let chart_element: Js.t(Dom_html.element) = Js.Unsafe.coerce(chart);
    let element_node: Js.t(Dom.node) = Js.Unsafe.coerce(element);
    let chart_node: Js.t(Dom.node) = Js.Unsafe.coerce(chart_element);
    ignore(Dom.appendChild(element_node, chart_node));
  } else {
    element##.textContent :=
      Js.Opt.return(
        Js.string(
          "Observable Plot runtime unavailable. Install @observablehq/plot.",
        ),
      );
  };
};

module PlotHookInput = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = GraphData.t;
  let combine = (_left: t, right: t): t => right;
};

module PlotHookImpl = {
  module State = {
    type t = unit;
  };

  module Input = PlotHookInput;

  let init = (_input: Input.t, element: Js.t(Dom_html.element)): State.t => {
    plot_clear_children(element);
    ();
  };

  let on_mount =
      (input: Input.t, _state: State.t, element: Js.t(Dom_html.element))
      : unit =>
    plot_render(input, element);

  let update =
      (
        ~old_input: Input.t,
        ~new_input: Input.t,
        _state: State.t,
        element: Js.t(Dom_html.element),
      )
      : unit => {
    ignore(old_input);
    plot_render(new_input, element);
  };

  let destroy =
      (_input: Input.t, _state: State.t, element: Js.t(Dom_html.element))
      : unit =>
    plot_clear_children(element);
};

module PlotHook = Virtual_dom.Vdom.Attr.Hooks.Make(PlotHookImpl);

let observable_plot_attr = (data: GraphData.t): Attr.t =>
  Attr.create_hook("hazel-observable-plot", PlotHook.create(data));

let runtime_missing_message: string = "Observable Plot runtime unavailable; ensure @observablehq/plot is bundled.";

let build_plot_view = (data: GraphData.t, warnings: list(string)): Node.t => {
  let container: Node.t =
    Node.div(
      ~attrs=[
        Attr.classes(["observable-plot-canvas"]),
        observable_plot_attr(data),
      ],
      [],
    );
  GraphProj.wrap_with_warnings(
    ~classes=["graph-projector", "graph-observable-plot", "graph-has-data"],
    [container],
    warnings,
  );
};

let build_error_view = (warnings: list(string), message: string): Node.t =>
  GraphProj.wrap_with_warnings(
    ~classes=["graph-projector", "graph-error"],
    [Node.text(message)],
    warnings,
  );

let placeholder_shape =
  ProjectorCore.Shape.{
    vertical: Block(12),
    horizontal: 56,
  };

let has_plot_runtime = plot_runtime_available;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = (_: Language.Any.t): option(model) => Some();

  let focusable = Focusable.non;

  let dynamics = true;

  let placeholder = (_model: model, _info: info): ProjectorCore.Shape.t => placeholder_shape;

  let update = (model: model, _info: info, _action: action): model => model;

  let view = ({info, status, _}: View.args(model, action)): View.t => {
    let indicated_class: list(string) =
      switch (status.indication) {
      | Some(_) => ["indicated"]
      | None => []
      };

    let class_list: list(string) =
      ["projector", "observable-plot"] @ indicated_class;

    let node: Node.t =
      switch (GraphProj.select_sample(info)) {
      | None => build_error_view([], "Awaiting runtime data")
      | Some(sample) =>
        let decoded: GraphProj.decoded_graph =
          GraphProj.decode_graph_data(sample.value);
        switch (decoded.graph) {
        | None =>
          build_error_view(decoded.warnings, "Unable to render graph data")
        | Some(graph_data) =>
          if (has_plot_runtime()) {
            build_plot_view(graph_data, decoded.warnings);
          } else {
            build_error_view(decoded.warnings, runtime_missing_message);
          }
        };
      };

    View.mk(Node.div(~attrs=[Attr.classes(class_list)], [node]));
  };
};
