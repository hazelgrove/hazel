open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Js_of_ocaml;
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

let select_sample = (info: info): option(Sample.t) =>
  switch (info.dynamics) {
  | Some(x) =>
    switch (
      Language.Sample.Selection.first_related_index(
        ~trimmed=false,
        ~ap_id=None,
        x.sample_cursor,
        x.samples,
      )
    ) {
    | Some(idx) => Some(List.nth(x.samples, idx))
    | None => None
    }
  | _ => None
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

module Rendering = {
  let margin_left: float = 56.;
  let margin_right: float = 16.;
  let margin_top: float = 26.;
  let margin_bottom: float = 36.;
};

let default_width_blocks: int = 56; /* matches legacy graph placeholder */
let default_height_blocks: int = 12;

let min_width_blocks: int = 32; /* ensure margins + readable plot */
let min_height_blocks: int = 8;

let col_width = () => Util.font_metrics^.col_width;
let row_height = () => Util.font_metrics^.row_height;

let width_px_of_blocks = (blocks: int): float =>
  float_of_int(blocks) *. col_width();

let height_px_of_blocks = (blocks: int): float =>
  float_of_int(blocks) *. row_height();

let clamp_width_blocks = (blocks: int): int => max(min_width_blocks, blocks);
let clamp_height_blocks = (blocks: int): int =>
  max(min_height_blocks, blocks);

// let css_size_style = (width_blocks: int, height_blocks: int): string => {
//   let width_px = int_of_float(Float.ceil(width_px_of_blocks(width_blocks)));
//   let height_px =
//     int_of_float(Float.ceil(height_px_of_blocks(height_blocks)));
//   Printf.sprintf("width:%dpx;height:%dpx;", width_px, height_px);
// };

[@deriving (show({with_path: false}), sexp, yojson)]
type size_model = {
  width_blocks: int,
  height_blocks: int,
};

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

let plot_render =
    (
      ~width_blocks: int,
      ~height_blocks: int,
      data: GraphData.t,
      element: Js.t(Dom_html.element),
    )
    : unit => {
  plot_clear_children(element);
  if (plot_runtime_available()) {
    let plot_global = Js.Unsafe.get(Js.Unsafe.global, "Plot");
    let plot_fn = (name: string): Js.Unsafe.any =>
      Js.Unsafe.get(plot_global, name);

    let reversed_rows = plot_accumulate_series_rows(data.series, []);
    let rows = List.rev(reversed_rows);
    let data_array = plot_rows_to_js_array(rows);

    let width_px = width_px_of_blocks(width_blocks);
    let height_px = height_px_of_blocks(height_blocks);

    let config =
      Js.Unsafe.obj([|
        ("width", plot_inject_float(width_px)),
        ("height", plot_inject_float(height_px)),
        ("marginLeft", plot_inject_float(Rendering.margin_left)),
        ("marginRight", plot_inject_float(Rendering.margin_right)),
        ("marginTop", plot_inject_float(Rendering.margin_top)),
        ("marginBottom", plot_inject_float(Rendering.margin_bottom)),
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
  type t = {
    graph: GraphData.t,
    width_blocks: int,
    height_blocks: int,
  };
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
    plot_render(
      ~width_blocks=input.width_blocks,
      ~height_blocks=input.height_blocks,
      input.graph,
      element,
    );

  let update =
      (
        ~old_input: Input.t,
        ~new_input: Input.t,
        _state: State.t,
        element: Js.t(Dom_html.element),
      )
      : unit => {
    ignore(old_input);
    plot_render(
      ~width_blocks=new_input.width_blocks,
      ~height_blocks=new_input.height_blocks,
      new_input.graph,
      element,
    );
  };

  let destroy =
      (_input: Input.t, _state: State.t, element: Js.t(Dom_html.element))
      : unit =>
    plot_clear_children(element);
};

module PlotHook = Virtual_dom.Vdom.Attr.Hooks.Make(PlotHookImpl);

let observable_plot_attr = (payload: PlotHookInput.t): Attr.t =>
  Attr.create_hook("hazel-ObservablePlot", PlotHook.create(payload));

module ResizeState = {
  type t = {
    pointer_id: int,
    capture_target: Js.t(Dom_html.element),
    start_client_x: float,
    start_client_y: float,
    start_width_blocks: int,
    start_height_blocks: int,
  };

  let active: ref(option(t)) = ref(None);
  let last_sent: ref(option((int, int))) = ref(None);
  let dispatch: ref(option((int, int) => Effect.t(unit))) = ref(None);

  let reset = (): unit => {
    active := None;
    last_sent := None;
    dispatch := None;
  };
};

let resize_pointerdown =
    (
      ~dispatch: (int, int) => Effect.t(unit),
      model: size_model,
      event: Js.t(Dom_html.pointerEvent),
    ) =>
  if (!Js.to_bool(event##.metaKey)) {
    ResizeState.reset();
    Effect.Ignore;
  } else {
    let target = Js.Opt.get(event##.currentTarget, () => failwith("resize"));
    let element: Js.t(Dom_html.element) = Js.Unsafe.coerce(target);
    JsUtil.setPointerCapture(element, event##.pointerId);
    ResizeState.dispatch := Some(dispatch);
    ResizeState.active :=
      Some({
        pointer_id: event##.pointerId,
        capture_target: element,
        start_client_x: float_of_int(event##.clientX),
        start_client_y: float_of_int(event##.clientY),
        start_width_blocks: model.width_blocks,
        start_height_blocks: model.height_blocks,
      });
    ResizeState.last_sent := None;
    Effect.Many([Effect.Stop_propagation, Effect.Prevent_default]);
  };

let resize_pointermove = (event: Js.t(Dom_html.pointerEvent)) => {
  switch (ResizeState.active^, ResizeState.dispatch^) {
  | (Some(state), Some(dispatch)) when state.pointer_id == event##.pointerId =>
    let delta_x = float_of_int(event##.clientX) -. state.start_client_x;
    let delta_y = float_of_int(event##.clientY) -. state.start_client_y;
    let start_width_f = float_of_int(state.start_width_blocks);
    let start_height_f = float_of_int(state.start_height_blocks);
    let desired_width = start_width_f +. delta_x /. col_width();
    let desired_height = start_height_f +. delta_y /. row_height();
    let new_width_blocks =
      if (desired_width >= start_width_f) {
        int_of_float(Float.ceil(desired_width));
      } else {
        int_of_float(Float.floor(desired_width));
      };
    let new_height_blocks =
      if (desired_height >= start_height_f) {
        int_of_float(Float.ceil(desired_height));
      } else {
        int_of_float(Float.floor(desired_height));
      };
    let clamped_width = clamp_width_blocks(new_width_blocks);
    let clamped_height = clamp_height_blocks(new_height_blocks);
    let pair = (clamped_width, clamped_height);
    if (ResizeState.last_sent^ == Some(pair)) {
      Effect.Ignore;
    } else {
      ResizeState.last_sent := Some(pair);
      Effect.Many([
        dispatch(clamped_width, clamped_height),
        Effect.Stop_propagation,
        Effect.Prevent_default,
      ]);
    };
  | _ => Effect.Ignore
  };
};

let finish_resize = (event: Js.t(Dom_html.pointerEvent)): Effect.t(unit) => {
  switch (ResizeState.active^) {
  | Some(state) when state.pointer_id == event##.pointerId =>
    if (JsUtil.hasPointerCapture(state.capture_target, state.pointer_id)) {
      JsUtil.releasePointerCapture(state.capture_target, state.pointer_id);
    };
    ResizeState.reset();
    Effect.Many([Effect.Stop_propagation, Effect.Prevent_default]);
  | _ => Effect.Ignore
  };
};

let dom_event_target: Js.t(Dom_html.eventTarget) =
  Js.Unsafe.coerce(Dom_html.document);

let _pointermove_listener =
  Dom_html.addEventListener(
    dom_event_target,
    Dom_html.Event.make("pointermove"),
    Dom.full_handler((_, event) => {
      Virtual_dom.Vdom.Effect.Expert.handle(
        event,
        resize_pointermove(event),
      );
      Js._false;
    }),
    Js._false,
  );

let _pointerup_listener =
  Dom_html.addEventListener(
    dom_event_target,
    Dom_html.Event.make("pointerup"),
    Dom.full_handler((_, event) => {
      Virtual_dom.Vdom.Effect.Expert.handle(event, finish_resize(event));
      Js._false;
    }),
    Js._false,
  );

let runtime_missing_message: string = "Observable Plot runtime unavailable; ensure @observablehq/plot is bundled.";

let build_plot_view =
    (
      ~model: size_model,
      ~dispatch,
      data: GraphData.t,
      warnings: list(string),
    )
    : Node.t => {
  //let shell_style = css_size_style(model.width_blocks, model.height_blocks);
  let chart =
    Node.div(
      ~attrs=[
        Attr.classes(["graph-chart", "ObservablePlot-chart"]),
        Attr.create("style", "width:100%;height:100%;"),
        observable_plot_attr({
          graph: data,
          width_blocks: model.width_blocks,
          height_blocks: model.height_blocks,
        }),
      ],
      [],
    );
  let handle =
    Node.div(
      ~attrs=[
        Attr.classes(["ObservablePlot-resize-handle"]),
        Attr.on_pointerdown(resize_pointerdown(~dispatch, model)),
      ],
      [],
    );
  let shell =
    Node.div(
      ~attrs=[
        Attr.classes(["ObservablePlot-shell"]),
        //Attr.create("style", shell_style),
      ],
      [chart, handle],
    );
  wrap_with_warnings(
    ~classes=["graph-projector", "graph-ObservablePlot", "graph-has-data"],
    [shell],
    warnings,
  );
};

let build_error_view = (warnings: list(string), message: string): Node.t => {
  ResizeState.reset();
  wrap_with_warnings(
    ~classes=["graph-projector", "graph-error"],
    [Node.text(message)],
    warnings,
  );
};

let has_plot_runtime = plot_runtime_available;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = size_model;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | ResizeTo(int, int);

  let default_model: model = {
    width_blocks: default_width_blocks,
    height_blocks: default_height_blocks,
  };

  let derived_model_of_sexp = model_of_sexp;
  let model_of_sexp = (sexp: Sexplib.Sexp.t): model =>
    switch (derived_model_of_sexp(sexp)) {
    | exception _ => default_model
    | m => m
    };

  let init = (_: Language.Any.t): option(model) => Some(default_model);

  let focusable = Focusable.non;

  let dynamics = true;

  let placeholder = (model: model, _info: info): ProjectorCore.Shape.t =>
    ProjectorCore.Shape.{
      vertical: Block(model.height_blocks),
      horizontal: model.width_blocks,
    };

  let update = (_model: model, _info: info, action: action): model =>
    switch (action) {
    | ResizeTo(width_blocks, height_blocks) => {
        width_blocks: clamp_width_blocks(width_blocks),
        height_blocks: clamp_height_blocks(height_blocks),
      }
    };

  let view =
      ({model, info, local, status, _}: View.args(model, action)): View.t => {
    let indicated_class: list(string) =
      switch (status.indication) {
      | Some(_) => ["indicated"]
      | None => []
      };

    let class_list: list(string) =
      ["projector", "ObservablePlot"] @ indicated_class;

    let node: Node.t =
      switch (select_sample(info)) {
      | None => build_error_view([], "Awaiting runtime data")
      | Some(sample) =>
        let decoded: decoded_graph = decode_graph_data(sample.value);
        switch (decoded.graph) {
        | None =>
          build_error_view(decoded.warnings, "Unable to render graph data")
        | Some(graph_data) =>
          if (has_plot_runtime()) {
            build_plot_view(
              ~model,
              ~dispatch=
                (width_blocks, height_blocks) =>
                  local(ResizeTo(width_blocks, height_blocks)),
              graph_data,
              decoded.warnings,
            );
          } else {
            build_error_view(decoded.warnings, runtime_missing_message);
          }
        };
      };

    View.mk(Node.div(~attrs=[Attr.classes(class_list)], [node]));
  };
};
