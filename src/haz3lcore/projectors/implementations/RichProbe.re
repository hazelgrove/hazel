open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

/* Signature for domain-specific representations with clear parsing and rendering phases.
      Each RichProbe module handles a specific visualization of syntax elements.

      - 'value': The abstract data type representing the parsed internal representation of the probed value.
        This is parsed and it's presence signifies the ability to visualize the expression.

      - 'model': The UI state for the probe's interactive elements and controls.
        Stores user inputs, selected options, and transient state.

      - 'action': Events that can change the probe's model, like user interactions
        (button clicks, input changes) or system updates. This can also be used to update the syntax/
   */
module type RichProbe = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type value;

  let update: (model, action) => model;
  /* Parse an expression into its domain-specific value representation.
     This extracts the structured data needed for interactive visualization. */
  let parse: (Sort.t, Exp.t) => option(value);
  /* Initialize the probe's state from a parsed value. Assumes value is valid. */
  let init: value => model;

  /* Reports the projector shape (vertical line count) needed to fit
     the rendered modal. ProbeProj routes this into refractor_shape_map
     so code below the probe shifts down instead of being overlapped. */
  let placeholder: (value, model) => ProjectorCore.Shape.t;

  let badge: Node.t;

  let render:
    (
      ~info: info,
      ~exp: Exp.t,
      ~value: value,
      ~view_seg: (Sort.t, Segment.t) => Node.t,
      ~model: model,
      ~local: action => Ui_effect.t(unit),
      ~parent: external_action => Ui_effect.t(unit),
      ~sort: Sort.t,
      unit
    ) =>
    Node.t;
};

/* Packed renderer: heterogeneous container for any RichProbe.
 * Model/action/value are serialized to strings so ProbeProj can
 * store a mixed list without knowing the concrete types. */
type packed_renderer = {
  id: string,
  can_handle: (Sort.t, Exp.t) => bool,
  parse_packed: (Sort.t, Exp.t) => option(string),
  init_packed: string => string,
  render_packed:
    (
      string,
      ~info: info,
      ~exp: Exp.t,
      ~view_seg: (Sort.t, Segment.t) => Node.t,
      ~local: string => Ui_effect.t(unit),
      ~parent: external_action => Ui_effect.t(unit),
      ~sort: Sort.t,
      unit
    ) =>
    option(Virtual_dom.Vdom.Node.t),
  update_packed: (string, string) => string,
  placeholder_packed:
    (string /*model*/, string /*value*/) => ProjectorCore.Shape.t,
  badge: Virtual_dom.Vdom.Node.t,
};

/* Pack a RichProbe module into a packed_renderer by serializing its
 * model/action/value through sexp. */
let pack_renderer =
    (
      type m,
      type a,
      module_impl: (module RichProbe with type model = m and type action = a),
      id: string,
    )
    : packed_renderer => {
  module R = (val module_impl);

  let serialize_model = m => m |> R.sexp_of_model |> Sexplib.Sexp.to_string;
  let deserialize_model = s => s |> Sexplib.Sexp.of_string |> R.model_of_sexp;
  let serialize_action = a => a |> R.sexp_of_action |> Sexplib.Sexp.to_string;
  let deserialize_action = s =>
    s |> Sexplib.Sexp.of_string |> R.action_of_sexp;
  let serialize_value = v => v |> R.sexp_of_value |> Sexplib.Sexp.to_string;

  {
    id,
    can_handle: (sort, exp) => Option.is_some(R.parse(sort, exp)),
    parse_packed: (sort, exp) =>
      R.parse(sort, exp) |> Option.map(serialize_value),
    init_packed: value_str => {
      let v = value_str |> Sexplib.Sexp.of_string |> R.value_of_sexp;
      let m = R.init(v);
      serialize_model(m);
    },
    render_packed:
      (model_str, ~info, ~exp, ~view_seg, ~local, ~parent, ~sort, ()) => {
      let v = R.parse(sort, exp);
      let model = model_str |> Sexplib.Sexp.of_string |> R.model_of_sexp;
      switch (v) {
      | Some(value) =>
        Some(
          R.render(
            ~info,
            ~exp,
            ~value,
            ~view_seg,
            ~model,
            ~local=action => local(serialize_action(action)),
            ~parent,
            ~sort,
            (),
          ),
        )
      | None => None
      };
    },
    update_packed: (model_str, action_str) =>
      R.update(deserialize_model(model_str), deserialize_action(action_str))
      |> serialize_model,
    placeholder_packed: (model_str, value_str) => {
      let v = value_str |> Sexplib.Sexp.of_string |> R.value_of_sexp;
      let m = model_str |> Sexplib.Sexp.of_string |> R.model_of_sexp;
      R.placeholder(v, m);
    },
    badge: R.badge,
  };
};
