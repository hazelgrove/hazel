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
  let parse: Exp.t => option(value);
  /* Initialize the probe's state from a parsed value. Assumes value is valid. */
  let init: value => model;

  let badge: Node.t;

  let render:
    (
      ~info: info,
      ~exp: Exp.t,
      ~view_seg: (Sort.t, Segment.t) => Node.t,
      ~model: model,
      ~local: action => Ui_effect.t(unit),
      ~parent: external_action => Ui_effect.t(unit),
      unit
    ) =>
    Node.t;
};
