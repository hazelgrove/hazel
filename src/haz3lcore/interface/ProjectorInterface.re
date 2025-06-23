open Util;
open WebUtil;

/* Global actions available to handlers in all projectors */
[@deriving (show({with_path: false}), sexp, yojson)]
type external_action =
  | Remove /* Remove projector entirely */
  | Escape(Util.Direction.t); /* Pass focus to parent editor */

/* External info proivded to all projectors */
[@deriving (show({with_path: false}), sexp, yojson)]
type info = {
  /* The id of the projector, equal to the id of the root
   * term of the syntax, provided directly here for convenience.
   * This is mostly intended to be used as a persistent unique
   * identifier to allow individual projectors to distiguish
   * their DOM nodes. */
  id: Id.t,
  sort: Sort.t,
  term: Language.Term.Any.t,
};

let mk_info = (~id: Id.t, ~sort: Sort.t, ~term: Language.Term.Any.t): info => {
  id,
  sort,
  term,
};

module View = {
  /* A projector has an inline view, which replaces the underlying
   * syntax. Optionally, it may have an overlay view, which is shown
   * in the same place, but above most base editor decorations
   * including the inline views of all other projectors, and/or
   * an offside view, which is rendered at the end of the base
   * editor line containing the projector */
  type t = {
    inline: Node.t,
    overlay: option(Node.t),
    offside: option(Node.t),
    enter_left: option(Ui_effect.t(unit)),
    enter_right: option(Ui_effect.t(unit)),
  };

  let mk =
      (~overlay=None, ~offside=None, ~enter_left=?, ~enter_right=?, inline) => {
    inline,
    overlay,
    offside,
    enter_left,
    enter_right,
  };
};

module type PROJECTOR = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model';
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action';
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus';

  /* Used for type checker bookkeeping - should always be Editor.model */
  type editor_model;

  /* Mk should return None if the projector doesn't want
   * to handle the provided term. Otherwise, it should
   * return the desired initial state of the model
   * before `mk_term` and `calculate` are called. */
  let mk: (Language.Any.t, unit => option(editor_model)) => option(model');

  /* If dynamics is true, this projector will be
   * instrumented with a probe to collect dynamic
   * information during evaluation */
  let dynamics: bool;

  let update:
    (~common: Common.t, ~sort: Sort.t, ~id: Id.t, model', action') => model';

  let mk_term:
    (~sort: Sort.t, ~prev: Calc.saved(Language.Any.t), model') =>
    (model', Calc.t(Language.Any.t));

  let calculate: (~common: Common.t, model') => model';

  let get_cursor_info:
    (
      ~common: Common.t,
      ~inject: action' => Ui_effect.t(unit),
      ~read_only: bool,
      model',
      focus'
    ) =>
    Cursor.t;

  /* The space left for the projector in the base editor */
  let placeholder: (~common: Common.t, ~id: Id.t, model') => ProjectorShape.t;

  let view:
    (
      ~common: Common.t,
      ~inject: action' => Ui_effect.t(unit),
      ~escape: external_action => Ui_effect.t(unit),
      ~take_focus: focus' => Ui_effect.t(unit),
      ~focus: option(focus'),
      ~info: info,
      model'
    ) =>
    View.t;

  let unproject: model' => editor_model;
};

module Defaults = {
  let calculate = (~calculate_ed as _, ~common as _, m) => m;

  let get_cursor_info =
      (~common as _, ~inject as _, ~read_only as _, _model, _focus) => Cursor.empty;
};
