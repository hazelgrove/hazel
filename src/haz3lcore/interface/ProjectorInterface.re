open Util;
open WebUtil;

type common = {
  settings: Language.CoreSettings.t,
  font_metrics: FontMetrics.t,
  secondary_icons: bool,
  show_backpack_targets: bool,
  color_highlights: option(ColorSteps.colorMap),
  statics: CachedStatics.t,
  dynamics: Language.Dynamics.Map.t,
};

type edit_mode('p_k, 'p_m, 'p_a, 'e_f) =
  | ReadOnly
  | Editable({
      inject: Action.t('p_k, 'p_m, 'p_a) => Ui_effect.t(unit),
      make_active: 'e_f => Ui_effect.t(unit),
      has_focus: option('e_f),
    });

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
};

let mk_info = (~id: Id.t, ~sort: Sort.t): info => {
  id,
  sort,
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

module type EDITOR = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus;

  module Model: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = model;

    /* Makes a new editor for the given term. This function
     * does not run `make_term` or `calculate`, so the
     * editor will not have any cached term or statics
     * and cannot be rendered until those functions are called. */
    let mk: (~inline: bool=?, Language.Any.t) => t;

    /* Must be called after `make_term`. Throws an
     * exception if no cached term is available. */
    let get_cached_term: t => Language.Any.t;

    /* Copies an editor but gives it a new id */
    let copy: t => t;
  };

  module Update: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = action;

    let update: (~common: common, t, Model.t) => Model.t;

    /* Makes a term from the editor, returning the updated
     * editor (with the term cached) and the term */
    let make_term:
      (~sort: Sort.t, Model.t) => (Model.t, Calc.t(Language.Any.t));

    /* Must be called after `make_term`. */
    let calculate: (~common: common, Model.t) => Model.t;

    let jump_to_tile_action: (Id.t, Model.t) => option(action);
  };

  module Focus: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = focus;

    /* This value is thunked to make the module "safe" for the Ocaml
     * compiler. */
    let here: unit => t;

    let get_cursor_info:
      (
        ~common: common,
        ~inject: Update.t => Ui_effect.t(unit),
        ~read_only: bool,
        Model.t,
        t
      ) =>
      Cursor.t;

    /* Focus this editor at the leftmost/rightmost position.
     * This function requires an `inject` function so that it
     * can move the cursor in the editor, and a `focus`
     * function so that it can update the focus to point to itself */
    let enter:
      (
        ~inject: Update.t => Ui_effect.t(unit),
        ~focus: t => Ui_effect.t(unit),
        Direction.t,
        Model.t
      ) =>
      Ui_effect.t(unit);
  };

  module View: {
    /* Returns the editor contents as a string */
    let print_string: Model.t => string;

    /* Must be called after `make_term` and `calculate`.
     * Returns the dimensions of the editor contents. */
    let get_dimensions: Model.t => Point.t;

    /* Must be called after `make_term` and `calculate`.*/
    let view:
      (
        ~font_metrics: FontMetrics.t,
        ~secondary_icons: bool,
        ~sort: Sort.t,
        ~background: bool=?,
        Model.t
      ) =>
      WebUtil.Node.t;

    /* Must be called after `make_term` and `calculate`.*/
    let view_editable:
      (
        ~common: common,
        ~inject: action => Ui_effect.t(unit),
        ~focus: Focus.t => Ui_effect.t(unit),
        ~focussed: option(Focus.t),
        ~escape: Direction.t => Ui_effect.t(unit),
        ~overlays: list(WebUtil.Node.t)=?,
        ~sort: Sort.t,
        Model.t
      ) =>
      WebUtil.Node.t;

    let term: (~common: common, Language.Any.t) => (WebUtil.Node.t, Point.t);
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
    (~common: common, ~sort: Sort.t, ~id: Id.t, model', action') => model';

  let mk_term:
    (~sort: Sort.t, ~prev: Calc.saved(Language.Any.t), model') =>
    (model', Calc.t(Language.Any.t));

  let calculate: (~common: common, model') => model';

  let get_cursor_info:
    (
      ~common: common,
      ~inject: action' => Ui_effect.t(unit),
      ~read_only: bool,
      model',
      focus'
    ) =>
    Cursor.t;

  /* The space left for the projector in the base editor */
  let placeholder: (~common: common, ~id: Id.t, model') => ProjectorShape.t;

  let view:
    (
      ~common: common,
      ~inject: action' => Ui_effect.t(unit),
      ~escape: external_action => Ui_effect.t(unit),
      ~take_focus: focus' => Ui_effect.t(unit),
      ~focus: option(focus'),
      ~id: Id.t,
      model'
    ) =>
    View.t;
};

module Defaults = {
  let calculate = (~calculate_ed as _, ~common as _, m) => m;

  let get_cursor_info =
      (~common as _, ~inject as _, ~read_only as _, _model, _focus) => Cursor.empty;
};
