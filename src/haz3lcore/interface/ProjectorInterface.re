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
  /* Static information about the syntax including type
   * information. Statics may be disabled by the user;
   * this case (None) must be handled by projector authors */
  statics: option(Language.Statics.Info.t),
  /* Dynamic information about the syntax including
   * live values of the syntax. Dynamics may be
   * disabled by the user; this case (None) must be
   * handled by projector authors */
  dynamics: option(Language.Dynamics.Info.t),
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
  type model;
  type action;
  type focus;

  module Model: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = model; // Transparent definition needed for handing editor to projectorinit

    let mk: (~inline: bool=?, Language.Any.t) => t;

    let get_trailing_hole_ctx:
      (t, Language.Statics.Map.t) => option(Language.Ctx.t);

    let get_cached_term: t => Language.Any.t;

    let copy: t => t;
  };

  module Update: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = action;

    let update: (~common: common, t, Model.t) => Model.t;

    let make_term:
      (~sort: Sort.t, Model.t) => (Model.t, Calc.t(Language.Any.t));

    let calculate: (~common: common, Model.t) => Model.t;

    let jump_to_tile_action: (Id.t, Model.t) => option(action);
  };

  module Focus: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t;

    // TODO[Matt]: Used in jump to tile logic which will need updating.
    // Thunked to make module "safe"
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
    let print_string: Model.t => string;

    let view:
      (
        ~font_metrics: FontMetrics.t,
        ~secondary_icons: bool,
        ~sort: Sort.t,
        Model.t
      ) =>
      WebUtil.Node.t;

    let get_dimensions: Model.t => Point.t;

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
  };
};

module type PROJECTOR = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus;

  let init:
    (~copy_ed: 'ed_m => 'ed_m, Language.Any.t, unit => option('ed_m)) =>
    option('model);
  let dynamics: bool;
  let update:
    (~common: common, ~sort: Sort.t, info, 'model, 'action) => 'model;
  let mk_term:
    (~sort: Sort.t, ~prev: Calc.saved(Language.Any.t), 'model) =>
    ('model, Calc.t(Language.Any.t));
  let calculate: (~common: common, 'model) => 'model;
  let get_cursor_info:
    (
      ~common: common,
      ~inject: 'action => Ui_effect.t(unit),
      ~read_only: bool,
      'model,
      'focus
    ) =>
    Cursor.t;
  let view:
    (
      ~common: common,
      ~ed_str: 'ed_m => string,
      ~local: 'action => Ui_effect.t(unit),
      ~parent: external_action => Ui_effect.t(unit),
      ~focus: 'focus => Ui_effect.t(unit),
      ~focussed: option('focus),
      'model,
      info
    ) =>
    View.t;
  let placeholder:
    (~ed_size: 'ed_m => Point.t, 'model, info) => ProjectorShape.t;
};
