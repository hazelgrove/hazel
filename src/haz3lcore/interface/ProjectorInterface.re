open Util;

type common = {
  settings: CoreSettings.t,
  font_metrics: FontMetrics.t,
  secondary_icons: bool,
  show_backpack_targets: bool,
  color_highlights: option(ColorSteps.colorMap),
  statics: CachedStatics.t,
  dynamics: Dynamics.Map.t,
};

type edit_mode('p_k, 'p_m, 'p_a, 'e_f) =
  | ReadOnly
  | Editable({
      inject: Action.t('p_k, 'p_m, 'p_a) => Ui_effect.t(unit),
      make_active: 'e_f => Ui_effect.t(unit),
      has_focus: option('e_f),
    });

module type EDITOR = {
  type model;
  type action;
  type focus;

  module Model: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = model; // Transparent definition needed for handing editor to projectorinit

    let mk: (~settings: CoreSettings.t, ~inline: bool=?, Any.t) => t;

    let get_trailing_hole_ctx: (t, Statics.Map.t) => option(Ctx.t);

    let get_cached_term: t => Term.Any.t;

    let copy: t => t;
  };

  module Update: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = action;

    let update: (~common: common, t, Model.t) => Model.t;

    let make_term: (~sort: Sort.t, Model.t) => (Model.t, Calc.t(Any.t));

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
      Web.Node.t;

    let get_dimensions: Model.t => Point.t;

    let view_editable:
      (
        ~common: common,
        ~inject: action => Ui_effect.t(unit),
        ~focus: Focus.t => Ui_effect.t(unit),
        ~focussed: option(Focus.t),
        ~escape: Direction.t => Ui_effect.t(unit),
        ~overlays: list(Web.Node.t)=?,
        ~sort: Sort.t,
        Model.t
      ) =>
      Web.Node.t;
  };
};
