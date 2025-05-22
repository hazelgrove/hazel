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

/* Global actions available to handlers in all projectors */
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
  statics: option(Statics.Info.t),
  /* Dynamic information about the syntax including
   * live values of the syntax. Dynamics may be
   * disabled by the user; this case (None) must be
   * handled by projector authors */
  dynamics: option(Dynamics.Info.t),
};

module Focusable = {
  /* Can the projector take focus, in the sense of handling
   * keyboard input? If so, how can it take focus? */

  /* Callbacks for projectors to react to getting focus */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus_keyboard = (Id.t, Direction.t) => unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus_pointer = Id.t => unit;

  /* If keyboard is not None, the projector can get focus
   * from keyboard arrow movement into it. If pointer is
   * not None, it can get focus from pointer interaction */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    pointer: option(focus_pointer),
    keyboard: option(focus_keyboard),
  };

  /* Default: A projector that cannot take focus */
  let non: t = {
    pointer: None,
    keyboard: None,
  };
};

/* The different kinds of projector. New projector
 * types need to be registered here in order to be
 * able to create and update their instances */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type projector_kind =
  // | Fold
  | Info
  | Pair
  // | Probe
  // | Checkbox
  | Slider
  // | SliderF
  // | Card
  | Livelit;
// | TextArea;

type status = {
  kind: projector_kind,
  sort: Sort.t,
  indication: option(Direction.t),
  selected: bool,
  error: bool,
};

type projector_data('p) = {
  p: Piece.projector('p),
  info,
  measurement: Measured.measurement,
  offside_base: int,
  status,
};

module type PROJECTOR = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type editor_model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type editor_action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type editor_focus;

  module Model: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = model;

    let mk:
      (projector_kind, Any.t, unit => option(editor_model)) => option(t);

    let get_kind: t => projector_kind;
    let get_shape:
      (Statics.Map.t, Dynamics.Map.t, Base.projector(t)) => ProjectorShape.t;
    let get_focusable: t => Focusable.t;
    let focusable_of_kind: projector_kind => Focusable.t;

    let make_term: (t, Sort.t) => Any.t;
  };

  module Update: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = action;

    let update:
      (~common: common, ~sort: Sort.t, ~id: Id.t, t, Model.t) => Model.t;

    let calculate: (~common: common, ~sort: Sort.t, Model.t) => Model.t;
  };

  module Focus: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = focus;

    let handle_key_event:
      (~focus: t, ~key: Key.t, Model.t) => option(Update.t);
  };

  module View: {
    let split_views:
      (
        ~common: common,
        ~sort: Sort.t,
        ~parent: external_action => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~focus: Focus.t => Ui_effect.t(unit),
        ~focussed: option(Focus.t),
        projector_data(Model.t)
      ) =>
      (Web.Node.t, option(Web.Node.t));

    let mk_status:
      (
        Base.projector(Model.t),
        ~editor_active: bool,
        ~indicated: option((Id.t, Direction.t)),
        ~selection_ids: list(Id.t),
        ~info: info,
        ~id: Id.t
      ) =>
      status;
  };
};

module type EDITOR = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type projector_model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type projector_action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type projector_focus;

  module Model: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = model; // Transparent definition needed for handing editor to projectorinit

    let mk: (~settings: CoreSettings.t, ~inline: bool=?, Any.t) => t;

    let get_z: t => Zipper.t(projector_model);
    let make_term: (Sort.t, t) => Any.t;
    let get_trailing_hole_ctx: (t, Statics.Map.t) => option(Ctx.t);
    // [@deriving (show({with_path: false}), sexp, yojson)]
    // type persistent;
    // let persist: t => persistent;
    // let unpersist: persistent => t;
    let of_zipper: (~sort: Sort.t, Zipper.t(projector_model)) => t; // TODO: Replace with persistence logic
  };

  module Update: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = action;

    let update: (~common: common, ~sort: Sort.t, t, Model.t) => Model.t;

    let calculate:
      (~common: common, ~is_edited: bool, ~sort: Sort.t, Model.t) => Model.t;

    let key_handoff:
      (Model.t, Key.t) =>
      option(
        Action.project(projector_kind, projector_model, projector_action),
      );
    let jump_to_tile_action: (Id.t, Model.t) => option(action);
  };

  module Focus: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = focus;

    // TODO[Matt]: Used in jump to tile logic which will need updating.
    let here: t;

    let handle_key_event:
      (~focus: t, ~key: Key.t, Model.t) => option(Update.t);
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

    let view_editable:
      (
        ~common: common,
        ~inject: action => Ui_effect.t(unit),
        ~focus: Focus.t => Ui_effect.t(unit),
        ~focussed: option(Focus.t),
        ~overlays: list(Web.Node.t)=?,
        ~sort: Sort.t,
        Model.t
      ) =>
      Web.Node.t;
  };

  // TODO: refactor these helper functions away

  let get_measured: Model.t => Measured.t;
  let get_tiles: Model.t => TileMap.t(projector_model);
};
