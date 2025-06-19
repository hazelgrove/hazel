open Util;
open Virtual_dom.Vdom;
open Language;

/* This descibes the API for projectors: GUIs which
 * can replace part of the program syntax and perform
 * actions on that underlying syntax, as well as
 * mainting their own custom state. The comments below
 * detail the procedure of defining a new projector.
 *
 * See zipper/projectors/ for examples
 * of currently available projectors */

/* The type of syntax which a projector can replace.
 * Right now projectors can replace a single piece */
[@deriving (show({with_path: false}), sexp, yojson)]
type syntax('p) = Base.piece('p);

[@deriving (show({with_path: false}), sexp, yojson)]
type external_action = ProjectorInterface.external_action;

module Calculate = {
  let default = (~calculate_ed as _, ~common as _, m) => m;
};

module CursorInfo = {
  let default =
      (
        ~get_cursor_info_ed as _,
        ~common as _,
        ~inject as _,
        ~read_only as _,
        _model,
        _focus,
      ) => Cursor.empty;
};

module View = ProjectorInterface.View;

/* External info proivded to all projectors */
[@deriving (show({with_path: false}), sexp, yojson)]
type info = ProjectorInterface.info;

type methods('model, 'action, 'focus, 'ed_m, 'ed_a, 'ed_f) = {
  init:
    (~copy_ed: 'ed_m => 'ed_m, Term.Any.t, unit => option('ed_m)) =>
    option('model),
  dynamics: bool,
  update:
    (
      ~update_ed: (~common: ProjectorInterface.common, 'ed_a, 'ed_m) => 'ed_m,
      ~common: ProjectorInterface.common,
      ~sort: Sort.t,
      info,
      'model,
      'action
    ) =>
    'model,
  mk_term:
    (
      ~mk_term_ed: (~sort: Sort.t, 'ed_m) => ('ed_m, Calc.t(Any.t)),
      ~sort: Sort.t,
      ~prev: Calc.saved(Any.t),
      'model
    ) =>
    ('model, Calc.t(Any.t)),
  calculate:
    (
      ~calculate_ed: (~common: ProjectorInterface.common, 'ed_m) => 'ed_m,
      ~common: ProjectorInterface.common,
      'model
    ) =>
    'model,
  get_cursor_info:
    (
      ~get_cursor_info_ed:
        (
          ~common: ProjectorInterface.common,
          ~inject: 'ed_a => Ui_effect.t(unit),
          ~read_only: bool,
          'ed_m,
          'ed_f
        ) =>
        Cursor.t,
      ~common: ProjectorInterface.common,
      ~inject: 'action => Ui_effect.t(unit),
      ~read_only: bool,
      'model,
      'focus
    ) =>
    Cursor.t,
  view:
    (
      ~common: ProjectorInterface.common,
      ~ed_str: 'ed_m => string,
      //TODO(andrew): integrate sort into ed
      ~view_ed: (~sort: Sort.t, ~background: bool=?, 'ed_m) => Node.t,
      ~view_editable:
        (
          ~common: ProjectorInterface.common,
          ~inject: 'ed_a => Ui_effect.t(unit),
          ~focus: 'ed_f => Ui_effect.t(unit),
          ~focussed: option('ed_f),
          ~escape: Direction.t => Ui_effect.t(unit),
          ~overlays: list(Node.t)=?,
          ~sort: Sort.t,
          'ed_m
        ) =>
        Node.t,
      ~enter_ed:
        (
          ~inject: 'ed_a => Ui_effect.t(unit),
          ~focus: 'ed_f => Ui_effect.t(unit),
          Direction.t,
          'ed_m
        ) =>
        Ui_effect.t(unit),
      ~mk_ed: Any.t => 'ed_m,
      ~mk_term_ed: (~sort: Sort.t, 'ed_m) => ('ed_m, Calc.t(Any.t)),
      ~calculate_ed: (~common: ProjectorInterface.common, 'ed_m) => 'ed_m,
      ~local: 'action => Ui_effect.t(unit),
      ~parent: external_action => Ui_effect.t(unit),
      ~focus: 'focus => Ui_effect.t(unit),
      ~focussed: option('focus),
      'model,
      info
    ) =>
    View.t,
  placeholder: (~ed_size: 'ed_m => Point.t, 'model, info) => ProjectorShape.t,
  sexp_of_model: ('ed_m => Sexplib.Sexp.t, 'model) => Sexplib.Sexp.t,
  model_of_sexp: (Sexplib.Sexp.t => 'ed_m, Sexplib.Sexp.t) => 'model,
  yojson_of_model: ('ed_m => Yojson.Safe.t, 'model) => Yojson.Safe.t,
  model_of_yojson: (Yojson.Safe.t => 'ed_m, Yojson.Safe.t) => 'model,
  sexp_of_action: ('ed_a => Sexplib.Sexp.t, 'action) => Sexplib.Sexp.t,
  action_of_sexp: (Sexplib.Sexp.t => 'ed_a, Sexplib.Sexp.t) => 'action,
  yojson_of_action: ('ed_a => Yojson.Safe.t, 'action) => Yojson.Safe.t,
  action_of_yojson: (Yojson.Safe.t => 'ed_a, Yojson.Safe.t) => 'action,
  sexp_of_focus: ('ed_f => Sexplib.Sexp.t, 'focus) => Sexplib.Sexp.t,
  focus_of_sexp: (Sexplib.Sexp.t => 'ed_f, Sexplib.Sexp.t) => 'focus,
  yojson_of_focus: ('ed_f => Yojson.Safe.t, 'focus) => Yojson.Safe.t,
  focus_of_yojson: (Yojson.Safe.t => 'ed_f, Yojson.Safe.t) => 'focus,
};
