open Util;
open Virtual_dom.Vdom;

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
      ~view_ed: (~sort: Sort.t, 'ed_m) => Node.t,
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

// /* To add a new projector:
//  * 1. Create a new module implementing Projector (e.g. FoldProj)
//  * 2. Add an entry for it in ProjectorCore.Kind.t
//  * 3. Register the module in ProjectorInit.to_module
//  * 4. If you want to expose the projector via a keyboard
//  *    shortcut, add a Project(...) entry in Keyboard.re
//  * 5. If you want to expose the projector in the projector
//  *    panel bottom bar UI, update ProjectorCore.Kind.name,
//  *    ProjectorCore.Kind.of_name, and ProjectorCore.projectors
//  * 6. If you want to manually manage the projector as part of
//  *    the update cycle, use the implementation of the
//  *    SetIndicated action in ProjectorPerform as a guide
//  *    for how to add/remove projectors in an editor */
// module type Projector = {
//   /* The internal model type of the projector which will
//    * be serialized and persisted. Use `unit` if you don't
//    * need other state beyond the underlying syntax */
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type model('ed);
//   let kind: ProjectorCore.Kind.gadt(model('ed));
//   /* An internal action type to be used in actions which
//    * update the model. Use `unit` if the basic projector
//    * actions (type `action`) above suffice */
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type action;
//   /* Init should return None if the projector doesn't want
//    * to handle the provided term. Otherwise, it should
//    * return the desired initial state of the model. */
//   let init: Term.Any.t => option(model('ed));
//   /* Does this projector have some notion of internal
//    * positions, whose handling should override the editor
//    * caret & keyboard handlers? If so, provide handlers
//    * here (see Focusable for more information) */
//   let focusable: Focusable.t;
//   /* If dynamics is true, this projector will be
//    * instrumented with a probe to collect dynamic
//    * information during evaluation */
//   let dynamics: bool;
//   /* Renders the DOM views for the projector */
//   let view:
//     (
//       model('ed),
//       info('p),
//       /* A callback for the projector's own actions */
//       ~local: action => Ui_effect.t(unit),
//       /* A callback for parent editor actions */
//       ~parent: external_action('p) => Ui_effect.t(unit),
//       /* Creates a non-interactive embedded syntax view,
//        * provided here to address a dependency cycle */
//       ~view_seg: View.seg('p)
//     ) =>
//     View.t;
//   /* The space left for the projector in the base editor */
//   let placeholder: (model('ed), info('p)) => ProjectorShape.t;
//   /* Update the local projector model given an action */
//   let update: (model('ed), info('p), action) => model('ed);
//   let mk_term: (model('ed), Any.t) => Any.t;
