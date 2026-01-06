open Util;

type edit_mode('ed_a, 'ed_f) =
  | ReadOnly
  | Editable({
      inject: 'ed_a => Ui_effect.t(unit),
      escape: Util.Direction.t => Ui_effect.t(unit),
      take_focus: 'ed_f => Ui_effect.t(unit),
      focus: option('ed_f),
    });

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
    let mk_uncalculated: (~inline: bool=?, Language.Any.t) => t;

    /* Makes a new editor for the given term. This function
     * will run `make_term` and `calculate` so the editor
     * will have a cached term and statics and can be rendered
     * immediately. */
    let init: (~common: Common.t, ~inline: bool=?, Language.Any.t) => t;

    // /* Makes a new editor for the given term.
    //  * This function will run `make_term` and `calculate`
    //  * so the editor will have a cached term and statics
    //  * and can be rendered immediately. */
    // let mk: (~inline: bool=?, Language.Any.t) => t;

    /* Must be called after `make_term`. Throws an
     * exception if no cached term is available. */
    let get_cached_term: t => Language.Any.t;

    /* split was removed - depended on TermRanges which was replaced by TermData.
     * If needed, reimplement using TermData. See comment in Editor.re. */

    /* Copies an editor but gives it a new id */
    let copy: t => t;
  };

  module Update: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = action;

    let update: (~common: Common.t, t, Model.t) => Model.t;

    /* Makes a term from the editor, returning the updated
     * editor (with the term cached) and the term */
    let make_term:
      (~sort: Sort.t, Model.t) => (Model.t, Calc.t(Language.Any.t));

    /* Must be called after `make_term`. */
    let calculate: (~common: Common.t, Model.t) => Model.t;

    let jump_to_tile_action: (Id.t, Model.t) => option(action);

    let can_undo: t => bool;
    let is_edit: t => bool;
    let should_scroll_active: t => bool;
  };

  module Focus: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = focus;

    /* This value is thunked to make the module "safe" for the Ocaml
     * compiler. */
    let here: unit => t;

    let get_cursor_info:
      (
        ~common: Common.t,
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
        ~common: Common.t,
        ~mode: edit_mode(Update.t, Focus.t),
        ~overlays: list(WebUtil.Node.t)=?,
        ~background: bool=?,
        ~sort: Sort.t,
        Model.t
      ) =>
      WebUtil.Node.t;

    let term:
      (~common: Common.t, Language.Any.t) => (WebUtil.Node.t, Point.t);
  };
};
