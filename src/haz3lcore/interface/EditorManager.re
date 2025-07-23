open Util;

// An "editor manager" contains an editor, along with an associated term and statics.
// The difference between this and a regular editor is that this creates its own term
// whereas regular editors require their parent to ask them for a term.

module M = (Editor: EditorInterface.EDITOR) => {
  module Model = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      editor: Editor.Model.t,
      statics: Calc.saved(CachedStatics.t),
    };

    let mk = (~inline: option(bool)=?, term: Language.Any.t): t => {
      {
        editor: Editor.Model.mk_uncalculated(~inline?, term),
        statics: Calc.Pending,
      };
    };

    let copy = (model: t): t => {
      {
        editor: Editor.Model.copy(model.editor),
        statics: model.statics,
      };
    };

    type persistent = PersistentZipper.t;

    // Note(Matt): these functions should eventually be factored away once serialization is handled properly.
    let get_editor = (model: t): Editor.Model.t => model.editor;
    let of_editor = (editor: Editor.Model.t): t => {
      {
        editor,
        statics: Calc.Pending,
      };
    };

    let get_statics = (model: t): CachedStatics.t =>
      model.statics |> Calc.get_saved(CachedStatics.empty);
  };

  module Update = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Editor.Update.t;

    let update =
        (
          ~common: Common.global,
          ~dynamics: Language.Dynamics.Map.t,
          action: t,
          model: Model.t,
        )
        : Updated.t(Model.t) => {
      let editor =
        Editor.Update.update(
          ~common=
            Common.t_of_global(
              ~statics=Calc.get_saved(CachedStatics.empty, model.statics),
              ~dynamics,
              common,
            ),
          action,
          model.editor,
        );
      Model.{
        editor,
        statics: model.statics,
      }
      |> Updated.return(
           ~is_edit=Editor.Update.is_edit(action),
           ~recalculate=true,
           ~scroll_active=Editor.Update.should_scroll_active(action),
         );
    };

    let can_undo = Editor.Update.can_undo;

    let calculate =
        (
          ~common: Common.global,
          ~stitch,
          ~dynamics,
          ~is_dynamic_term,
          ~ctx=?,
          model: Model.t,
        )
        : Model.t => {
      let (editor, term) =
        Editor.Update.make_term(~sort=Sort.Any, model.editor);
      let statics =
        if (common.settings.statics && common.settings.elaborate) {
          model.statics
          |> {
            open Calc.Syntax;
            let.calc term = term;
            CachedStatics.init_from_term(
              ~ctx?,
              ~settings=common.settings,
              ~is_dynamic_term,
              term |> Language.Any.is_exp |> Option.get |> stitch,
            );
          }
          |> Calc.save;
        } else {
          Calc.Pending;
        };
      let editor =
        Editor.Update.calculate(
          ~common=
            Common.t_of_global(
              ~statics=Calc.get_saved(CachedStatics.empty, statics),
              ~dynamics,
              common,
            ),
          editor,
        );
      {
        editor,
        statics,
      };
    };

    let jump_to_tile_action = (id: Id.t, model: Model.t): option(t) =>
      Editor.Update.jump_to_tile_action(id, model.editor);
  };

  module Focus = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Editor.Focus.t;

    let get_cursor_info =
        (
          ~common: Common.global,
          ~dynamics: Language.Dynamics.Map.t,
          ~inject: Update.t => Ui_effect.t(unit),
          ~read_only: bool,
          model: Model.t,
          focus: t,
        ) =>
      Editor.Focus.get_cursor_info(
        ~common=
          Common.t_of_global(
            ~statics=Calc.get_saved(CachedStatics.empty, model.statics),
            ~dynamics,
            common,
          ),
        ~inject,
        ~read_only,
        model.editor,
        focus,
      );
  };

  module View = {
    let get_dimensions = (model: Model.t) =>
      Editor.View.get_dimensions(model.editor);

    let view =
        (
          ~common: Common.global,
          ~dynamics: Language.Dynamics.Map.t,
          ~mode: EditorInterface.edit_mode('a, 'f),
          ~overlays: option(list(WebUtil.Node.t))=?,
          ~background: option(bool)=?,
          ~sort: Sort.t,
          model: Model.t,
        ) => {
      let statics = Calc.get_saved(CachedStatics.empty, model.statics);
      Editor.View.view(
        ~common=Common.t_of_global(~statics, ~dynamics, common),
        ~mode,
        ~overlays?,
        ~background?,
        ~sort,
        model.editor,
      );
    };
  };
};
