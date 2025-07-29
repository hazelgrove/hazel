open Util;
open Calc.Syntax;

// OutputEditorManager is intended to be used for "outputs", e.g. evaluation
// results, stepper results, etc.
//
// The main difference from EditorManager, is that in the calcualte method
// it is kept in sync with a term, and that term is used for statics etc,
// rather than the editor's statics.
//
// Note: this currently only supports Exps

/* This file follows conventions in [docs/ui-architecture.md] */

module M = (Editor: EditorInterface.EDITOR) => {
  module Model = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      // Derived:
      editor: Calc.saved(Editor.Model.t), // Has some state but all state is reset when term changes.
      statics: Calc.saved(CachedStatics.t),
    };

    let mk_uncalculated = {
      editor: Calc.Pending,
      statics: Calc.Pending,
    };
  };

  module Update = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Editor.Update.t;

    // All updates are etherial and will be reset when the term changes.
    let update =
        (
          ~common: Common.global,
          ~dynamics: Language.Dynamics.Map.t,
          action: t,
          model: Model.t,
        )
        : Updated.t(Model.t) => {
      let editor =
        Calc.map_saved(
          Editor.Update.update(
            ~common=
              Common.t_of_global(
                ~statics=Calc.get_saved(CachedStatics.empty, model.statics),
                ~dynamics,
                common,
              ),
            action,
          ),
          model.editor,
        );
      Model.{
        editor,
        statics: model.statics,
      }
      |> Updated.return(
           ~is_edit=Editor.Update.is_edit(action),
           ~recalculate=false,
           ~scroll_active=Editor.Update.should_scroll_active(action),
         );
    };
  };

  let can_undo = Editor.Update.can_undo;

  let calcualte =
      (
        ~common: Common.global,
        ~settings: Calc.t(Language.CoreSettings.t),
        ~dynamics: Language.Dynamics.Map.t,
        ~ctx=?,
        ~term: Calc.t(Language.Any.t),
        ~inline=?,
        {editor, statics}: Model.t,
      ) => {
    let editor =
      editor
      |> {
        let.calc term = term;
        Editor.Model.mk_uncalculated(~inline?, term);
      };

    let statics =
      statics
      |> {
        let.calc term = term
        and.calc settings = settings
        and.calc ctx = Calc.of_option(ctx);
        CachedStatics.init_from_term(
          ~ctx?,
          ~settings,
          ~is_dynamic_term=true,
          term |> Language.Any.is_exp |> Option.get,
        );
      };

    let editor =
      editor
      |> Calc.map_t(
           Editor.Update.calculate(
             ~common=
               Common.t_of_global(
                 ~statics=statics |> Calc.get_value,
                 ~dynamics,
                 common,
               ),
           ),
         );

    Model.{
      editor: editor |> Calc.save,
      statics: statics |> Calc.save,
    };
  };

  module Focus = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Editor.Focus.t;

    let get_cursor_info =
        (
          ~common: Common.global,
          ~dynamics: Language.Dynamics.Map.t,
          ~inject: Update.t => Ui_effect.t(unit),
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
        ~read_only=true,
        model.editor
        |> Calc.get_saved_exc(
             ~print="OutputEditorManager.calculate not called",
           ),
        focus,
      );
  };

  module View = {
    let get_dimensions = (model: Model.t) =>
      Editor.View.get_dimensions(
        model.editor
        |> Calc.get_saved_exc(
             ~print="OutputEditorManager.calculate not called",
           ),
      );

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
        model.editor
        |> Calc.get_saved_exc(
             ~print="OutputEditorManager.calculate not called",
           ),
      );
    };
  };
};
