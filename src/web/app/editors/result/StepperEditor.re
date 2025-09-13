open Util;
open Haz3lcore;

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // Updated
    editor: CodeSelectable.Model.t,
    // Read-only
    taken_steps: list(Id.t),
    next_steps: list(Id.t),
    refls: list(Id.t),
  };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = CodeSelectable.Update.t;

  let update = (~settings, action, model: Model.t): Updated.t(Model.t) => {
    let* editor =
      CodeSelectable.Update.update(~settings, action, model.editor);
    Model.{
      editor,
      taken_steps: model.taken_steps,
      next_steps: model.next_steps,
      refls: model.refls,
    };
  };

  let can_undo = CodeSelectable.Update.can_undo;

  let calculate =
      (
        ~settings,
        ~is_edited,
        ~stitch,
        ~dynamics: Language.Dynamics.Map.t,
        ~ana,
        {editor, taken_steps, next_steps, refls}: Model.t,
      )
      : Model.t => {
    let editor =
      CodeSelectable.Update.calculate(
        ~settings,
        ~is_edited,
        ~stitch,
        ~dynamics,
        ~is_dynamic_term=true,
        ~ana,
        editor,
      );
    {
      editor,
      taken_steps,
      next_steps,
      refls,
    };
  };
};

module Selection = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = CodeSelectable.Selection.t;

  let get_cursor_info = CodeSelectable.Selection.get_cursor_info;

  let handle_key_event = CodeSelectable.Selection.handle_key_event;
};

module View = {
  type event =
    | MakeActive
    | TakeStep(int)
    | Refl(int);

  let deco =
      (
        ~syntax: CachedSyntax.t,
        ~font_metrics: FontMetrics.t,
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected_id: option(Id.t),
        signal: event => Ui_effect.t(unit),
        model: Model.t,
      ) => {
    open WebUtil;

    let next_steps =
        (next_steps: list(Id.t), ~inject: int => Ui_effect.t(unit)) =>
      next_steps
      |> List.filter_map(TermData.root_tile(_, syntax.term_data))
      |> List.mapi((i, t: Tile.t) =>
           div_c(
             "step-next",
             Arms.term(
               ~attr=[Attr.on_mouseup(_ => inject(i))],
               ~font_metrics,
               ~syntax,
               t,
             ),
           )
         );

    let taken_steps = (taken_steps: list(Id.t)) =>
      taken_steps
      |> List.filter_map(TermData.root_tile(_, syntax.term_data))
      |> List.map(t =>
           div_c("step-taken", Arms.term(~font_metrics, ~syntax, t))
         );

    let refl_steps =
        (refl_steps: list(Id.t), ~inject: int => Ui_effect.t(unit)) =>
      refl_steps
      |> List.filter_map(TermData.root_tile(_, syntax.term_data))
      |> List.mapi((i, t: Tile.t) =>
           div_c(
             "step-refl",
             Arms.term(
               ~attr=[Attr.on_mousedown(_ => inject(i))],
               ~font_metrics,
               ~syntax,
               t,
             ),
           )
         );

    taken_steps(model.taken_steps)
    @ next_steps(model.next_steps, ~inject=x =>
        Some(List.nth(model.next_steps, x)) == selected_id
          ? signal(TakeStep(x))
          : inject(Select(Term(Id(List.nth(model.next_steps, x), Right))))
      )
    @ refl_steps(model.refls, ~inject=x => signal(Refl(x)));
  };

  let view =
      (
        ~globals: Globals.t,
        ~inject,
        ~signal: event => 'a,
        ~overlays=[],
        ~selected,
        ~selected_id,
        model: Model.t,
      ) =>
    CodeSelectable.View.view(
      ~signal=
        fun
        | MakeActive => signal(MakeActive),
      ~selected,
      ~globals,
      ~inject,
      ~overlays=
        overlays
        @ deco(
            ~syntax=model.editor.editor.syntax,
            ~font_metrics=globals.font_metrics,
            ~inject,
            ~selected_id,
            signal,
            model,
          ),
      model.editor,
    );
};
