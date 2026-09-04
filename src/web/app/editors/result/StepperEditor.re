open Util_web;
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
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = CodeSelectable.Update.t;
};

module Selection = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = CodeSelectable.Selection.t;

  let get_cursor_info = CodeSelectable.Selection.get_cursor_info;
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
    open Util_web.WebUtil;

    let next_steps =
        (next_steps: list(Id.t), ~inject: int => Ui_effect.t(unit)) =>
      next_steps
      |> List.filter_map(TermData.root_tile(_, syntax.term_data))
      |> List.mapi((i, t: Tile.t) =>
           div_c(
             "step-next",
             Arms.term(
               ~attr=[Attr.on_mousedown(_ => inject(i))],
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
        {
          open OptUtil.Syntax;
          let+ range =
            TermData.extreme_measures(
              List.nth(model.next_steps, x),
              model.editor.editor.syntax.term_data,
              model.editor.editor.syntax.measured,
            );
          Some(List.nth(model.next_steps, x)) == selected_id
            ? signal(TakeStep(x)) : inject(Select(PointToPoint(range)));
        }
        |> Option.value(~default=Ui_effect.Ignore)
      )
    @ refl_steps(model.refls, ~inject=x =>
        {
          open OptUtil.Syntax;
          let+ range =
            TermData.extreme_measures(
              List.nth(model.refls, x),
              model.editor.editor.syntax.term_data,
              model.editor.editor.syntax.measured,
            );
          Some(List.nth(model.refls, x)) == selected_id
            ? signal(Refl(x))
            : {
              inject(Select(PointToPoint(range)));
            };
        }
        |> Option.value(~default=Ui_effect.Ignore)
      );
  };

  /* Steppers don't support probe dynamics - expressions shown are
     intermediate evaluation steps, not the main program being probed. */
  let view =
      (
        ~globals: Globals.t,
        ~inject,
        ~signal: event => 'a,
        ~overlays=[],
        ~selected,
        ~selected_id,
        ~_dynamics: Language.Dynamics.Map.t=Language.Dynamics.Map.empty,
        model: Model.t,
      ) => {
    CodeSelectable.View.view(
      ~expand_selection=true,
      ~dynamics=Language.Dynamics.Map.empty,
      ~signal=
        fun
        | MakeActive => signal(MakeActive),
      ~edit_mode=
        EditMode.Editable({
          inject,
          escape: _ => Ui_effect.Ignore,
          take_focus: _ => Ui_effect.Ignore,
          focus: selected ? Some() : None,
        }),
      ~globals,
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
};
