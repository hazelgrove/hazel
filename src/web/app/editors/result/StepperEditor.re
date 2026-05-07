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

    let step_segment = (~class_name, ~attrs=[], id: Id.t): option(Node.t) => {
      switch (TermData.segment(id, syntax.term_data)) {
      | None => None
      | Some(segment) =>
        Some(
          Node.div(
            ~attrs=[Attr.class_(class_name)] @ attrs,
            Highlight.of_segment(
              ~measured=syntax.measured,
              ~shape_map=syntax.shape_map,
              ~font_metrics,
              ~shape_init=Some(Convex),
              ~clss=[],
              segment,
            ),
          ),
        )
      };
    };

    let next_steps =
        (next_steps: list(Id.t), ~inject: int => Ui_effect.t(unit)) => {
      let step_tile = id =>
        switch (TermData.segment(id, syntax.term_data)) {
        | Some(segment) =>
          switch (
            segment
            |> List.find_opt(
                 fun
                 | Piece.Tile(t) => Tile.id(t) == id
                 | _ => false,
               )
          ) {
          | Some(Piece.Tile(t)) => Some(t)
          | _ => TermData.root_tile(id, syntax.term_data)
          }
        | None => TermData.root_tile(id, syntax.term_data)
        };
      next_steps
      |> List.mapi((i, id) =>
           switch (step_tile(id)) {
           | Some(t) =>
             Some(
               div_c(
                 "step-next",
                 Arms.term(
                   ~attr=[Attr.on_mousedown(_ => inject(i))],
                   ~font_metrics,
                   ~syntax,
                   t,
                 ),
               ),
             )
           | None => None
           }
         )
      |> List.filter_map(Fun.id);
    };

    let taken_steps = (taken_steps: list(Id.t)) =>
      taken_steps |> List.filter_map(step_segment(~class_name="step-taken"));

    let refl_steps =
        (refl_steps: list(Id.t), ~inject: int => Ui_effect.t(unit)) =>
      refl_steps
      |> List.mapi((i, id) =>
           step_segment(
             ~class_name="step-refl",
             ~attrs=[Attr.on_mousedown(_ => inject(i))],
             id,
           )
         )
      |> List.filter_map(Fun.id);

    taken_steps(model.taken_steps)
    @ next_steps(model.next_steps, ~inject=x =>
        {
          open OptUtil.Syntax;
          let step_id = List.nth(model.next_steps, x);
          let+ range =
            TermData.extreme_measures(
              step_id,
              model.editor.editor.syntax.term_data,
              model.editor.editor.syntax.measured,
            );
          Some(step_id) == selected_id
            ? signal(TakeStep(x)) : inject(Select(PointToPoint(range)));
        }
        |> Option.value(~default=Ui_effect.Ignore)
      )
    @ refl_steps(model.refls, ~inject=x =>
        {
          open OptUtil.Syntax;
          let refl_id = List.nth(model.refls, x);
          let+ range =
            TermData.extreme_measures(
              refl_id,
              model.editor.editor.syntax.term_data,
              model.editor.editor.syntax.measured,
            );
          Some(refl_id) == selected_id
            ? signal(Refl(x)) : inject(Select(PointToPoint(range)));
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
