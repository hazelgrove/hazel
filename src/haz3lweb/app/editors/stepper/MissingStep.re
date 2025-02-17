open Haz3lcore;
open Util;
open Calc.Syntax;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type rewrites = {rewrites: list(Exp.t)};

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    next_steps:
      Calc.saved(list((FilterAction.action, EvaluatorStep.EvalObj.t))),
    selected_id: Calc.saved(option(Id.t)),
    selected_exp: Calc.saved(option(Exp.t)),
    rewrites: Calc.saved(option(rewrites)),
  };

  let init = {
    next_steps: Calc.Pending,
    selected_id: Calc.Pending,
    selected_exp: Calc.Pending,
    rewrites: Calc.Pending,
  };

  let get_next_steps =
      (m: t): list((FilterAction.action, EvaluatorStep.EvalObj.t)) =>
    m.next_steps |> Calc.get_saved_exc(~print="get_next_steps");

  let get_selected_exp = (m: t): Exp.t =>
    m.selected_exp
    |> Calc.saved_to_option
    |> Option.join
    |> OptUtil.get(() => EmptyHole |> Exp.fresh);
};

module Update = {
  let calculate =
      (
        ~settings as _,
        exp,
        _state,
        new_next_steps,
        {next_steps: _, rewrites, selected_exp, selected_id}: Model.t,
        editor,
      )
      : Model.t => {
    let selected_id =
      // hacky way to get a currently-selected id
      {
        let editor: CodeSelectable.Model.t = editor |> Calc.get_value;
        try({
          let zipper = editor.editor.state.zipper;
          let selection = zipper.selection.content;
          let skel = Segment.skel(selection);
          let root = Skel.root(skel);
          let idx = Aba.first_a(root);
          let piece = List.nth(selection, idx);
          let id = Piece.id(piece);
          Some(id);
        }) {
        | _ => None
        };
      }
      |> Calc.set(_, selected_id);
    let selected_exp =
      selected_exp
      |> {
        let.calc selected_id = selected_id
        and.calc exp = exp;
        open OptUtil.Syntax;
        let* id = selected_id;
        let* exp' = ProofHacks.find_exp_id(id, exp);
        Some(exp');
      };
    let rewrites =
      rewrites
      |> {
        let.calc exp = selected_exp;
        open OptUtil.Syntax;
        let* exp' = exp;
        Some(Model.{rewrites: ProofCtx.get_rewrites(Axioms.v, exp')});
      };
    {
      next_steps: new_next_steps |> Calc.save,
      rewrites: rewrites |> Calc.save,
      selected_exp: selected_exp |> Calc.save,
      selected_id: selected_id |> Calc.save,
    };
  };
};

module View = {
  type event =
    | AddInduction
    | AddForall
    | HideStepper
    | AddAxiom(Exp.t, Exp.t);

  let view_justification =
      (
        ~globals: Globals.t,
        ~signal,
        ~undo: option(Ui_effect.t(unit)),
        _model: Model.t,
      ) => {
    let button_back =
      Widgets.button_d(
        Icons.undo,
        switch (undo) {
        | Some(u) => u
        | None => Ui_effect.Ignore
        },
        ~disabled=Option.is_none(undo),
        ~tooltip="Step Backwards",
      );
    let button_induction =
      Widgets.button_d(
        Icons.star,
        signal(AddInduction),
        ~disabled=false,
        ~tooltip="Begin a proof by induction",
      );
    let button_forall =
      Widgets.button_d(
        Icons.star,
        signal(AddForall),
        ~disabled=false,
        ~tooltip="Prove a forall",
      );
    let button_hide_stepper =
      Widgets.toggle(~tooltip="Show Stepper", "s", true, _ =>
        signal(HideStepper)
      );
    let toggle_show_history =
      Widgets.toggle(
        ~tooltip="Show History",
        "h",
        globals.settings.core.evaluation.stepper_history,
        _ =>
        globals.inject_global(Set(Evaluation(ShowRecord)))
      );
    let eval_settings =
      Widgets.button(Icons.gear, _ =>
        globals.inject_global(Set(Evaluation(ShowSettings)))
      );
    Web.Node.div(
      ~attrs=[Web.Attr.classes(["stepper-controls"])],
      [
        button_back,
        button_induction,
        button_forall,
        eval_settings,
        toggle_show_history,
        button_hide_stepper,
      ],
    );
  };

  let view_step_content = (~globals, ~signal, model: Model.t) => {
    (
      model.rewrites
      |> Calc.get_saved_exc(~print="view_step_rewrites")
      |> Option.value(~default=Model.{rewrites: []})
      |> (r => r.rewrites)
      |> List.is_empty
        ? []
        : [
          // Web.Node.text("Selection:"),
          // switch (
          //   model.selected_exp |> Calc.get_saved_exc(~print="view_step_content")
          // ) {
          // | Some(exp) =>
          //   exp
          //   |> Haz3lcore.ExpToSegment.(
          //        exp_to_segment(
          //          ~settings=
          //            Settings.of_core(~inline=false, globals.settings.core),
          //        )
          //      )
          //   |> CodeViewable.view_segment(
          //        ~globals,
          //        ~sort=Exp,
          //        ~shape_map=Haz3lcore.Id.Map.empty,
          //      )
          // | None => Web.Node.text("(None)")
          // },
          Web.Node.text("Rewrites:"),
        ]
    )
    @ (
      List.map(
        (exp: Exp.t) =>
          [
            exp
            |> Haz3lcore.ExpToSegment.(
                 exp_to_segment(
                   ~settings=
                     Settings.of_core(~inline=false, globals.settings.core),
                 )
               )
            |> CodeViewable.view_segment(
                 ~globals,
                 ~sort=Exp,
                 ~shape_map=Haz3lcore.Id.Map.empty,
               ),
            Widgets.button(Icons.star, _ =>
              signal(AddAxiom(Model.get_selected_exp(model), exp))
            ),
          ],
        model.rewrites
        |> Calc.get_saved_exc(~print="view_step_rewrites")
        |> Option.value(~default=Model.{rewrites: []})
        |> (r => r.rewrites),
      )
      |> List.flatten
    );
  };
};
