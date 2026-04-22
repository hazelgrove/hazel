open Util;
open WebUtil;
open Calc.Syntax;
open Language;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    filter: Calc.t(string),
    all_rules: Calc.saved(ProofCtx.t),
    filtered_rewrites: Calc.saved(list(AssumptionBox.Model.t)),
  };

  let init = {
    filter: Calc.NewValue(""),
    all_rules: Calc.Pending,
    filtered_rewrites: Calc.Pending,
  };
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | SetFilter(string);

  let update = (~settings as _, action, model): Updated.t(Model.t) => {
    switch (action) {
    | SetFilter(filter) =>
      Model.{
        ...model,
        filter: Calc.NewValue(filter),
      }
      |> Updated.return_quiet
    };
  };

  let calculate =
      (
        ~info_map: Calc.t(Statics.Map.t),
        ~ctx: Calc.t(SemanticCtx.t),
        ~selected_exp: Calc.t(option(Exp.t)),
        model: Model.t,
      )
      : Model.t => {
    let all_rules =
      model.all_rules
      |> {
        let.calc ctx = ctx;
        let env = SemanticCtx.get_env(ctx);
        let ctx = SemanticCtx.get_ctx(ctx);
        ProofCtx.of_env(~builtins=Axioms.v, ~ctx, env);
      };

    let filtered_rewrites =
      model.filtered_rewrites
      |> {
        let.calc all_rules = all_rules
        and.calc ctx = ctx
        and.calc filter = model.filter
        and.calc selected_exp = selected_exp
        and.calc info_map = info_map;

        let all_assumption_boxes =
          all_rules
          |> (
            filter == ""
              ? x => x
              : List.filter(({name, _}: ProofCtx.entry) =>
                  StringUtil.subseq_search(name, filter)
                )
          )
          |> List.map(ctx_entry =>
               AssumptionBox.Model.{ctx_entry: ctx_entry}
             )
          |> (
            filter == ""
              ? List.filter((ab: AssumptionBox.Model.t) =>
                  switch (selected_exp) {
                  | Some(selected_exp) =>
                    ProofRule.is_active(
                      ~info_map,
                      ~env=SemanticCtx.get_env(ctx),
                      ab.ctx_entry.rule,
                      selected_exp |> DHExp.strip_ascriptions,
                    )
                  | None => false
                  }
                )
              : (x => x)
          );

        all_assumption_boxes;
      };
    {
      filter: model.filter |> Calc.make_old,
      all_rules: all_rules |> Calc.save,
      filtered_rewrites: filtered_rewrites |> Calc.save,
    };
  };
};

module Selection = {
  open Cursor;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = unit;

  let get_cursor_info =
      (~selection as (): t, _model: Model.t): cursor(Update.t) => {
    empty;
  };

  let handle_key_event =
      (~selection as (): t, _model: Model.t, _event): option(Update.t) => {
    None;
  };
};

module View = {
  let view =
      (
        ~globals,
        ~info_map,
        ~env,
        ~full_exp,
        ~selected_exp,
        ~inject: Update.t => Ui_effect.t(unit),
        ~take_focus: Selection.t => Ui_effect.t(unit),
        ~add_axiom_step:
           (string, int, Exp.t, Direction.t, string) => Ui_effect.t(unit),
        model: Model.t,
      ) => {
    let unpacked_rewrites =
      model.filtered_rewrites
      |> Calc.get_saved_exc(~print="view_step_rewrites");
    [
      Node.input(
        ~attrs=[
          Attr.value(model.filter |> Calc.get_value),
          Attr.placeholder("search assumptions..."),
          Attr.on_focus(_ => take_focus()),
          Attr.on_input((_, s) => inject(SetFilter(s))),
        ],
        (),
      ),
    ]
    @ List.map(
        (am: AssumptionBox.Model.t) =>
          AssumptionBox.View.view(
            ~globals,
            ~info_map,
            ~env,
            ~active_selection=
              Some((
                selected_exp,
                [],
                fun
                | AssumptionBox.EqualityLeft(e) => {
                    add_axiom_step(
                      am.ctx_entry.name,
                      try(ProofHacks.exp_idx(selected_exp, full_exp)) {
                      | _ => 0
                      },
                      selected_exp,
                      Left,
                      e,
                    );
                  }
                | AssumptionBox.EqualityRight(e) =>
                  add_axiom_step(
                    am.ctx_entry.name,
                    try(ProofHacks.exp_idx(selected_exp, full_exp)) {
                    | _ => 0
                    },
                    selected_exp,
                    Right,
                    e,
                  ),
              )),
            am,
          ),
        unpacked_rewrites,
      );
  };
};
