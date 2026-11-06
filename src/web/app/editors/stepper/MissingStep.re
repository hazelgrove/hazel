open Language;
open Util;
open WebUtil;
open Calc.Syntax;
open Haz3lcore;

module Model = {
  /* The proof forms that cite an in-scope fact, picked from a list. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type fact_form =
    | Revert
    | Contradiction;

  let keyword = (f: fact_form): string =>
    switch (f) {
    | Revert => "revert"
    | Contradiction => "contradiction"
    };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type open_box =
    | AxiomsOpen(AxiomsBox.Model.t)
    | RewritesOpen({
        editor: CodeEditable.Model.t,
        cached_exp: Calc.saved(Exp.t),
        cached_result: option(bool),
      })
    /* A pick list with no state of its own: the candidates are read off
       `assumptions` when it renders. */
    | FactPicksOpen(fact_form)
    | NoneOpen;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type assumptions = list(AssumptionBox.Model.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    /* The hole this row stands for, or None for the row past the end of
     * the chain, which extends the step before it. */
    proof: Calc.saved(option(Proof.t)),
    /* The goal, whose selection the picker acts on. It is rebuilt only
     * when the goal changes, so the selection survives recalculation. */
    editor: Calc.saved(CodeSelectable.Model.t),
    next_steps: Calc.saved(EvaluatorStep.status),
    refls: Calc.saved(list(Exp.t)),
    selected_id: Calc.saved(option(Id.t)),
    selected_exp: Calc.saved(option(Exp.t)),
    full_exp: Calc.saved(Exp.t),
    /* The goal as written. `full_exp` is the env-substituted display
     * (`<sum>([])`); eval steps quote this form, where an inlined `FixF`
     * isn't mistaken for an automatic unwrap. */
    eval_exp: Calc.saved(Exp.t),
    /* For each step in `next_steps`, the node of `eval_exp` that taking
     * it writes as `eval <node> at <n> end`. */
    step_sources: Calc.saved(list(Id.t)),
    assumptions: Calc.saved(option(assumptions)),
    open_box,
    cached_env: Calc.saved(Environment.t(Exp.t)) // TODO[Matt]: remove this later, just to get env into view for now.
  };

  let init = {
    proof: Calc.Pending,
    editor: Calc.Pending,
    next_steps: Calc.Pending,
    refls: Calc.Pending,
    selected_id: Calc.Pending,
    selected_exp: Calc.Pending,
    full_exp: Calc.Pending,
    eval_exp: Calc.Pending,
    step_sources: Calc.Pending,
    assumptions: Calc.Pending,
    open_box: NoneOpen,
    cached_env: Calc.Pending,
  };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | ToggleAxioms
    | ProposeRewrite
    | UpdateResult(bool)
    | RewriteEditorAction(CodeEditable.Update.t)
    | AxiomBoxAction(AxiomsBox.Update.t)
    | ToggleFactPicks(Model.fact_form);

  let update = (~settings, action, model: Model.t): Updated.t(Model.t) => {
    switch (action, model.open_box) {
    | (ToggleAxioms, _) =>
      let open_box =
        switch (model.open_box) {
        | NoneOpen
        | RewritesOpen(_)
        | FactPicksOpen(_) => Model.AxiomsOpen(AxiomsBox.Model.init)
        | AxiomsOpen(_) => Model.NoneOpen
        };
      Model.{
        ...model,
        open_box,
      }
      |> Updated.return_quiet(~logged=true);
    | (ProposeRewrite, _) =>
      let open_box =
        switch (model.open_box) {
        | NoneOpen
        | AxiomsOpen(_)
        | FactPicksOpen(_) =>
          Model.RewritesOpen({
            editor:
              CodeEditable.Model.mk(
                Editor.Model.mk(Zipper.init(), ~root=Exp),
              ),
            cached_exp: Calc.Pending,
            cached_result: None,
          })
        | RewritesOpen(_) => Model.NoneOpen
        };
      Model.{
        ...model,
        open_box,
      }
      |> Updated.return_quiet(~recalculate=true, ~logged=true);
    | (RewriteEditorAction(action), RewritesOpen({editor, _} as r)) =>
      let* new_editor = CodeEditable.Update.update(~settings, action, editor);
      Model.{
        ...model,
        open_box:
          Model.RewritesOpen({
            ...r,
            editor: new_editor,
          }),
      };
    | (RewriteEditorAction(_), _) => model |> Updated.raise_invalid_action
    | (UpdateResult(result), RewritesOpen(r)) =>
      Model.{
        ...model,
        open_box:
          Model.RewritesOpen({
            ...r,
            cached_result: Some(result),
          }),
      }
      |> Updated.return_quiet(~logged=true)
    | (UpdateResult(_), _) => model |> Updated.raise_invalid_action
    | (AxiomBoxAction(action), AxiomsOpen(m)) =>
      let* updated = AxiomsBox.Update.update(~settings, action, m);
      Model.{
        ...model,
        open_box: Model.AxiomsOpen(updated),
      };
    | (AxiomBoxAction(_), _) => model |> Updated.raise_invalid_action
    /* The button that opened a pick list closes it. */
    | (ToggleFactPicks(form), _) =>
      let open_box =
        switch (model.open_box) {
        | FactPicksOpen(open_form) when open_form == form => Model.NoneOpen
        | NoneOpen
        | AxiomsOpen(_)
        | RewritesOpen(_)
        | FactPicksOpen(_) => Model.FactPicksOpen(form)
        };
      Model.{
        ...model,
        open_box,
      }
      |> Updated.return_quiet(~recalculate=true, ~logged=true);
    };
  };

  /* Selection in the row's editor is what the overlay acts on, so the
   * stepper routes its editor actions here. */
  let update_editor =
      (~settings, action: CodeSelectable.Update.t, model: Model.t)
      : Updated.t(Model.t) =>
    switch (model.editor) {
    | Calc.Calculated(editor) =>
      let* editor = CodeSelectable.Update.update(~settings, action, editor);
      Model.{
        ...model,
        editor: Calc.Calculated(editor),
      };
    | Calc.Pending => model |> Updated.raise_invalid_action
    };

  /* The checker's `eval` step passes through `VarLookup`s silently
   * (Evaluator.Proofs.advance_hidden), so a step the stepper offers can't always
   * be written at its own node: in `all_lists(xs) == true` the only step
   * is the lookup of `all_lists`. Each step is offered as the step the
   * checker takes at the nearest node around it where it takes one, here
   * `all_lists(xs)`, and that node is the step's source. */

  /* Past this many silent steps nothing is offered, so a goal whose
   * hidden steps diverge can't hang the stepper. */
  let max_silent_steps = 64;

  let checker_step = (~env, e: Exp.t): option(EvaluatorStep.step) => {
    open OptUtil.Syntax;
    let* (_, e) =
      Evaluator.Proofs.advance_hidden(~max_steps=max_silent_steps, ~env, e);
    switch (EvaluatorStep.get_status(~settings=CoreSettings.on, e, env)) {
    | AvailableSteps([s, ..._]) => Some(s)
    | AvailableSteps([])
    | AutoStep(_) => None
    };
  };

  /* The `n` for which the checker's `eval <at_exp> at <n> end` finds
   * `at_exp` itself rather than an equal node before it. The checker
   * matches env-substituted terms (Occurrence.nth_env_target), so
   * `substituted` is the written goal with `env` substituted.
   *
   * None if `at_exp` holds a negative number. Evaluation produces those,
   * but source can't spell them: written out, `-5` reads back as a
   * negation, which the checker doesn't match against the number. */
  let checker_idx = (~env, ~substituted: Exp.t, at_exp: Exp.t): option(int) => {
    let rec has_negative_number = (e: Exp.t) =>
      switch (Exp.term_of(e)) {
      | Atom(Int(n)) => Bigint.compare(n, Bigint.of_int(0)) < 0
      | Atom(SInt(n)) => n < 0
      | Atom(Float(f)) => f < 0.0
      | _ => List.exists(has_negative_number, Occurrence.children(e))
      };
    let target = Substitution.in_exp(env, at_exp);
    let rec find = n =>
      switch (Occurrence.nth(target, n, substituted)) {
      | Some(found) when Exp.rep_id(found) == Exp.rep_id(at_exp) => Some(n)
      | Some(_) => find(n + 1)
      | None => None
      };
    has_negative_number(at_exp) ? None : find(0);
  };

  /* The steps to offer on the written goal, each with its source. A step
   * with no source the checker can find isn't offered, and steps with the
   * same source are offered once. */
  let offered_steps =
      (~settings: CoreSettings.t, ~env, written: Exp.t)
      : list((EvaluatorStep.step, Id.t)) => {
    let substituted = lazy(Substitution.in_exp(env, written));
    /* The node with `id` and the nodes enclosing it, nearest first. */
    let rec self_and_ancestors = (id, e: Exp.t): option(list(Exp.t)) =>
      Exp.rep_id(e) == id
        ? Some([e])
        : Occurrence.children(e)
          |> List.find_map(self_and_ancestors(id))
          |> Option.map(path => path @ [e]);
    let offer = (s: EvaluatorStep.step) =>
      self_and_ancestors(EvaluatorStep.get_step_id(s), written)
      |> Option.value(~default=[])
      |> List.find_map(at_exp =>
           switch (checker_step(~env, at_exp)) {
           | Some(step)
               when
                 checker_idx(
                   ~env,
                   ~substituted=Lazy.force(substituted),
                   at_exp,
                 )
                 != None =>
             Some((step, Exp.rep_id(at_exp)))
           | _ => None
           }
         );
    let candidates =
      switch (EvaluatorStep.get_status(~settings, written, env)) {
      | AutoStep(s) => [s]
      | AvailableSteps(steps) => steps
      };
    candidates
    |> List.filter_map(offer)
    |> ListUtil.dedup_f(((_, id), (_, id')) => id == id');
  };

  let step_source = (model: Model.t, step: EvaluatorStep.step): option(Id.t) =>
    switch (model.next_steps, model.step_sources) {
    | (Calculated(AvailableSteps(steps)), Calculated(sources))
        when List.length(steps) == List.length(sources) =>
      List.combine(steps, sources) |> List.assq_opt(step)
    | _ => None
    };

  /* What taking `step` writes: `eval <at_exp> at <at_idx> end`. */
  let written_target =
      (model: Model.t, step: EvaluatorStep.step): option((Exp.t, int)) => {
    open OptUtil.Syntax;
    let* id = step_source(model, step);
    let* written = model.eval_exp |> Calc.get_saved_opt;
    let* env = model.cached_env |> Calc.get_saved_opt;
    let* at_exp = Exp.find_by_id(id, written);
    let+ at_idx =
      checker_idx(
        ~env,
        ~substituted=Substitution.in_exp(env, written),
        at_exp,
      );
    (at_exp, at_idx);
  };

  /* Map a step's source from the written goal onto the displayed term so
   * underlines land on `<sum>` rather than a vanished `Var`. */
  let display_id = (~written: Exp.t, ~display: Exp.t, source: Id.t): Id.t =>
    Occurrence.map_rep_id(~from=written, ~onto=display, source)
    |> Option.value(~default=source);

  let display_step_id = (model: Model.t, step: EvaluatorStep.step): Id.t =>
    switch (
      step_source(model, step),
      model.eval_exp |> Calc.get_saved_opt,
      model.full_exp |> Calc.get_saved_opt,
    ) {
    | (Some(source), Some(written), Some(display)) =>
      display_id(~written, ~display, source)
    | (Some(source), _, _) => source
    | (None, _, _) => EvaluatorStep.get_step_id(step)
    };

  let calculate =
      (
        ~settings: CoreSettings.t,
        ~exp: Calc.t(Exp.t),
        ~eval_exp as eval_in: Calc.t(Exp.t)=exp,
        ~ctx: Calc.t(SemanticCtx.t),
        {
          proof,
          editor,
          next_steps,
          refls,
          assumptions,
          selected_exp,
          full_exp,
          eval_exp: saved_eval_exp,
          step_sources,
          selected_id,
          open_box,
          cached_env,
        }: Model.t,
      )
      : Model.t => {
    /* Rekey the incoming expression against the one this row last
     * rendered, so callers can hand us a fresh `Calc.t` without forcing
     * the editor (and the selection it holds) to be rebuilt. */
    let exp = Calc.set(~eq=Exp.fast_equal, Calc.get_value(exp), full_exp);
    let eval_exp =
      Calc.set(~eq=Exp.fast_equal, Calc.get_value(eval_in), saved_eval_exp);
    let editor =
      editor
      |> {
        let.calc exp = exp
        and.calc ctx = ctx;
        CodeSelectable.Model.mk_from_exp(~settings, ~root=Exp, exp)
        |> CodeEditable.Update.calculate(
             ~settings,
             ~is_edited=true,
             ~is_dynamic_term=true,
             ~dynamics=Dynamics.Map.empty,
             ~stitch=x => x,
             ~ctx=SemanticCtx.get_ctx(ctx),
           );
      };
    /* Statics of the row's own editor: the expressions shown here are
     * elaboration output with freshened ids, so no info map from further
     * up covers them. */
    let info_map =
      editor |> Calc.map(e => CodeEditable.Model.get_statics(e).info_map);
    /* Eval steps are computed on the written goal + env. The displayed
     * term already has `sum` inlined as `FixF`, and those hidden
     * FixUnwraps would otherwise collapse `get_status` to AutoStep and
     * wipe the overlay. */
    let offered =
      Calc.saved_pair((step_sources, next_steps))
      |> {
        let.calc eval_exp = eval_exp
        and.calc ctx = ctx;
        let offered =
          offered_steps(~settings, ~env=SemanticCtx.get_env(ctx), eval_exp);
        (
          List.map(snd, offered),
          EvaluatorStep.AvailableSteps(List.map(fst, offered)),
        );
      };
    let step_sources = offered |> Calc.map(fst);
    let new_next_steps = offered |> Calc.map(snd);
    let selected_id =
      // hacky way to get a currently-selected id
      {
        let editor: CodeSelectable.Model.t = editor |> Calc.get_value;
        try(
          {
            open OptUtil.Syntax;
            let zipper = editor.editor.state.zipper;
            let* id =
              TermData.get_root_id_using_ranges(
                zipper.selection.content,
                editor.editor.syntax.term_data,
                CachedSyntax.measured(editor.editor.syntax),
              );
            Some(id);
          }
        ) {
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
        let* exp' = Exp.find_by_id(id, exp);
        Some(exp');
      };
    let assumptions =
      assumptions
      |> {
        let.calc _exp = selected_exp
        and.calc ctx = ctx;
        let proof_ctx =
          ProofCtx.of_theorem_ctx(
            ~builtins=Axioms.v,
            SemanticCtx.get_ctx(ctx),
          )
          |> List.map(ctx_entry => AssumptionBox.Model.{ctx_entry: ctx_entry});
        Some(proof_ctx);
      };
    let refls =
      refls
      |> {
        let.calc exp = exp
        and.calc eval_exp = eval_exp
        and.calc ctx = ctx
        and.calc step_sources = step_sources
        and.calc info_map = info_map;
        let step_ids =
          List.map(
            display_id(~written=eval_exp, ~display=exp),
            step_sources,
          );
        MatchExp.find_refls(~info_map, ~env=SemanticCtx.get_env(ctx), exp)
        |> List.filter(e => !List.mem(Exp.rep_id(e), step_ids));
      };
    let open_box =
      switch (open_box) {
      | RewritesOpen({editor, cached_exp, cached_result}) =>
        // Calculate syntax, holes, types, etc for the editor
        let editor =
          CodeEditable.Update.calculate(
            ~settings,
            ~is_edited=true,
            ~is_dynamic_term=true,
            ~dynamics=Dynamics.Map.empty,
            ~stitch=x => x,
            ~ctx=Calc.get_value(ctx) |> SemanticCtx.get_ctx,
            editor,
          );
        // Extract an exp from the editor
        let cached_exp =
          Calc.set(
            ~eq=Exp.fast_equal_with_lexemes,
            CodeEditable.Model.get_statics(editor).elaborated,
            cached_exp,
          );
        // Reset result if editor changes
        let cached_result =
          Calc.Calculated(cached_result)
          |> {
            let.calc _ = cached_exp;
            None;
          };
        Model.RewritesOpen({
          editor,
          cached_exp: cached_exp |> Calc.save,
          cached_result: cached_result |> Calc.get_value,
        });
      | AxiomsOpen(m) =>
        AxiomsOpen(
          AxiomsBox.Update.calculate(~info_map, ~ctx, ~selected_exp, m),
        )
      /* Nothing to calculate: a pick list holds no editor, and its
         candidates come from `assumptions`, computed above. */
      | FactPicksOpen(_) as picks => picks
      | NoneOpen => NoneOpen
      };
    let cached_env =
      cached_env
      |> {
        let.calc ctx = ctx;
        SemanticCtx.get_env(ctx);
      };
    {
      proof,
      editor: editor |> Calc.save,
      next_steps: new_next_steps |> Calc.save,
      refls: refls |> Calc.save,
      assumptions: assumptions |> Calc.save,
      full_exp: exp |> Calc.save,
      eval_exp: eval_exp |> Calc.save,
      step_sources: step_sources |> Calc.save,
      selected_exp: selected_exp |> Calc.save,
      selected_id: selected_id |> Calc.save,
      cached_env: cached_env |> Calc.save,
      open_box,
    };
  };
};

module Selection = {
  open Cursor;
  // Selection handles focus

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | RewriteEditor(CodeEditable.Selection.t)
    | AxiomBoxSelection(AxiomsBox.Selection.t);

  let get_cursor_info =
      (~inject, ~selection: t, model: Model.t): cursor(Update.t) => {
    switch (selection, model.open_box) {
    | (RewriteEditor(selection), RewritesOpen({editor, _})) =>
      let+ ci =
        CodeEditable.Selection.get_cursor_info(
          ~inject=a => inject(Update.RewriteEditorAction(a)),
          ~selection,
          editor,
        );
      Update.RewriteEditorAction(ci);
    | (RewriteEditor(_), _) => empty
    | (AxiomBoxSelection(selection), AxiomsOpen(m)) =>
      let+ ci = AxiomsBox.Selection.get_cursor_info(~selection, m);
      Update.AxiomBoxAction(ci);
    | (AxiomBoxSelection(_), _) => empty
    };
  };
};

module View = {
  open OptUtil.Syntax;
  type event =
    | AddInduction(option(Exp.t))
    | AddForall
    /* `funext <binder> => ?`, with the goal's left-hand function's own
       parameter pattern as the binder where it has one
       (`ProofCheck.Funext.binder`), or a hole. */
    | AddFunext(option(Pat.t))
    | HideStepper
    | AddAxiomStep(string, int, Exp.t, Direction.t, string)
    | AddAlgebriteStep(int, Exp.t, Exp.t)
    /* The wrapping forms, `<keyword> <exp> => ?` around this row's hole
       (see StepperBase.Terms). The argument is a hole for the forms with
       nothing to search, which the user then types into, or the
       expression picked from a list. */
    | AddAssume(Exp.t)
    | AddRevert(Exp.t)
    | AddGeneralize(Exp.t)
    | AddHave(Exp.t)
    | AddContradiction(Exp.t)
    | MakeActive(Selection.t)
    | TakeStep(int)
    | Refl(int);

  let get_segment_bounds = (~measured: Measured.t, segment: Segment.t) => {
    let* first_piece = ListUtil.hd_opt(segment);
    let Point.{row: start_y, col: start_x} =
      Measured.find_p(~msg="get_segment_bounds", first_piece, measured)
      |> (m => m.origin);
    let* last_piece = ListUtil.last_opt(segment);
    let Point.{row: end_y, col: end_x} =
      Measured.find_p(~msg="get_segment_bounds", last_piece, measured)
      |> (m => m.last);
    let rec get_left = (current_left: int, row: int, final_row: int) =>
      if (row > final_row) {
        current_left;
      } else {
        get_left(
          Int.min(
            current_left,
            Measured.Rows.find(row, measured.rows).content_start,
          ),
          row + 1,
          final_row,
        );
      };
    let left = get_left(start_x, start_y, end_y);
    let rec get_right = (current_right: int, row: int, final_row: int) =>
      if (row == final_row) {
        current_right;
      } else {
        get_right(
          Int.max(
            current_right,
            Measured.Rows.find(row, measured.rows).max_col,
          ),
          row + 1,
          final_row,
        );
      };
    let right = get_right(end_x, start_y, end_y);
    Some((left, right, start_y, end_y + 1));
  };

  let selected_exp = (model: Model.t): option(Exp.t) =>
    model.selected_exp |> Calc.get_saved_exc(~print="Selected Exp");

  let full_exp = (model: Model.t): Exp.t =>
    model.full_exp |> Calc.get_saved_exc(~print="full_exp not cached");

  /* The buttons the picker offers for the row's selection, as labels
   * and what clicking does. A menu only picks, never edits: a form with
   * an argument to search (revert and contradiction match an in-scope
   * fact) opens a pick list, and one with nothing to search (assume,
   * generalize and have take any proposition) inserts the step with a
   * hole argument for the user to type into. */
  let buttons =
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~info_map: Statics.Map.t,
        model: Model.t,
      )
      : list((string, Ui_effect.t(unit))) => {
    let selected = selected_exp(model);
    let full_exp = full_exp(model);
    let env = Calc.get_saved(Environment.empty, model.cached_env);
    /* Inserting a step forces the record open, or the row just written
     * would not be on screen. */
    let insert = (event: event) =>
      Ui_effect.Many([
        globals.inject_global(Set(Evaluation(ForceShowRecord))),
        signal(event),
      ]);
    let hole = () => Exp.fresh(EmptyHole);
    let index_of_selected = ids =>
      switch (selected) {
      | Some(e) => List.find_index(id => id == Exp.rep_id(e), ids)
      | None => None
      };
    let step =
      switch (
        model.next_steps
        |> Calc.get_saved_exc(~print="next_steps")
        |> (
          fun
          | AutoStep(_) => []
          | AvailableSteps(steps) => steps
        )
        |> List.map(Update.display_step_id(model))
        |> index_of_selected
      ) {
      | Some(idx) => [("Step", signal(TakeStep(idx)))]
      | None => []
      };
    let refl =
      switch (
        model.refls
        |> Calc.get_saved_exc(~print="refls")
        |> List.map(Exp.rep_id)
        |> index_of_selected
      ) {
      | Some(idx) => [("Reflexivity", insert(Refl(idx)))]
      | None => []
      };
    let function_body =
      selected == Some(full_exp) && Exp.is_fun(full_exp)
        ? [("Function Body", insert(AddForall))] : [];
    /* Exactly when the goal is `f == g` at an arrow type, by the checker's
     * own detector, so the button never offers a step that would mark.
     * The arrow type keeps it off `==` between lists of functions. */
    let funext =
      ProofCheck.Funext.goal(~info_map, ~env, full_exp) == None
        ? []
        : [
          (
            "Funext",
            insert(
              AddFunext(ProofCheck.Funext.binder(~info_map, ~env, full_exp)),
            ),
          ),
        ];
    /* With goal `A ==> B`, assuming exactly A strips the antecedent. */
    let assume_antecedent =
      switch (Exp.term_of(full_exp)) {
      | BinOp(Bool(Implies), a, _) => [
          ("Assume antecedent", insert(AddAssume(a))),
        ]
      | _ => []
      };
    step
    @ refl
    @ function_body
    @ funext
    @ [
      ("Algebra ▼", inject(ProposeRewrite)),
      ("Assumptions ▼", inject(ToggleAxioms)),
      ("Assume", insert(AddAssume(hole()))),
    ]
    @ assume_antecedent
    @ [
      ("Revert ▼", inject(ToggleFactPicks(Revert))),
      ("Generalize", insert(AddGeneralize(hole()))),
      ("Have", insert(AddHave(hole()))),
      ("Contradiction ▼", inject(ToggleFactPicks(Contradiction))),
      ("Cases/Induction", insert(AddInduction(selected))),
    ];
  };

  /* The box a dropdown button opens below the buttons. */
  module Box = {
    let view_axioms =
        (
          ~globals: Globals.t,
          ~signal: event => Ui_effect.t(unit),
          ~inject: Update.t => Ui_effect.t(unit),
          ~info_map: Statics.Map.t,
          model: Model.t,
          axioms: AxiomsBox.Model.t,
        ) =>
      div_c(
        "axiom-box",
        AxiomsBox.View.view(
          ~globals,
          ~info_map,
          ~env=
            model.cached_env |> Calc.get_saved_exc(~print="env not cached"),
          ~inject=(a: AxiomsBox.Update.t) => inject(AxiomBoxAction(a)),
          ~take_focus=
            (s: AxiomsBox.Selection.t) =>
              signal(MakeActive(AxiomBoxSelection(s))),
          ~add_axiom_step=
            (a, b, c, d, e) => signal(AddAxiomStep(a, b, c, d, e)),
          ~full_exp=full_exp(model),
          ~selected_exp=
            selected_exp(model)
            |> Option.value(~default=EmptyHole |> Exp.fresh, _),
          axioms,
        ),
      );

    /* Replace the selection with an expression the user types, once the
     * rewrite checker accepts the two as equal. */
    let view_rewrite =
        (
          ~globals: Globals.t,
          ~signal: event => Ui_effect.t(unit),
          ~inject: Update.t => Ui_effect.t(unit),
          ~selected: option(Selection.t),
          model: Model.t,
          editor: CodeEditable.Model.t,
          cached_exp: Calc.saved(Exp.t),
          cached_result: option(bool),
        ) => {
      let replacement =
        Calc.get_saved_exc(~print="cached exp not calculated", cached_exp);
      let selected_exp =
        selected_exp(model) |> Option.value(~default=Exp.fresh(EmptyHole));
      let in_env = e =>
        Substitution.in_exp(
          model.cached_env |> Calc.get_saved_exc(~print="env not cached"),
          e,
        );
      let check_or_replace =
        switch (cached_result) {
        | Some(true) => [
            Node.text("Valid"),
            Widgets.button(
              ~clss=["proof-button"],
              Node.text("Replace"),
              ~tooltip="replace",
              _ =>
              switch (Occurrence.index(selected_exp, full_exp(model))) {
              | Some(at_idx) =>
                signal(
                  AddAlgebriteStep(
                    at_idx,
                    selected_exp,
                    in_env(replacement),
                  ),
                )
              | None => Ui_effect.Ignore
              }
            ),
          ]
        | Some(false) => [Node.text("Invalid")]
        | None => [
            Widgets.button(
              ~clss=["proof-button"],
              Node.text("Check"),
              _ =>
                inject(
                  UpdateResult(
                    RewriteChecker.check_rewrite(
                      in_env(selected_exp),
                      in_env(replacement),
                    ),
                  ),
                ),
              ~tooltip="check",
            ),
          ]
        };
      div_c(
        "rewrite-box",
        [
          Node.text("Replace: "),
          CodeViewable.view_any(
            ~globals,
            ~settings=
              ExpToSegment.Settings.of_core(
                ~inline=false,
                ~fold_fn_bodies=`Text,
                globals.settings.core,
              ),
            Exp(selected_exp),
          ),
          Node.text("With: "),
          div_c(
            "inline-editor-wrapper",
            [
              CodeEditable.View.view(
                ~globals,
                ~signal=
                  fun
                  | MakeActive => signal(MakeActive(RewriteEditor())),
                ~edit_mode=
                  EditMode.Editable({
                    inject: x => inject(RewriteEditorAction(x)),
                    escape: _ => Ui_effect.Ignore,
                    take_focus: _ => Ui_effect.Ignore,
                    focus:
                      switch (selected) {
                      | Some(RewriteEditor ()) => Some()
                      | _ => None
                      },
                  }),
                ~dynamics=Dynamics.Map.empty,
                editor,
              ),
            ],
          ),
        ]
        @ check_or_replace,
      );
    };

    /* The in-scope facts `form` can cite, each inserting the step with
     * that fact in place. `revert` and `contradiction` match a fact by
     * `Exp.fast_equal` (ProofCheck.Discharge.lookup), so typing it is error
     * prone. The list is the Assumptions box's, without the builtin
     * axioms (rules, not facts of this scope) and captured entries. A
     * `with` clause is typed in afterwards. */
    let view_picks =
        (
          ~globals: Globals.t,
          ~signal: event => Ui_effect.t(unit),
          model: Model.t,
          form: Model.fact_form,
        ) => {
      let keyword = Model.keyword(form);
      let pick = (~label: string, exp: Exp.t) =>
        Widgets.button(
          ~clss=["proof-button"],
          ~tooltip=
            "Write `" ++ keyword ++ " " ++ label ++ " =>` into the proof",
          Node.text(label),
          _ =>
          Ui_effect.Many([
            globals.inject_global(Set(Evaluation(ForceShowRecord))),
            switch (form) {
            | Revert => signal(AddRevert(exp))
            | Contradiction => signal(AddContradiction(exp))
            },
          ])
        );
      let picks =
        model.assumptions
        |> Calc.get_saved_opt
        |> Option.join
        |> Option.value(~default=[])
        |> List.filter((ab: AssumptionBox.Model.t) =>
             !ab.ctx_entry.is_captured
             && !
                  List.exists(
                    (e: ProofCtx.entry) => e.name == ab.ctx_entry.name,
                    Axioms.v,
                  )
           )
        |> List.map((ab: AssumptionBox.Model.t) =>
             pick(~label=ab.ctx_entry.name, ab.ctx_entry.exp)
           );
      div_c(
        "proof-form-box",
        picks == []
          ? [Node.text("No in-scope facts to " ++ keyword ++ ".")]
          : [Node.text(keyword ++ " ")] @ picks,
      );
    };

    let view =
        (
          ~globals: Globals.t,
          ~signal: event => Ui_effect.t(unit),
          ~inject: Update.t => Ui_effect.t(unit),
          ~selected: option(Selection.t),
          ~info_map: Statics.Map.t,
          model: Model.t,
        )
        : list(Node.t) =>
      switch (model.open_box) {
      | NoneOpen => []
      | AxiomsOpen(axioms) => [
          view_axioms(~globals, ~signal, ~inject, ~info_map, model, axioms),
        ]
      | RewritesOpen({editor, cached_exp, cached_result}) => [
          view_rewrite(
            ~globals,
            ~signal,
            ~inject,
            ~selected,
            model,
            editor,
            cached_exp,
            cached_result,
          ),
        ]
      | FactPicksOpen(form) => [view_picks(~globals, ~signal, model, form)]
      };
  };

  /* The picker's buttons, and the box one of them opened, over the
   * row's selection. */
  let view_overlay =
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected: option(Selection.t),
        model: Model.t,
      ) => {
    let editor =
      model.editor |> Calc.get_saved_exc(~print="missing step editor");
    let info_map = CodeEditable.Model.get_statics(editor).info_map;
    let proof_button = ((label: string, callback: Ui_effect.t(unit))) =>
      Node.div(
        ~attrs=[
          Attr.classes(["proof-button"]),
          Attr.on_pointerdown(_ => Virtual_dom.Vdom.Effect.Stop_propagation),
          Attr.on_click(_ =>
            Ui_effect.Many([
              callback,
              Virtual_dom.Vdom.Effect.Stop_propagation,
            ])
          ),
        ],
        [Node.text(label)],
      );
    switch (
      get_segment_bounds(
        ~measured=CachedSyntax.measured(editor.editor.syntax),
        editor.editor.state.zipper.selection.content,
      )
    ) {
    | None => []
    | Some((left, right, top, bottom)) => [
        Node.div(
          ~attrs=[
            Attr.classes(["missing-step-overlay-align"]),
            DecUtil.position(
              ~width=right - left,
              ~height=bottom - top,
              ~font_metrics=globals.font_metrics,
              Point.{
                col: left,
                row: top,
              },
            ),
          ],
          [
            Node.div(
              ~attrs=[
                Attr.class_("proof-context-box"),
                Attr.on_pointerdown(_ =>
                  Virtual_dom.Vdom.Effect.Stop_propagation
                ),
              ],
              [
                Node.div(
                  ~attrs=[Attr.classes(["proof-selection-buttons"])],
                  List.map(
                    proof_button,
                    buttons(~globals, ~signal, ~inject, ~info_map, model),
                  ),
                ),
              ]
              @ Box.view(
                  ~globals,
                  ~signal,
                  ~inject,
                  ~selected,
                  ~info_map,
                  model,
                ),
            ),
          ],
        ),
      ]
    };
  };

  let view_justification =
      (
        ~globals: Globals.t,
        ~hide_stepper: Ui_effect.t(unit),
        ~is_toplevel: bool,
        _model: Model.t,
      ) => {
    let button_hide_stepper =
      Widgets.toggle(~tooltip="Show Stepper", "s", true, _ => hide_stepper);
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
    Node.div(
      ~attrs=[Attr.classes(["stepper-controls"])],
      is_toplevel
        ? [eval_settings, toggle_show_history, button_hide_stepper] : [],
    );
  };
};
