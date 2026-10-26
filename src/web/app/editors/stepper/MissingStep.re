open Language;
open Util;
open WebUtil;
open Calc.Syntax;
open Haz3lcore;

module Model = {
  /* The proof forms this row can insert. The wrapping three share the
   * whole flow: take an expression, write `<kw> <exp> => ` around the
   * current hole. `Contradiction` shares the expression-picking UI but is
   * TERMINAL, so it writes `contradiction <exp> end` instead
   * (docs/prover-obligations.md, Phase 4e). */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type proof_form =
    | Assume
    | Revert
    | Generalize
    | Contradiction;

  let proof_form_keyword = (f: proof_form): string =>
    switch (f) {
    | Assume => "assume"
    | Revert => "revert"
    | Generalize => "generalize"
    | Contradiction => "contradiction"
    };

  /* What follows the expression in the written form. */
  let proof_form_suffix = (f: proof_form): string =>
    switch (f) {
    | Assume
    | Revert
    | Generalize => "=> ?"
    | Contradiction => "end"
    };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type open_box =
    | AxiomsOpen(AxiomsBox.Model.t)
    | RewritesOpen({
        editor: CodeEditable.Model.t,
        cached_exp: Calc.saved(Exp.t),
        cached_result: option(bool),
      })
    /* A wrapping form whose argument cannot be guessed but CAN be
       searched: the box lists candidates and picking one inserts the
       step. Purely a pick list — it holds no editor, because the
       inserted step's argument is edited in place (docs/prover-
       obligations.md §3.4: menus only ever PICK, never EDIT). */
    | ProofFormPicksOpen({form: proof_form})
    | NoneOpen;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type assumptions = list(AssumptionBox.Model.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    /* The proof sub-term this row stands in for: the hole it would
     * replace, or None when the row was synthesized past the end of the
     * chain and can only extend the preceding leaf. */
    proof: Calc.saved(option(Proof.t)),
    /* The row's own editor. A missing step is a step in the chain in its
     * own right, so it owns the editor whose selection drives the
     * overlay; it is rebuilt only when the expression changes so that
     * selection survives recalculation. */
    editor: Calc.saved(CodeSelectable.Model.t),
    next_steps: Calc.saved(EvaluatorStep.status),
    refls: Calc.saved(list(Exp.t)),
    selected_id: Calc.saved(option(Id.t)),
    selected_exp: Calc.saved(option(Exp.t)),
    full_exp: Calc.saved(Exp.t),
    /* The checker's written goal. `full_exp` is the env-substituted
     * display (`<sum>([])`); eval overlay / `eval <exp> at <n>` quote
     * this form so inlined `FixF`s are not mistaken for auto-unwraps. */
    eval_exp: Calc.saved(Exp.t),
    /* D73: for each offered step (in `next_steps` order), its source node
     * in `eval_exp` — the subterm `eval ... at <n>` writes
     * (Update.offered_steps). */
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
  let get_selected_exp = (m: t): Exp.t =>
    m.selected_exp
    |> Calc.saved_to_option
    |> Option.join
    |> OptUtil.get(() => EmptyHole |> Exp.fresh);
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
    | ToggleProofFormPicks(Model.proof_form);

  let update = (~settings, action, model: Model.t): Updated.t(Model.t) => {
    switch (action, model.open_box) {
    | (ToggleAxioms, _) =>
      let open_box =
        switch (model.open_box) {
        | NoneOpen
        | RewritesOpen(_)
        | ProofFormPicksOpen(_) => Model.AxiomsOpen(AxiomsBox.Model.init)
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
        | ProofFormPicksOpen(_) =>
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
    /* Toggle: the same button closes the pick list it opened, and
       switching forms (or coming from another box) opens that form's
       list. The list carries no state of its own — the candidates are
       read off `model.assumptions` at view time. */
    | (ToggleProofFormPicks(form), _) =>
      let open_box =
        switch (model.open_box) {
        | ProofFormPicksOpen({form: open_form}) when open_form == form => Model.NoneOpen
        | NoneOpen
        | AxiomsOpen(_)
        | RewritesOpen(_)
        | ProofFormPicksOpen(_) => Model.ProofFormPicksOpen({form: form})
        };
      Model.{
        ...model,
        open_box,
      }
      |> Updated.return_quiet(~recalculate=true, ~logged=true);
    };
  };

  let can_undo = (action: t): bool => {
    switch (action) {
    | ToggleAxioms
    | ProposeRewrite
    | UpdateResult(_)
    | RewriteEditorAction(_)
    | AxiomBoxAction(_)
    | ToggleProofFormPicks(_) => false
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

  let map_calc = (f: 'a => 'b, x: Calc.t('a)): Calc.t('b) =>
    switch (x) {
    | Calc.OldValue(x) => Calc.OldValue(f(x))
    | Calc.NewValue(x) => Calc.NewValue(f(x))
    };

  /* D73 Reset-list-case: a goal like `all_lists(xs) == true` offers only
   * the VarLookup of `all_lists`, which the checker's eval step treats as
   * proof-silent (Evaluator.re, advance_hidden). So a silent step is
   * offered as the step the checker takes for the nearest WRITTEN subterm
   * around it: `eval all_lists(xs) at <n> end`. Each offered step keeps
   * that source node of the written goal; the rest of the goal is
   * untouched, and nothing is mapped across a change of shape. */
  let max_silent_steps = 64;
  let max_occurrence_idx = 64;

  let is_proof_silent = (s: EvaluatorStep.step): bool =>
    EvaluatorStep.get_step_kind(s) == Transition.VarLookup;

  /* The checker's eval step on `e` (Evaluator.re, `step`): hidden steps
   * and a leading silent step, then the first available step. Bounded;
   * None when the bound runs out or nothing visible remains. */
  let checker_step = (~env, e: Exp.t): option(EvaluatorStep.step) => {
    let rec go = (fuel, e) =>
      fuel <= 0
        ? None
        : (
          switch (EvaluatorStep.get_status(~settings=CoreSettings.on, e, env)) {
          | AutoStep(s) => take(fuel, s)
          | AvailableSteps([s, ..._]) when is_proof_silent(s) => take(fuel, s)
          | AvailableSteps([s, ..._]) => Some(s)
          | AvailableSteps([]) => None
          }
        )
    and take = (fuel, s) =>
      Option.bind(EvaluatorStep.take_step(s), go(fuel - 1));
    go(max_silent_steps, e);
  };

  exception Ancestors(list(Exp.t));

  /* The node with `id` and its enclosing expressions, nearest first. */
  let self_and_ancestors = (id: Id.t, e: Exp.t): list(Exp.t) => {
    let stack = ref([]);
    switch (
      Exp.map_term(
        ~f_exp=
          (cont, exp) => {
            stack := [exp, ...stack^];
            if (Exp.rep_id(exp) == id) {
              raise(Ancestors(stack^));
            };
            let r = cont(exp);
            stack := List.tl(stack^);
            r;
          },
        e,
      )
    ) {
    | exception (Ancestors(path)) => path
    | _ => []
    };
  };

  /* The `at <n>` under which the checker finds exactly the written node
   * `at_exp`. ProofHacks.nth_exp_env is `nth_exp` on both sides
   * env-substituted, then the match's id looked up in the written goal;
   * `substituted` is the written goal substituted once by the caller. */
  let checker_idx =
      (~env, ~substituted: Exp.t, at_exp: Exp.t): option(int) => {
    let target = Substitution.in_exp(env, at_exp);
    let rec go = n =>
      n > max_occurrence_idx
        ? None
        : (
          switch (ProofHacks.nth_exp(target, n, substituted)) {
          | Some(found) when Exp.rep_id(found) == Exp.rep_id(at_exp) =>
            Some(n)
          | Some(_) => go(n + 1)
          | None => None
          }
        );
    go(0);
  };

  /* Offered steps for the written goal, each with its written source node.
   * A step whose source the checker could not locate is not offered. */
  let offered_steps =
      (~settings: CoreSettings.t, ~env, written: Exp.t)
      : list((EvaluatorStep.step, Id.t)) => {
    let candidates =
      switch (EvaluatorStep.get_status(~settings, written, env)) {
      | AutoStep(s) => [s]
      | AvailableSteps(steps) => steps
      };
    let substituted = lazy(Substitution.in_exp(env, written));
    let source = s =>
      self_and_ancestors(EvaluatorStep.get_step_id(s), written)
      |> List.find_map(at_exp =>
           switch (checker_step(~env, at_exp)) {
           | Some(step)
               when
                 checker_idx(~env, ~substituted=Lazy.force(substituted), at_exp)
                 != None =>
             Some((step, Exp.rep_id(at_exp)))
           | _ => None
           }
         );
    List.fold_left(
      (acc, s) =>
        switch (source(s)) {
        | Some((_, id) as offered)
            when !List.exists(((_, id')) => id' == id, acc) => [
            offered,
            ...acc,
          ]
        | _ => acc
        },
      [],
      candidates,
    )
    |> List.rev;
  };

  /* `sources` is aligned with `steps`; anything else has no source. */
  let source_in = (steps, sources: list(Id.t), step): option(Id.t) =>
    List.length(steps) == List.length(sources)
      ? List.combine(steps, sources) |> List.assq_opt(step) : None;

  let step_source = (model: Model.t, step: EvaluatorStep.step): option(Id.t) =>
    switch (model.next_steps, model.step_sources) {
    | (Calculated(AvailableSteps(steps)), Calculated(sources)) =>
      source_in(steps, sources, step)
    | _ => None
    };

  /* What taking the step writes: its written source and the index the
   * checker finds it under (`eval <at_exp> at <idx> end`). */
  let written_target =
      (model: Model.t, step: EvaluatorStep.step): option((Exp.t, int)) => {
    open OptUtil.Syntax;
    let* id = step_source(model, step);
    let* written = model.eval_exp |> Calc.get_saved_opt;
    let* env = model.cached_env |> Calc.get_saved_opt;
    let* at_exp = ProofHacks.find_exp_id(id, written);
    let* at_idx =
      checker_idx(
        ~env,
        ~substituted=Substitution.in_exp(env, written),
        at_exp,
      );
    Some((at_exp, at_idx));
  };

  /* Map a step's written source onto the displayed term so underlines
   * land on `<sum>` rather than a vanished `Var`. */
  let display_id_of =
      (~written: Exp.t, ~display: Exp.t, source: Id.t): Id.t =>
    ProofHacks.map_rep_id(~from=written, ~onto=display, source)
    |> Option.value(~default=source);

  let display_step_id = (model: Model.t, step: EvaluatorStep.step): Id.t =>
    switch (
      step_source(model, step),
      model.eval_exp |> Calc.get_saved_opt,
      model.full_exp |> Calc.get_saved_opt,
    ) {
    | (Some(source), Some(written), Some(display)) =>
      display_id_of(~written, ~display, source)
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
      editor |> map_calc(e => CodeEditable.Model.get_statics(e).info_map);
    /* Eval steps are computed on the WRITTEN goal + env. The displayed
     * term already has `sum` inlined as `FixF`, and those hidden
     * FixUnwraps would otherwise collapse `get_status` to AutoStep and
     * wipe the overlay. */
    /* D73: each offered step with its written source node. */
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
    let step_sources = offered |> map_calc(fst);
    let new_next_steps = offered |> map_calc(snd);
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
        let* exp' = ProofHacks.find_exp_id(id, exp);
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
        and.calc ctx = ctx
        and.calc new_next_steps = new_next_steps
        and.calc eval_exp = eval_exp
        and.calc step_sources = step_sources
        and.calc info_map = info_map;
        let next_steps =
          new_next_steps
          |> (
            fun
            | EvaluatorStep.AutoStep(_) => []
            | EvaluatorStep.AvailableSteps(steps) => steps
          );
        let display_id = (s: EvaluatorStep.step) =>
          source_in(next_steps, step_sources, s)
          |> Option.map(display_id_of(~written=eval_exp, ~display=exp))
          |> Option.value(~default=EvaluatorStep.get_step_id(s));
        ProofHacks.find_refls(~info_map, ~env=SemanticCtx.get_env(ctx), exp)
        |> List.filter(e =>
             !List.exists(s => e |> Exp.rep_id == display_id(s), next_steps)
           );
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
      | ProofFormPicksOpen(_) as picks => picks
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
    /* `funext <binder> => ?` around this row's hole: functional
       extensionality. No-search form (the argument's NAME is the user's
       free choice), so one click inserts the shape and the caret lands
       in the binder. The payload is the binder as it should first
       appear: the goal's left-hand function's own parameter pattern
       where there is one to copy (`ProofCheck.funext_binder`), and
       `None` — a hole — where there is not. */
    | AddFunext(option(Pat.t))
    | HideStepper
    | AddAxiomStep(string, int, Exp.t, Direction.t, string)
    | AddAlgebriteStep(int, Exp.t, Exp.t)
    /* The wrapping forms: `assume`/`revert`/`generalize <exp> => ?`
       written around this row's hole (see StepperBase.assume_term &
       friends). The payload is the argument as it should first appear: an
       EmptyHole for the no-search forms (the user types into the inserted
       step's own arg editor), or a picked expression for the prefills.
       Either way the insertion is immediate and focus lands in the new
       step's arg slot — see StepperBase's `emit_wrapping_form`. */
    | AddAssume(Exp.t)
    | AddRevert(Exp.t)
    | AddGeneralize(Exp.t)
    /* `have <exp> proof ? => ?` around this row's hole. Same no-search
       convention as Assume/Generalize: one click inserts with a HOLE
       proposition and focus lands in the new step's arg slot, and the
       subproof hole becomes its own picker row inside the nested box. */
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

  let view_overlay =
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected: option(Selection.t),
        model: Model.t,
      ) =>
    {
      let editor =
        model.editor |> Calc.get_saved_exc(~print="missing step editor");
      let info_map = CodeEditable.Model.get_statics(editor).info_map;
      let+ (left, right, top, bottom) =
        get_segment_bounds(
          ~measured=CachedSyntax.measured(editor.editor.syntax),
          editor.editor.state.zipper.selection.content,
        );

      let proof_button = (~callback: Ui_effect.t(unit), label: string) => {
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
      };

      let show_step_button =
        switch (
          model.selected_exp |> Calc.get_saved_exc(~print="Selected Exp")
        ) {
        | Some(selected_exp) =>
          List.find_index(
            x => x == (selected_exp |> Exp.rep_id),
            model.next_steps
            |> Calc.get_saved_exc(~print="next_steps")
            |> (
              fun
              | AutoStep(_) => []
              | AvailableSteps(steps) => steps
            )
            |> List.map(step => Update.display_step_id(model, step)),
          )
        | None => None
        };

      let show_refl_button =
        switch (
          model.selected_exp |> Calc.get_saved_exc(~print="Selected Exp")
        ) {
        | Some(selected_exp) =>
          List.find_index(
            x => x == (selected_exp |> Exp.rep_id),
            model.refls
            |> Calc.get_saved_exc(~print="refls")
            |> List.map(refl => refl |> Exp.rep_id),
          )
        | None => None
        };

      /* Funext is offered exactly when the goal is `f == g` between two
         things of ARROW type — the same detector `ProofCheck`'s Funext arm
         uses, so the button never offers a step that would immediately
         mark. The arrow requirement is what keeps it off `==` between two
         LISTS of functions, which is an induction, not an extensionality. */
      let funext_goal_env = {
        let full_exp =
          model.full_exp |> Calc.get_saved_exc(~print="full_exp not cached");
        let env = Calc.get_saved(Environment.empty, model.cached_env);
        (full_exp, env);
      };
      let show_funext_button = {
        let (full_exp, env) = funext_goal_env;
        ProofCheck.funext_goal(~info_map, ~env, full_exp) != None;
      };
      /* The binder the inserted step should open with — the LHS
         function's parameter pattern, when the goal has one to give. */
      let funext_binder = {
        let (full_exp, env) = funext_goal_env;
        ProofCheck.funext_binder(~info_map, ~env, full_exp);
      };

      let show_function_body_button = {
        Calc.get_saved_exc(model.selected_exp)
        == Some(Calc.get_saved_exc(model.full_exp))
        && Exp.is_fun(Calc.get_saved_exc(model.full_exp));
      };

      /* The wrapping proof forms (`assume`/`revert`/`generalize <exp> =>`).
         Convention (docs/prover-obligations.md §3.4): a menu only ever
         PICKS. Where there is nothing to search — Assume and Generalize
         take an arbitrary expression — one click inserts the step with a
         HOLE argument and focus lands in the new step's own inline arg
         editor, so the user just types. Where the argument must be
         searched (Revert matches an in-scope fact by `Exp.fast_equal`),
         the button opens a pick list and picking inserts directly. Both
         paths force the record open, or the row just written would not be
         on screen. */
      let insert_form = (event: event) =>
        Ui_effect.Many([
          globals.inject_global(Set(Evaluation(ForceShowRecord))),
          signal(event),
        ]);
      let hole = () => EmptyHole |> Exp.fresh;
      let wrapping_form_buttons = {
        let full_exp =
          model.full_exp |> Calc.get_saved_exc(~print="full_exp not cached");
        [proof_button(~callback=insert_form(AddAssume(hole())), "Assume")]
        @ (
          /* Implication intro (docs/prover-obligations.md §2.1): with goal
             `A ==> B`, assuming exactly A strips the antecedent and incurs
             NO obligation. That is a pick, not an edit, so it inserts the
             step with A already in place. */
          switch (full_exp |> Exp.term_of) {
          | BinOp(Bool(Implies), a, _) => [
              proof_button(
                ~callback=insert_form(AddAssume(a)),
                "Assume antecedent",
              ),
            ]
          | _ => []
          }
        )
        @ [
          proof_button(
            ~callback=inject(ToggleProofFormPicks(Revert)),
            "Revert ▼",
          ),
          proof_button(
            ~callback=insert_form(AddGeneralize(hole())),
            "Generalize",
          ),
          /* `have` is a no-search form like Assume — the proposition is
             arbitrary — so it inserts immediately with a hole argument
             (docs/prover-obligations.md §3.4). The (!) panel's "Prove
             here" writes the same form around an obligation's region;
             this is the same move started from the step-picker, with the
             subproof left open. */
          proof_button(~callback=insert_form(AddHave(hole())), "Have"),
        ];
      };

      // I want to make a bunch of buttons here:
      // Evaluate [TODO], Rewrite, Axioms, Cases,
      let buttons =
        Node.div(
          ~attrs=[Attr.classes(["proof-selection-buttons"])],
          (
            switch (show_step_button) {
            | None => []
            | Some(idx) => [
                proof_button(
                  ~callback=Ui_effect.Many([signal(TakeStep(idx))]),
                  "Step",
                ),
              ]
            }
          )
          @ (
            switch (show_refl_button) {
            | None => []
            | Some(idx) => [
                proof_button(
                  ~callback=
                    Ui_effect.Many([
                      globals.inject_global(
                        Set(Evaluation(ForceShowRecord)),
                      ),
                      signal(Refl(idx)),
                    ]),
                  "Reflexivity",
                ),
              ]
            }
          )
          @ (
            show_function_body_button
              ? [
                proof_button(
                  ~callback=
                    Ui_effect.Many([
                      globals.inject_global(
                        Set(Evaluation(ForceShowRecord)),
                      ),
                      signal(AddForall),
                    ]),
                  "Function Body",
                ),
              ]
              : []
          )
          @ (
            show_funext_button
              ? [
                proof_button(
                  ~callback=
                    Ui_effect.Many([
                      globals.inject_global(
                        Set(Evaluation(ForceShowRecord)),
                      ),
                      signal(AddFunext(funext_binder)),
                    ]),
                  "Funext",
                ),
              ]
              : []
          )
          @ [
            proof_button(~callback=inject(ProposeRewrite), "Algebra ▼"),
            proof_button(~callback=inject(ToggleAxioms), "Assumptions ▼"),
          ]
          @ wrapping_form_buttons
          @ [
            proof_button(
              ~callback=inject(ToggleProofFormPicks(Contradiction)),
              "Contradiction ▼",
            ),
            proof_button(
              ~callback=
                Ui_effect.Many([
                  globals.inject_global(Set(Evaluation(ForceShowRecord))),
                  signal(
                    AddInduction(
                      model.selected_exp
                      |> Calc.get_saved_exc(~print="Selected Exp"),
                    ),
                  ),
                ]),
              "Cases/Induction",
            ),
          ],
        );

      [
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
              [buttons]
              @ {
                switch (model.open_box) {
                | NoneOpen => []
                | AxiomsOpen(m) => [
                    div_c(
                      "axiom-box",
                      AxiomsBox.View.view(
                        ~globals,
                        ~info_map,
                        ~env=
                          model.cached_env
                          |> Calc.get_saved_exc(~print="env not cached"),
                        ~inject=
                          (a: AxiomsBox.Update.t) =>
                            inject(AxiomBoxAction(a)),
                        ~take_focus=
                          (s: AxiomsBox.Selection.t) =>
                            signal(MakeActive(AxiomBoxSelection(s))),
                        ~add_axiom_step=
                          (a, b, c, d, e) =>
                            signal(AddAxiomStep(a, b, c, d, e)),
                        ~full_exp=
                          model.full_exp
                          |> Calc.get_saved_exc(~print="full_exp not cached"),
                        ~selected_exp=
                          model.selected_exp
                          |> Calc.get_saved_exc(~print="Selected Exp")
                          |> Option.value(~default=EmptyHole |> Exp.fresh, _),
                        m,
                      ),
                    ),
                  ]
                | RewritesOpen({editor, cached_exp, cached_result}) =>
                  let unboxed_cached_exp =
                    Calc.get_saved_exc(
                      ~print="cached exp not calculated",
                      cached_exp,
                    );
                  let unboxed_selected_exp =
                    Option.value(
                      ~default=EmptyHole |> Exp.fresh,
                      Calc.get_saved_exc(
                        ~print="selected exp not calculated",
                        model.selected_exp,
                      ),
                    );
                  [
                    // one element list with a div
                    // with a list containing two elements
                    // an Editor for user to propose their rewrite
                    // a button to submit the rewrite
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
                          Exp(unboxed_selected_exp),
                        ),
                        Node.text("With: "),
                        div_c(
                          "inline-editor-wrapper",
                          [
                            CodeEditable.View.view(
                              ~globals,
                              ~signal=
                                fun
                                | MakeActive =>
                                  signal(MakeActive(RewriteEditor())),
                              ~edit_mode=
                                EditMode.Editable({
                                  inject: x =>
                                    inject(RewriteEditorAction(x)),
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
                      @ {
                        switch (cached_result) {
                        | Some(true) => [
                            Node.text("Valid"),
                            Widgets.button(
                              ~clss=["proof-button"],
                              Node.text("Replace"),
                              ~tooltip="replace",
                              _ =>
                              switch (
                                ProofHacks.exp_idx(
                                  unboxed_selected_exp,
                                  model.full_exp
                                  |> Calc.get_saved_exc(~print="full_exp"),
                                )
                              ) {
                              | Some(at_idx) =>
                                signal(
                                  AddAlgebriteStep(
                                    at_idx,
                                    unboxed_selected_exp,
                                    unboxed_cached_exp
                                    |> Substitution.in_exp(
                                         model.cached_env
                                         |> Calc.get_saved_exc(
                                              ~print="env not cached",
                                            ),
                                       ),
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
                                      unboxed_selected_exp
                                      |> Substitution.in_exp(
                                           model.cached_env
                                           |> Calc.get_saved_exc(
                                                ~print="env not cached",
                                              ),
                                         ),
                                      unboxed_cached_exp
                                      |> Substitution.in_exp(
                                           model.cached_env
                                           |> Calc.get_saved_exc(
                                                ~print="env not cached",
                                              ),
                                         ),
                                    ),
                                  ),
                                ),
                              ~tooltip="check",
                            ),
                          ]
                        };
                      },
                    ),
                  ];
                | ProofFormPicksOpen({form}) =>
                  let keyword = Model.proof_form_keyword(form);
                  /* A pick list, nothing more: each entry INSERTS the step
                     with that expression already in place (docs/prover-
                     obligations.md §3.4). `revert` matches an in-scope fact
                     by `Exp.fast_equal` (ProofCheck.lookup_fact), so
                     free-typing it is error-prone — hence the search. The
                     listing is the one the Assumptions box shows, minus the
                     builtin axioms (global rules, not facts of this scope)
                     and captured entries. */
                  let pick = (~label: string, exp: Exp.t) =>
                    Widgets.button(
                      ~clss=["proof-button"],
                      ~tooltip=
                        "Write `"
                        ++ keyword
                        ++ " "
                        ++ label
                        ++ " =>` into the proof",
                      Node.text(label),
                      _ =>
                      Ui_effect.Many([
                        globals.inject_global(
                          Set(Evaluation(ForceShowRecord)),
                        ),
                        switch (form) {
                        | Assume => signal(AddAssume(exp))
                        | Revert => signal(AddRevert(exp))
                        | Contradiction => signal(AddContradiction(exp))
                        | Generalize => signal(AddGeneralize(exp))
                        },
                      ])
                    );
                  let picks =
                    switch (form) {
                    /* `contradiction` resolves its argument through the
                       SAME channel-1 lookup as `revert`
                       (ProofCheck.cited_fact / lookup_fact), so it offers
                       the same in-scope-facts picker. Only the CITED
                       fact is picked here; a `with <var> = <exp>` rewrite
                       (Phase 4e) is typed into the proof afterwards, like
                       every other with-clause. */
                    | Contradiction
                    | Revert =>
                      model.assumptions
                      |> Calc.get_saved_opt
                      |> Option.join
                      |> Option.value(~default=[])
                      |> List.filter((ab: AssumptionBox.Model.t) =>
                           !ab.ctx_entry.is_captured
                           && !
                                List.exists(
                                  (e: ProofCtx.entry) =>
                                    e.name == ab.ctx_entry.name,
                                  Axioms.v,
                                )
                         )
                      |> List.map((ab: AssumptionBox.Model.t) =>
                           pick(~label=ab.ctx_entry.name, ab.ctx_entry.exp)
                         )
                    /* No-search forms don't open a list at all; their
                       buttons insert straight away. */
                    | Assume
                    | Generalize => []
                    };
                  [
                    div_c(
                      "proof-form-box",
                      picks == []
                        ? [
                          Node.text(
                            "No in-scope facts to " ++ keyword ++ ".",
                          ),
                        ]
                        : [Node.text(keyword ++ " ")] @ picks,
                    ),
                  ];
                };
              },
            ),
          ],
        ),
      ];
    }
    |> Option.value(~default=[]);

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
