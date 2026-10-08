open Util;
open Language;

/* The stepper a probe drawer shows for one sample: the probed expression,
   closed over the sample's values, stepped from there. */

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  span: Sample.span_ref,
  /* None until the sample is there with its function values, which a
     sample keeps only while its probe steps */
  exp: option(Exp.t),
  stepper: StepperView.Model.t,
  /* the stepper took an action since it was last calculated */
  dirty: bool,
  /* the step the pointer last went to: its editor draws its selection,
     so a redex's first click shows before the second steps */
  [@default None]
  focus: option(StepperView.Focus.t),
};

let init = (span: Sample.span_ref): t => {
  span,
  exp: None,
  stepper: StepperView.Model.init,
  dirty: false,
  focus: None,
};

/* a step taken or undone: the drawer scrolls the newest step into view
   after it renders */
let reveal = ref(false);

/* a selection or a panel toggle is not a step */
let rec steps = (a: StepperView.Update.t): bool =>
  switch (a) {
  | NextStep(a) => steps(a)
  | EditorAction(_)
  | StepKindAction(_) => false
  | _ => true
  };

let closed_exp =
    (
      ~info_map: Statics.Map.t,
      ~dynamics: Dynamics.Map.t,
      span: Sample.span_ref,
    )
    : option(Exp.t) => {
  open OptUtil.Syntax;
  let* info = Statics.Map.lookup_exp(span.probe_id, info_map);
  let* samples = Dynamics.Map.lookup(span.probe_id, dynamics);
  let* sample = List.find_opt(Sample.ref_matches(span), samples);
  let+ bindings =
    sample.env
    |> List.map((en: Sample.Env.entry) =>
         switch (en.value) {
         | Val(d) => Some((en.binding.name, d))
         | Opaque => None
         }
       )
    |> OptUtil.sequence;
  let env =
    List.fold_left(
      (env, b) => Environment.extend(env, b),
      Environment.empty,
      bindings,
    );
  info.elab_term |> Substitution.in_exp(env);
};

let ctx =
  SemanticCtx.of_ctx_and_env(Builtins.ctx_init(None), Builtins.closure_env);

/* the drawer's stepper, recalculated only when its sample, its settings
   or its own state changed; a new one redraws the probe views */
let calculate =
    (
      ~settings: Calc.t(CoreSettings.t),
      ~info_map: Statics.Map.t,
      ~dynamics: Dynamics.Map.t,
      ~stepping: option(Sample.span_ref),
      prev: option(t),
    )
    : option(t) =>
  switch (stepping) {
  | None => None
  | Some(span) =>
    let ps =
      switch (prev) {
      | Some(ps) when ps.span == span => ps
      | _ => init(span)
      };
    /* a run in progress may lack the sample: keep the last one */
    let exp =
      switch (closed_exp(~info_map, ~dynamics, span)) {
      | Some(e) => Some(e)
      | None => ps.exp
      };
    let same_exp =
      switch (exp, ps.exp) {
      | (Some(a), Some(b)) => a === b || Exp.fast_equal(a, b)
      | (None, None) => true
      | _ => false
      };
    let fresh =
      switch (prev) {
      | Some(p) => p !== ps
      | None => true
      };
    if (!fresh && same_exp && !ps.dirty && !Calc.is_new(settings)) {
      prev;
    } else {
      Haz3lcore.ProbeProj.Settings.version :=
        Haz3lcore.ProbeProj.Settings.version^ + 1;
      switch (exp, ps.exp) {
      | (None, _) =>
        Some({
          ...ps,
          dirty: false,
        })
      | (Some(e), old) =>
        let e =
          switch (old) {
          | Some(o) when same_exp => o
          | _ => e
          };
        Some({
          ...ps,
          span,
          exp: Some(e),
          stepper:
            StepperView.Update.calculate(
              ~settings,
              ~ctx=OldValue(ctx),
              same_exp && !fresh ? OldValue(e) : NewValue(e),
              ps.stepper,
            ),
          dirty: false,
        });
      };
    };
  };

/* view state: not an edit to the program, not an undo step */
let update = (~settings, a: StepperView.Update.t, ps: t): Updated.t(t) => {
  open Updated;
  if (steps(a)) {
    reveal := true;
  };
  let updated = {
    let* stepper = StepperView.Update.update(~settings, a, ps.stepper);
    {
      ...ps,
      stepper,
      dirty: true,
    };
  };
  /* the drawer's cached view shows the old stepper until this moves */
  Haz3lcore.ProbeProj.Settings.version :=
    Haz3lcore.ProbeProj.Settings.version^ + 1;
  {
    ...updated,
    is_edit: false,
    historic: false,
    recalculate: true,
    /* the main editor's caret isn't what moved */
    scroll_active: false,
  };
};

/* rows a stepper takes: one code block per step shown (just the current
   one unless history is on) */
let stepper_rows = (~settings: CoreSettings.t, s: StepperView.Model.t): int => {
  let rec go = (m: StepperBase.step_model) => {
    let shown =
      StepperBase.StepKind.is_missing_step(m.step_kind)
      || settings.evaluation.stepper_history
      && (
        m.hidden != Calc.Calculated(true)
        || settings.evaluation.show_hidden_steps
      );
    let here =
      switch (m.editor) {
      | _ when !shown => 0
      | Calc.Calculated(ed) =>
        Haz3lcore.Measured.num_rows(ed.editor.syntax.measured)
      | Calc.Pending => 1
      };
    here
    + (
      switch (m.next_step) {
      | Some(n) => go(n)
      | None => 0
      }
    );
  };
  max(1, go(s.root));
};

let rows = (~settings: CoreSettings.t, ps: t): int =>
  switch (ps.exp) {
  | None => 1
  | Some(_) => stepper_rows(~settings, ps.stepper)
  };

/* the result view's stepper, panel included: undo, settings, history,
   and its stepper toggle, which closes the drawer */
let view =
    (
      ~globals: Globals.t,
      ~inject: StepperView.Update.t => Ui_effect.t(unit),
      ~focus: option(StepperView.Focus.t) => Ui_effect.t(unit),
      ~close: Ui_effect.t(unit),
      ps: t,
    )
    : option(Virtual_dom.Vdom.Node.t) =>
  switch (ps.exp) {
  | None => None
  | Some(_) =>
    Some(
      WebUtil.div_c(
        "probe-stepper",
        StepperView.View.view(
          ~globals,
          ~signal=
            fun
            | HideStepper => close
            | MakeActive(f) => focus(Some(f)),
          ~inject,
          ~selected=ps.focus,
          ~is_toplevel=true,
          ps.stepper,
        ),
      ),
    )
  };
