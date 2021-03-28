/**
 * Flags for enabling/disabling live results
 * and configuring the result view
 */
module Evaluator = {
  type t =
    | Evaluator
    | StepEvaluator;
};

module Evaluation = {
  type t = {
    evaluate: bool,
    evaluator_type: Evaluator.t,
    step_evaluator_option: EvaluatorStep.evaluator_option,
    show_evaluate_steps: bool,
    show_case_clauses: bool,
    show_fn_bodies: bool,
    show_casts: bool,
    show_unevaluated_expansion: bool,
  };

  let init = {
    evaluate: true,
    evaluator_type: Evaluator,
    step_evaluator_option: EvaluatorStep.default_option,
    show_evaluate_steps: false,
    show_case_clauses: false,
    show_fn_bodies: false,
    show_casts: false,
    show_unevaluated_expansion: false,
  };

  [@deriving sexp]
  type update =
    | Toggle_evaluate
    | Toggle_use_step_evaluator
    | Toggle_pause_at_empty_hole
    | Toggle_show_evaluate_steps
    | Toggle_show_case_clauses
    | Toggle_show_fn_bodies
    | Toggle_show_casts
    | Toggle_show_unevaluated_expansion;

  let apply_update = (u: update, settings: t) =>
    switch (u) {
    | Toggle_evaluate => {...settings, evaluate: !settings.evaluate}
    | Toggle_use_step_evaluator => {
        ...settings,
        evaluator_type:
          switch (settings.evaluator_type) {
          | Evaluator => StepEvaluator
          | StepEvaluator => Evaluator
          },
      }
    | Toggle_pause_at_empty_hole => {
        ...settings,
        step_evaluator_option:
          // ...settings.step_evaluator_option,
          {
            pause_at_empty_hole:
              !settings.step_evaluator_option.pause_at_empty_hole,
          },
      }
    | Toggle_show_evaluate_steps => {
        ...settings,
        show_evaluate_steps: !settings.show_evaluate_steps,
      }
    | Toggle_show_case_clauses => {
        ...settings,
        show_case_clauses: !settings.show_case_clauses,
      }
    | Toggle_show_fn_bodies => {
        ...settings,
        show_fn_bodies: !settings.show_fn_bodies,
      }
    | Toggle_show_casts => {...settings, show_casts: !settings.show_casts}
    | Toggle_show_unevaluated_expansion => {
        ...settings,
        show_unevaluated_expansion: !settings.show_unevaluated_expansion,
      }
    };
};

/**
 * Flags for benchmarking various portions of
 * the render cycle
 */
module Performance = {
  type t = {
    measure: bool,
    model_perform_edit_action: bool,
    program_get_doc: bool,
    layoutOfDoc_layout_of_doc: bool,
    uhcode_view: bool,
    cell_view: bool,
    page_view: bool,
    hazel_create: bool,
    update_apply_action: bool,
    program_evaluate: bool,
  };
  let init = {
    measure: false,
    model_perform_edit_action: true,
    program_get_doc: true,
    layoutOfDoc_layout_of_doc: true,
    uhcode_view: true,
    cell_view: true,
    page_view: true,
    hazel_create: true,
    update_apply_action: true,
    program_evaluate: false,
  };

  [@deriving sexp]
  type update =
    | Toggle_measure
    | Toggle_model_perform_edit_action
    | Toggle_program_get_doc
    | Toggle_layoutOfDoc_layout_of_doc
    | Toggle_uhcode_view
    | Toggle_cell_view
    | Toggle_page_view
    | Toggle_hazel_create
    | Toggle_update_apply_action
    | Toggle_program_evaluate;

  let apply_update = (u: update, settings: t) =>
    switch (u) {
    | Toggle_measure => {...settings, measure: !settings.measure}
    | Toggle_model_perform_edit_action => {
        ...settings,
        model_perform_edit_action: !settings.model_perform_edit_action,
      }
    | Toggle_program_get_doc => {
        ...settings,
        program_get_doc: !settings.program_get_doc,
      }
    | Toggle_layoutOfDoc_layout_of_doc => {
        ...settings,
        layoutOfDoc_layout_of_doc: !settings.layoutOfDoc_layout_of_doc,
      }
    | Toggle_uhcode_view => {...settings, uhcode_view: !settings.uhcode_view}
    | Toggle_cell_view => {...settings, cell_view: !settings.cell_view}
    | Toggle_page_view => {...settings, page_view: !settings.page_view}
    | Toggle_hazel_create => {
        ...settings,
        hazel_create: !settings.hazel_create,
      }
    | Toggle_update_apply_action => {
        ...settings,
        update_apply_action: !settings.update_apply_action,
      }
    | Toggle_program_evaluate => {
        ...settings,
        program_evaluate: !settings.program_evaluate,
      }
    };
};

type t = {
  evaluation: Evaluation.t,
  performance: Performance.t,
  memoize_doc: bool,
};

let init: t = {
  evaluation: Evaluation.init,
  performance: Performance.init,
  memoize_doc: true,
};

[@deriving sexp]
type update =
  | Toggle_memoize_doc
  | Evaluation(Evaluation.update)
  | Performance(Performance.update);

let apply_update = (u: update, settings: t) =>
  switch (u) {
  | Toggle_memoize_doc => {...settings, memoize_doc: !settings.memoize_doc}
  | Evaluation(u) => {
      ...settings,
      evaluation: Evaluation.apply_update(u, settings.evaluation),
    }
  | Performance(u) => {
      ...settings,
      performance: Performance.apply_update(u, settings.performance),
    }
  };
