/**
 * Flags for enabling/disabling live results
 * and configuring the result view
 */
module Evaluation = {
  type t = {
    evaluate: bool,
    show_case_clauses: bool,
    show_fn_bodies: bool,
    show_casts: bool,
    show_unevaluated_elaboration: bool,
  };

  let init = {
    evaluate: true,
    show_case_clauses: false,
    show_fn_bodies: true,
    show_casts: false,
    show_unevaluated_elaboration: false,
  };

  [@deriving sexp]
  type update =
    | Toggle_evaluate
    | Toggle_show_case_clauses
    | Toggle_show_fn_bodies
    | Toggle_show_casts
    | Toggle_show_unevaluated_elaboration;

  let apply_update = (u: update, settings: t) =>
    switch (u) {
    | Toggle_evaluate => {...settings, evaluate: !settings.evaluate}
    | Toggle_show_case_clauses => {
        ...settings,
        show_case_clauses: !settings.show_case_clauses,
      }
    | Toggle_show_fn_bodies => {
        ...settings,
        show_fn_bodies: !settings.show_fn_bodies,
      }
    | Toggle_show_casts => {...settings, show_casts: !settings.show_casts}
    | Toggle_show_unevaluated_elaboration => {
        ...settings,
        show_unevaluated_elaboration: !settings.show_unevaluated_elaboration,
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
    | Toggle_update_apply_action;

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
    };
};

/**
 * Flags for display of right panel
 */
module RightPanel = {
  type t = {
    panel_open: bool,
    syntactic_form: bool,
    code_summary: bool,
    code_explanation: bool,
    code_example: bool,
    cursor_inspector: bool,
    context_inspector: bool,
    undo_history_panel: bool,
    settings_panel: bool,
  };

  /* TODO: Hannah - make sure to return this back to default settings */
  let init = {
    panel_open: true,
    syntactic_form: true,
    code_summary: false,
    code_explanation: true,
    code_example: true,
    cursor_inspector: false,
    context_inspector: false,
    undo_history_panel: false,
    settings_panel: false,
  };

  [@deriving sexp]
  type update =
    | Toggle_open
    | Toggle_syntactic_form
    | Toggle_code_summary
    | Toggle_code_explanation
    | Toggle_code_example
    | Toggle_cursor_inspector
    | Toggle_context_inspector
    | Toggle_undo_history_panel
    | Toggle_settings_panel;

  let apply_update = (u: update, settings: t) =>
    switch (u) {
    | Toggle_open => {...settings, panel_open: !settings.panel_open}
    | Toggle_syntactic_form => {
        ...settings,
        syntactic_form: !settings.syntactic_form,
      }
    | Toggle_code_summary => {
        ...settings,
        code_summary: !settings.code_summary,
      }
    | Toggle_code_explanation => {
        ...settings,
        code_explanation: !settings.code_explanation,
      }
    | Toggle_code_example => {
        ...settings,
        code_example: !settings.code_example,
      }
    | Toggle_cursor_inspector => {
        ...settings,
        cursor_inspector: !settings.cursor_inspector,
      }
    | Toggle_context_inspector => {
        ...settings,
        context_inspector: !settings.context_inspector,
      }
    | Toggle_undo_history_panel => {
        ...settings,
        undo_history_panel: !settings.undo_history_panel,
      }
    | Toggle_settings_panel => {
        ...settings,
        settings_panel: !settings.settings_panel,
      }
    };
};

type t = {
  evaluation: Evaluation.t,
  performance: Performance.t,
  right_panel: RightPanel.t,
  memoize_doc: bool,
};

let init: t = {
  evaluation: Evaluation.init,
  performance: Performance.init,
  right_panel: RightPanel.init,
  memoize_doc: true,
};

[@deriving sexp]
type update =
  | Toggle_memoize_doc
  | Evaluation(Evaluation.update)
  | Performance(Performance.update)
  | RightPanel(RightPanel.update);

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
  | RightPanel(u) => {
      ...settings,
      right_panel: RightPanel.apply_update(u, settings.right_panel),
    }
  };
