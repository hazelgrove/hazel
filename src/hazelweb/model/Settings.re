open Sexplib.Std;
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
    show_unevaluated_expansion: bool,
  };

  let init = {
    evaluate: true,
    show_case_clauses: false,
    show_fn_bodies: false,
    show_casts: false,
    show_unevaluated_expansion: false,
  };

  [@deriving sexp]
  type update =
    | Toggle_evaluate
    | Toggle_show_case_clauses
    | Toggle_show_fn_bodies
    | Toggle_show_casts
    | Toggle_show_unevaluated_expansion;

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
 * Flags for the display of the cursor inspector
 */
module CursorInspector = {
  type t = {
    visible: bool,
    show_expanded: bool,
    novice_mode: bool,
    strategy_guide: bool,
    strategy_guide_lit: bool,
    strategy_guide_var: bool,
    strategy_guide_fun: bool,
    strategy_guide_branch: bool,
    strategy_guide_new_var: bool,
    strategy_guide_other: bool,
  };

  let init = {
    visible: false,
    show_expanded: false,
    novice_mode: true,
    strategy_guide: false,
    strategy_guide_lit: false,
    strategy_guide_var: false,
    strategy_guide_fun: false,
    strategy_guide_branch: false,
    strategy_guide_new_var: false,
    strategy_guide_other: false,
  };

  [@deriving sexp]
  type update =
    | Toggle_visible
    | Set_visible(bool)
    | Toggle_show_expanded
    | Toggle_novice_mode
    | Set_guide(bool)
    | Toggle_strategy_guide
    | Toggle_strategy_guide_lit
    | Toggle_strategy_guide_var
    | Toggle_strategy_guide_fun
    | Toggle_strategy_guide_branch
    | Toggle_strategy_guide_new_var
    | Toggle_strategy_guide_other;

  let apply_update = (u: update, settings: t) =>
    switch (u) {
    | Toggle_visible => {...settings, visible: !settings.visible}
    | Set_visible(b) => {...settings, visible: b}
    | Toggle_show_expanded => {
        ...settings,
        show_expanded: !settings.show_expanded,
      }
    | Toggle_novice_mode => {...settings, novice_mode: !settings.novice_mode}
    | Set_guide(b) => {...settings, strategy_guide: b}
    | Toggle_strategy_guide => {
        ...settings,
        strategy_guide: !settings.strategy_guide,
      }
    | Toggle_strategy_guide_lit => {
        ...settings,
        strategy_guide_lit: !settings.strategy_guide_lit,
      }
    | Toggle_strategy_guide_var => {
        ...settings,
        strategy_guide_var: !settings.strategy_guide_var,
      }
    | Toggle_strategy_guide_fun => {
        ...settings,
        strategy_guide_fun: !settings.strategy_guide_fun,
      }
    | Toggle_strategy_guide_branch => {
        ...settings,
        strategy_guide_branch: !settings.strategy_guide_branch,
      }
    | Toggle_strategy_guide_new_var => {
        ...settings,
        strategy_guide_new_var: !settings.strategy_guide_new_var,
      }
    | Toggle_strategy_guide_other => {
        ...settings,
        strategy_guide_other: !settings.strategy_guide_other,
      }
    };
};

type t = {
  evaluation: Evaluation.t,
  performance: Performance.t,
  cursor_inspector: CursorInspector.t,
  memoize_doc: bool,
};

let init: t = {
  evaluation: Evaluation.init,
  performance: Performance.init,
  cursor_inspector: CursorInspector.init,
  memoize_doc: true,
};

[@deriving sexp]
type update =
  | Toggle_memoize_doc
  | Evaluation(Evaluation.update)
  | Performance(Performance.update)
  | CursorInspector(CursorInspector.update);

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
  | CursorInspector(u) => {
      ...settings,
      cursor_inspector:
        CursorInspector.apply_update(u, settings.cursor_inspector),
    }
  };
