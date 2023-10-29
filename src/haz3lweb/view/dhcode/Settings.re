open Sexplib.Std;

/**
 * Flags for enabling/disabling live results
 * and configuring the result view
 */
module Evaluation = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    evaluate: bool,
    stepping: bool,
    postprocess: bool,
    show_record: bool,
    show_case_clauses: bool,
    show_fn_bodies: bool,
    show_casts: bool,
    show_unevaluated_elaboration: bool,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  let init = {
    evaluate: true,
    stepping: false,
    postprocess: true,
    show_record: false,
    show_case_clauses: true,
    show_fn_bodies: true,
    show_casts: true,
    show_unevaluated_elaboration: false,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type update =
    | Toggle_evaluate
    | Toggle_stepping
    | Toggle_show_record
    | Toggle_show_case_clauses
    | Toggle_show_fn_bodies
    | Toggle_show_casts
    | Toggle_show_unevaluated_elaboration;

  let apply_update = (u: update, settings: t) =>
    switch (u) {
    | Toggle_evaluate => {...settings, evaluate: !settings.evaluate}
    | Toggle_stepping => {...settings, stepping: !settings.stepping}
    | Toggle_show_record => {...settings, show_record: !settings.show_record}
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
