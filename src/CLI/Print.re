open Haz3lcore;

let exp_to_segment_settings: ExpToSegment.Settings.t = {
  secondary: AutoFormat,
  parenthesization: Defensive,
  label_format: QuoteWhenNecessary,
  inline: false,
  fold_case_clauses: false,
  project_tables: false,
  fold_fn_bodies: `NoFold,
  hide_fixpoints: false,
  show_filters: true,
  show_unknown_as_hole: true,
};

let segmentize =
  ExpToSegment.exp_to_segment(~settings=exp_to_segment_settings, _);

let print = exp => Printer.of_segment(~holes="?", segmentize(exp));
