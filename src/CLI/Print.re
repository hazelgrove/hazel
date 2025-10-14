open Haz3lcore;

let exp_to_segment_settings: ExpToSegment.Settings.t = {
  inline: false,
  fold_case_clauses: false,
  fold_fn_bodies: false,
  hide_fixpoints: false,
  show_filters: true,
  show_unknown_as_hole: true,
  multiline_lists: false,
};

let segmentize =
  ExpToSegment.exp_to_segment(~settings=exp_to_segment_settings, _);

let print = exp => Printer.of_segment(~holes="?", segmentize(exp));
