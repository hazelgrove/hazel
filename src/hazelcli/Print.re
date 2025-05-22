open Haz3lcore;

let exp_to_segment_settings: bool => ExpToSegment.Settings.t =
  inline => {
    inline,
    fold_case_clauses: false,
    fold_fn_bodies: false,
    hide_fixpoints: false,
    fold_cast_types: false,
    show_filters: true,
    show_unknown_as_hole: true,
  };

let segmentize = inline =>
  ExpToSegment.exp_to_segment(~settings=exp_to_segment_settings(inline), _);

let print = (~inline=false, exp) =>
  Printer.of_segment(~holes=Some("?"), segmentize(inline, exp));
