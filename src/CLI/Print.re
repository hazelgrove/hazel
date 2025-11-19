open Haz3lcore;

let exp_to_segment_settings: ExpToSegment.Settings.t =
  ExpToSegment.Settings.editable(~inline=false);

let segmentize =
  ExpToSegment.exp_to_segment(~settings=exp_to_segment_settings, _);

let print = exp => Printer.of_segment(~holes="?", segmentize(exp));
