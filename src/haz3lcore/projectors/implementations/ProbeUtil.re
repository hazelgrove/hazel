open Util_web;
open ProjectorBase;
open Language;

/* Shared rendering helpers for probe-like projectors (ProbeProj samples and
 * TableProj/TableRenderer cells). Width is measured in Unicode display
 * columns rather than byte length so wide-glyph values size correctly. */

let len_seg = (utility: utility, seg: Segment.t): int =>
  seg |> utility.seg_to_string |> Unicode.Width.columns_of_string;

let seg_of_exp = (utility: utility, exp: Exp.t): (Segment.t, int) => {
  let seg = utility.term_to_seg(~inline=true, Exp(exp));
  (seg, len_seg(utility, seg));
};

let abbreviated_seg_of =
    (utility: utility, available: int, exp: Exp.t): (Segment.t, int) => {
  let (abbr_exp, _length) =
    exp
    |> DHExp.strip_ascriptions
    |> Exp.strip_projectors
    |> Abbreviate.abbreviate_exp(~available);
  seg_of_exp(utility, abbr_exp);
};
