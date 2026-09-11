open Util;
open Language;
open ProjectorBase;
open Virtual_dom.Vdom;
open Node;

/* ProbePill — the pure geometry/typography of a probe sample pill,
   shared between ProbeProj's interactive samples and standalone
   contexts (the constellation's value chips and constant panels).

   Deliberately stateless: budget POLICY (window defaults, per-sample
   resize overrides) and budget STORAGE stay with the caller. This
   module owns the two directions of the length relationship —
   budget -> fitted segment, and target width -> budget — plus the
   standalone DOM hierarchy that proj-probe.css styles.

   Extracted from ProbeProj.re (see the pill-view abstraction note in
   plans/agent-canvas-docket.md). */

/* Structural abbreviation at a character budget; returns the segment
   and its actual rendered length in columns. */
let fit_seg = ProbeUtil.abbreviated_seg_of;

/* Largest budget whose rendered width fits the target: width isn't
   linear in budget (Abbreviate is discrete), so bisect. `width_at`
   maps a budget to a rendered width in columns. */
let best_budget = (width_at: int => int, target_width: int): int => {
  let rec find_upper = (b: int): int =>
    if (b > 500 || width_at(b) > target_width) {
      b;
    } else {
      find_upper(b * 2 + 1);
    };
  let upper = find_upper(max(1, target_width));
  let rec bisect = (lo: int, hi: int): int =>
    if (lo >= hi) {
      lo;
    } else {
      let mid = (lo + hi + 1) / 2;
      if (width_at(mid) <= target_width) {
        bisect(mid, hi);
      } else {
        bisect(lo, mid - 1);
      };
    };
  bisect(target_width, upper);
};

/* Fit a value to a column target directly (the standalone callers'
   one-step path). */
let fit_to =
    (~utility: utility, ~target_cols: int, value: Exp.t): (Segment.t, int) => {
  let width_at = (b: int): int => fit_seg(utility, b, value) |> snd;
  fit_seg(utility, best_budget(width_at, target_cols), value);
};

/* Width classes keyed off the FITTED length, so the class can never
   drift from the segment it describes. */
let length_cls = (length: int): string =>
  if (length > 10) {
    "extra";
  } else if (length > 4) {
    "s" ++ string_of_int(length - 4);
  } else {
    "s0";
  };

/* The pill's own DOM hierarchy for contexts OUTSIDE a probe projector:
   proj-probe.css keys the backing/ink/typography on these classes, so
   standalone chips stay visually identical to in-probe samples by
   construction. */
let standalone =
    (~state_classes: list(string)=["focus", "depth-same"], content) =>
  div(
    ~attrs=[Attr.classes(["sample"])],
    [div(~attrs=[Attr.classes(["value", ...state_classes])], content)],
  );
