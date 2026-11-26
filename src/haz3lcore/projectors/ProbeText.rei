/* Renders a segment or zipper as plain text with each probe's recorded value
   inlined at the probe, for anywhere without a DOM. `~window` selects how much of a probe's sample
   history is shown. */

let of_segment:
  (
    ~projector_to_segment: Base.projector => Segment.t=?,
    ~window: Language.Sample.Window.mode=?,
    ~probe_map: Language.Sample.Map.t,
    ~refractors: Zipper.Refractor.RefractorList.t,
    Segment.t
  ) =>
  string;

let of_zipper:
  (
    ~projector_to_segment: Base.projector => Segment.t=?,
    ~window: Language.Sample.Window.mode=?,
    ~probe_map: Language.Sample.Map.t,
    ZipperBase.t
  ) =>
  string;

/** Between a probe's values in one line of text. */
let value_separator: string;
/** The longest prefix of [s] within [cols] columns, on grapheme bounds. */
let prefix_within_columns: (int, string) => string;
/** A value as one line of program text, cut to [max_length]. */
let format_value: (~max_length: int=?, Language.Exp.t) => string;
/** Each probe's values, or its empty-status indicator. */
let probe_values:
  (
    ~window: Language.Sample.Window.mode,
    ~probe_map: Language.Sample.Map.t,
    list(Id.t)
  ) =>
  list(string);
