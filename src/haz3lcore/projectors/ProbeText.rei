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
