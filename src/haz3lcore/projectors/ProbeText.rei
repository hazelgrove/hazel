/* Plain-text rendering of a segment or zipper with probe values inlined, for
   the CLI, the composition agent and the debug console. The walk itself is
   private. */

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
