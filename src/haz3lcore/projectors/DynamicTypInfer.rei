/* The type a probed expression was observed to have at runtime, inferred
   from the values the probe sampled.

   Samples are already-elaborated values, so a type is read off each one by
   running statics over it and the results are met. The result is only ever
   as precise as the values that happened to flow through: it describes what
   was seen, not what the expression can produce. */

open Language;

/* The meet of the types of every sample: None when there are no samples, or
   when the sample types disagree. Statics is total, so a sample it marks
   still reports the type it recovered rather than nothing. [ctx] is the
   source context, so that user-defined types are visible. */
let dynamic_typ_of_samples: (~ctx: Ctx.t, list(Sample.t)) => option(Typ.t);

/* Converting a type to a segment and naming the tokens of it that some other
   type does not account for: ProjectorBase.utility.typ_to_seg_with_diff_ids,
   injected rather than called directly because it sits downstream of the
   projectors and this module cannot name it. */
type typ_to_seg_with_diff_ids =
  (~ctx: Ctx.t, ~against: Typ.t, Typ.t) => (Base.segment, Id.Set.t);

/* The segment to show for [dynamic_typ], and the ids of its tokens that
   [static_typ] does not account for -- the ones runtime supplied rather than
   statics. The segment is built here rather than left to the caller because
   the ids describe that one segment and no other. */
let segment_and_dynamic_ids:
  (
    ~typ_to_seg_with_diff_ids: typ_to_seg_with_diff_ids,
    ~ctx: Ctx.t,
    ~static_typ: Typ.t,
    ~dynamic_typ: Typ.t
  ) =>
  (Base.segment, Id.Set.t);
