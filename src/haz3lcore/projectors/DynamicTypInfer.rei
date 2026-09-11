/* The type a probed expression was observed to have at runtime, inferred
   from the values the probe sampled.

   Samples are already-elaborated values, so a type is read off each one by
   running statics over it and the results are met. The result is only ever
   as precise as the values that happened to flow through: it describes what
   was seen, not what the expression can produce. */

open Language;

/* The meet of the types of every sample, or None if a sample fails to
   type-check or the samples disagree. [ctx] is the source context, so that
   user-defined types are visible. */
let dynamic_typ_of_samples: (~ctx: Ctx.t, list(Sample.t)) => option(Typ.t);

/* Rendering a type and naming the tokens of it that some other type does not
   account for: ProjectorBase.utility.typ_to_seg_with_diff_ids, injected rather
   than called directly because the renderer sits downstream of the
   projectors and this module cannot name it. */
type render_with_diff_ids =
  (~ctx: Ctx.t, ~against: Typ.t, Typ.t) => (Base.segment, Id.Set.t);

/* The segment to show in the type probe's Dynamic mode, and the ids of its
   tokens that came from runtime rather than from [static_typ]. Rendering is
   done here rather than left to the caller because the ids describe that one
   render and no other. Falls back to Unknown when nothing can be inferred. */
let displayed_segment_and_dynamic_ids:
  (
    ~render_with_diff_ids: render_with_diff_ids,
    ~ctx: Ctx.t,
    ~static_typ: Typ.t,
    ~samples: list(Sample.t)
  ) =>
  (Base.segment, Id.Set.t);
