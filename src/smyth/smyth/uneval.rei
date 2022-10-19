/** Live bidirectional example checking via live unevaluation, as defined in
    {b Figure 6} of the ICFP 2020 paper. */;

open Lang;

/** Whether or not unevaluation should proceed in the "minimal" mode.
    Minimal mode uses {e U-Case-Guess} and not {e U-Case};
    non-minimal mode does the opposite. */

let minimal_uneval: ref(bool);

/** Live bidirectional example checking. */

let check:
  (hole_ctx, datatype_ctx, hole_filling, exp, worlds) => Nondet.t(constraints);

/** Live unevaluation. */

let uneval:
  (hole_ctx, datatype_ctx, hole_filling, res, example) =>
  Nondet.t(constraints);

/** Assertion simplification, as defined in {b Figure 7} of the ICFP 2020
    paper. */

let simplify_assertions:
  (hole_ctx, datatype_ctx, resumption_assertions) => Nondet.t(constraints);
