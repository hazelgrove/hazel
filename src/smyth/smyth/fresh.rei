/** Manages fresh (unique) hole names.

    {b Warning:} this module uses mutation! */;

open Lang;

/** The canonical unused hole name. */

let unused: hole_name;

/** [set_largest_hole h] sets the current laregest hole to be [h]; all new
    holes generated after calling [set_largest_hole h] will have hole name
    greater than [h]. */

let set_largest_hole: hole_name => unit;

/** Generates a fresh (unique) hole name. */

let gen_hole: unit => hole_name;
