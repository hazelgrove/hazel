/** Ranking algorithms for synthesis solutions.

    Algorithm configurable via the {!Params} module. */;

open Lang;

/** Computes a rank for a synthesis solution (lower is better). */

let rank: list((hole_name, exp)) => int;

/** Sorts a list of synthesis solutions by rank. */

let sort: list(list((hole_name, exp))) => list(list((hole_name, exp)));

/** Returns the best-ranked {i recursive} synthesis solution from a list. */

let first_recursive:
  list(list((hole_name, exp))) => option(list((hole_name, exp)));
