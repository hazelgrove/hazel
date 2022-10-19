/** Hole fillings, unfilled holes (unsolved constraints), and unevaluation
    constraints.

    This module provides functions that operate on the various constraint data
    structures used in {e Smyth}. */;

open Lang;

/** Deletes and returns the minimum element of a hole map (such as a
    {!type:Lang.unsolved_constraints}). */

let delete_min: hole_map('a) => option(((hole_name, 'a), hole_map('a)));

/** The empty unevaluation constraint. */

let empty: constraints;

/** [from_hole_filling hf] creates a new unevaluation constraint consisting of
    the hole filling [hf] and no unfilled holes. */

let from_hole_filling: hole_filling => constraints;

/** [from_unsolved_constraints us] creates a new unevaluation constraint
    consisting of the unfilled holes [us] and no hole filling. */

let from_unsolved_constraints: unsolved_constraints => constraints;

/** [solved_singleton h e] creates a new unevaluation constraint
    consisting of the singleton hole filling [h -> e] and no unfilled
    holes. */

let solved_singleton: (hole_name, exp) => constraints;

/** [solved_singleton h x] creates a new unevaluation constraint
    consisting of the singleton unfilled hole [h -> x] and no hole filling. */

let unsolved_singleton: (hole_name, worlds) => constraints;

/** Syntactically merges a list of hole fillings, failing if a hole is bound
    to two syntactically unequal expressions. */

let merge_solved: list(hole_filling) => option(hole_filling);

/** Syntactically merges a list of unfilled holes. */

let merge_unsolved: list(unsolved_constraints) => unsolved_constraints;

/** Syntactically merges a list of unevaluation constraints using
    {!merge_solved} and {!merge_unsolved}. */

let merge: list(constraints) => option(constraints);

/** Unevaluation constraint satisfaction as defined by {b Figure 5} of the
    ICFP 2020 paper.

    [satisfies hf ks] checks if a hole filling [hf] satisfies a set of
    uenvaluation constraints [ks] using {!Example.exp_satisfies}
    (i.e., the notion of "ground truth" satisfaction). */

let satisfies: (hole_filling, constraints) => bool;
