(** Hole fillings, unfilled holes (unsolved constraints), and unevaluation
    constraints.

    This module provides functions that operate on the various constraint
    data structures used in {e Smyth}. *)

open Lang

val delete : hole_name -> 'a hole_map -> 'a hole_map
(** Deletes an element from a hole map (such as a
    {!type:Lang.unsolved_constraints}). *)

val delete_min : 'a hole_map -> ((hole_name * 'a) * 'a hole_map) option
(** Deletes and returns the minimum element of a hole map (such as a
    {!type:Lang.unsolved_constraints}). *)

val empty : constraints
(** The empty unevaluation constraint. *)

val from_hole_filling : hole_filling -> constraints
(** [from_hole_filling hf] creates a new unevaluation constraint consisting
    of the hole filling [hf] and no unfilled holes. *)

val from_unsolved_constraints : unsolved_constraints -> constraints
(** [from_unsolved_constraints us] creates a new unevaluation constraint
    consisting of the unfilled holes [us] and no hole filling. *)

val solved_singleton : hole_name -> exp -> constraints
(** [solved_singleton h e] creates a new unevaluation constraint consisting
    of the singleton hole filling [h -> e] and no unfilled holes. *)

val unsolved_singleton : hole_name -> worlds -> constraints
(** [solved_singleton h x] creates a new unevaluation constraint consisting
    of the singleton unfilled hole [h -> x] and no hole filling. *)

val merge_solved : hole_filling list -> hole_filling option
(** Syntactically merges a list of hole fillings, failing if a hole is bound
    to two syntactically unequal expressions. *)

val merge_unsolved : unsolved_constraints list -> unsolved_constraints
(** Syntactically merges a list of unfilled holes. *)

val merge : constraints list -> constraints option
(** Syntactically merges a list of unevaluation constraints using
    {!merge_solved} and {!merge_unsolved}. *)

val satisfies : hole_filling -> constraints -> bool
(** Unevaluation constraint satisfaction as defined by {b Figure 5} of the
    ICFP 2020 paper.

    [satisfies hf ks] checks if a hole filling [hf] satisfies a set of
    uenvaluation constraints [ks] using {!Example.exp_satisfies} (i.e., the
    notion of "ground truth" satisfaction). *)
