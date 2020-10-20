(** Type-and-example-directed hole synthesis, as defined in {b Figure 8} of
    the ICFP 2020 paper. *)

open Lang

val fill :
     synthesis_params
  -> hole_ctx
  -> datatype_ctx
  -> hole_filling
  -> fill_goal
  -> (constraints * hole_ctx) Nondet.t
(** [fill params delta sigma f g] performs type-and-example-directed hole
    synthesis over the goal defined by [g] as defined by {b Figure 8} of the
    ICFP 2020 paper. Relies on {!Refine} and {!Branch} for the rule
    {e Refine-or-Branch} and {!Term_gen} for the rule {e Guess-and-Check}. *)
