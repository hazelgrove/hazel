(** Constraint solving, as defined in {b Figure 8} of the ICFP 2020 paper. *)

open Lang

val solve_any :
     hole_ctx
  -> datatype_ctx
  -> constraints Nondet.t
  -> (hole_filling * hole_ctx) Nondet.t
(** [solve_any delta sigma possible_ks] tries to solve any one of the
    possible constraints [possible_ks]. As soon as [solve_any] finds a
    solution to one of the [possible_ks], it returns that solution (which is
    a nondeterministic set.) *)
