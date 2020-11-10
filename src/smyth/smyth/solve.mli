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

val solve_once :
     hole_name
  -> hole_ctx
  -> datatype_ctx
  -> (hole_filling * worlds hole_map) Nondet.t
  -> ( hole_filling
     * (hole_name * (type_ctx * typ * string option * int)) list )
     Nondet.t
(** [solve_once hole_name delta sigma possible_ks] tries to incrementally
    solve the constraint in [possible_ks] bound to [hole_name]. As soon as
    [solve_once] finds a solution to the named element of [possible_ks], it
    returns that solution (which is a nondeterministic set.) *)
