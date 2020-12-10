(** Type-and-example-directed branching, as defined in {b Figure 9} of the
    ICFP 2020 paper. *)

open Lang

val branch :
     int
  -> hole_ctx
  -> datatype_ctx
  -> hole_filling
  -> synthesis_goal
  -> ((exp * fill_goal list) * constraints) Nondet.t
(** [branch max_scrutinee_size delta sigma hf goal] splits the synthesis goal
    [goal] into subgoals based on type-and-example-directed branching with a
    maximum synthesized scrutinee size of [max_scrutinee_size]. *)
