(** Example helpers and "ground truth" example satisfaction. *)

open Lang

val from_value : value -> example
(** "Upcasts" a simple value to an example. *)

val res_satisfies : hole_filling -> res -> example -> bool
(** Example satisfaction as defined in {b Figure 5} of the ICFP 2020 paper. *)

val exp_satisfies : hole_filling -> exp -> worlds -> bool
(** Example constraint satisfaction as defined in {b Figure 5} of the ICFP
    2020 paper. *)
