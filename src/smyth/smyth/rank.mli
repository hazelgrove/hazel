(** Ranking algorithms for synthesis solutions.

    Algorithm configurable via the {!Params} module. *)

open Lang

val rank : (hole_name * exp) list -> int
(** Computes a rank for a synthesis solution (lower is better). *)

val sort : (hole_name * exp) list list -> (hole_name * exp) list list
(** Sorts a list of synthesis solutions by rank. *)

val first_recursive :
  (hole_name * exp) list list -> (hole_name * exp) list option
(** Returns the best-ranked {i recursive} synthesis solution from a list. *)
