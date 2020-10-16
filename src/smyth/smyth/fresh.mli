(** Manages fresh (unique) hole names.

    {b Warning:} this module uses mutation! *)

open Lang

val unused : hole_name
(** The canonical unused hole name. *)

val set_largest_hole : hole_name -> unit
(** [set_largest_hole h] sets the current laregest hole to be [h]; all new
    holes generated after calling [set_largest_hole h] will have hole name
    greater than [h]. *)

val gen_hole : unit -> hole_name
(** Generates a fresh (unique) hole name. *)
