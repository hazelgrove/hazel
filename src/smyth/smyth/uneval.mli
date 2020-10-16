(** Live bidirectional example checking via live unevaluation, as defined in
    {b Figure 6} of the ICFP 2020 paper. *)

open Lang

val minimal_uneval : bool ref
(** Whether or not unevaluation should proceed in the "minimal" mode. Minimal
    mode uses {e U-Case-Guess} and not {e U-Case}; non-minimal mode does the
    opposite. *)

val check :
     hole_ctx
  -> datatype_ctx
  -> hole_filling
  -> exp
  -> worlds
  -> constraints Nondet.t
(** Live bidirectional example checking. *)

val uneval :
     hole_ctx
  -> datatype_ctx
  -> hole_filling
  -> res
  -> example
  -> constraints Nondet.t
(** Live unevaluation. *)

val simplify_assertions :
  hole_ctx -> datatype_ctx -> resumption_assertions -> constraints Nondet.t
(** Assertion simplification, as defined in {b Figure 7} of the ICFP 2020
    paper. *)
