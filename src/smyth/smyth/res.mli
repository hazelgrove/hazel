(** Result helpers. *)

open Lang

val final : res -> bool
(** Determines whether or not a result is {i final}, as defined in
    {b Figure 11} of the ICFP 2020 paper (Appendix A). *)

val determinate : res -> bool
(** Determines whether or not a result is {i determinate}, as defined in
    {b Figure 11} of the ICFP 2020 paper (Appendix A). *)

val indeterminate : res -> bool
(** Determines whether or not a result is {i indeterminate}, as defined in
    {b Figure 11} of the ICFP 2020 paper (Appendix A). *)

val to_value : res -> value option
(** "Downcasts" a result to a simple value, as defined after {b Figure 11} of
    the ICFP 2020 paper (Appendix A). *)

val from_value : value -> res
(** "Upcasts" a simple value to a result. *)

val consistent : res -> res -> resumption_assertions option
(** [consistent r1 r2] returns the resumption assertions needed to ensure
    that [r1] and [r2] are consistent, as defined in {b Figure 7} of the ICFP
    2020 paper. *)
