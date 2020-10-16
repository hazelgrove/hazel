(** Pretty-printing. *)

open Lang

val pat : pat -> string
(** Pretty-prints a pattern. *)

val typ : typ -> string
(** Pretty-prints a type. *)

val exp : exp -> string
(** Pretty-prints an expression. *)
