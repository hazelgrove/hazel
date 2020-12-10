(** Pattern helpers. *)

open Lang

val syntactically_equal : pat -> pat -> bool
(** [syntactically_equal p1 p2] determines if [p1] and [p2] have the same
    abstract syntax tree. *)

val bind_res : pat -> res -> env option
(** Performs pattern-matching to bind a pattern to a result. *)

val bind_rec_name_res : string option -> res -> env
(** Performs pattern-matching to bind a (possibly [None]) recursive function
    name to a result. *)

val bind_typ : bind_spec -> pat -> typ -> type_ctx option
(** Performs pattern-matching to bind a pattern to a type. *)

val bind_rec_name_typ : string option -> typ -> type_ctx
(** Performs pattern-matching to bind a (possibly [None]) recursive function
    name to a type. *)
