(** Evaluation environments.

    This module provides functions that operate on the evaluation environment
    data structure used in {e Smyth}.

    Entries closer to the "beginning" of the environment shadow entries
    appearing later. Entries are typically "consed" to the beginning of an
    environment (for example, with {!add_res}). *)

open Lang

val empty : env
(** The empty evaluation environment. *)

val all_res : env -> (string * res) list
(** [all_res env] returns all name-to-{!Lang.res} bindings in the environment
    [env]; that is, the assignment of expression variables to results. *)

val all_type : env -> (string * typ) list
(** [all_type env] returns all name-to-{!Lang.typ} bindings in the
    environment [env]; that is, the assignment of type variables to types. *)

val concat : env list -> env
(** Concatenates a list of environments. *)

val add_res : string * res -> env -> env
(** Adds a name-to-{!Lang.res} binding to the beginning of an environment. *)

val concat_res : (string * res) list -> env -> env
(** Concatenates a list of name-to-{!Lang.res} bindings to the beginning of
    an environment. *)

val add_type : string * typ -> env -> env
(** Adds a name-to-{!Lang.typ} binding to the beginning of an environment. *)

val concat_type : (string * typ) list -> env -> env
(** Concatenates a list of name-to-{!Lang.typ} bindings to the beginning of
    an environment. *)
