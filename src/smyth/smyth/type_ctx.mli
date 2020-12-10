(** Type contexts.

    This module provides functions that operate on the type context data
    structure used in {e Smyth}.

    Entries closer to the "beginning" of the context shadow entries appearing
    later. Entries are typically "consed" to the beginning of an environment
    (for example, with {!add_type}). *)

open Lang

val empty : type_ctx
(** The empty type context. *)

val all_type : type_ctx -> type_binding list
(** [all_type gamma] returns all name-to-{!Lang.typ} (and {!Lang.bind_spec})
    bindings in the type context [gamma]. *)

val all_poly : type_ctx -> string list
(** [all_poly gamma] returns all polymorphic type variables in the type
    context [gamma]. *)

val concat : type_ctx list -> type_ctx
(** Concatenates a list of type contexts. *)

val add_type : type_binding -> type_ctx -> type_ctx
(** Adds a name-to-{!Lang.typ} binding to the beginning of a type context. *)

val concat_type : type_binding list -> type_ctx -> type_ctx
(** Concatenates a list of name-to-{!Lang.typ} bindings to the beginning of a
    type context. *)

val add_poly : poly_binding -> type_ctx -> type_ctx
(** Adds a polymorphic type variable to a type context. *)

val concat_poly : poly_binding list -> type_ctx -> type_ctx
(** Concatenates a list of polymorphic type variables to the beginning of a
    type context. *)

val peel_type : type_ctx -> (type_binding * type_ctx) option
(** [peel_type gamma] "pattern matches" on [gamma]. If [gamma] is nonempty,
    the first name-to-{!Lang.typ} binding in it is returned along with the
    result of removing that binding from [gamma] (i.e., the "rest" of
    [gamma]). If [gamma] is empty, [None] is returned. *)

val names : type_ctx -> string list
(** [names gamma] returns a list of all bound names in [gamma]; that is, both
    the names of the name-to-{!Lang.typ} bindings as well as the polymorphic
    type variables of [gamma]. *)
