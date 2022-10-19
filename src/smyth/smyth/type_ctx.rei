/** Type contexts.

    This module provides functions that operate on the type context
    data structure used in {e Smyth}.

    Entries closer to the "beginning" of the context shadow entries
    appearing later. Entries are typically "consed" to the beginning of an
    environment (for example, with {!add_type}). */;

open Lang;

/** The empty type context. */

let empty: type_ctx;

/** [all_type gamma] returns all name-to-{!Lang.typ} (and {!Lang.bind_spec})
    bindings in the type context [gamma]. */

let all_type: type_ctx => list(type_binding);

/** [all_poly gamma] returns all polymorphic type variables in the type context
    [gamma]. */

let all_poly: type_ctx => list(string);

/** Concatenates a list of type contexts. */

let concat: list(type_ctx) => type_ctx;

/** Adds a name-to-{!Lang.typ} binding to the beginning of a type context. */

let add_type: (type_binding, type_ctx) => type_ctx;

/** Concatenates a list of name-to-{!Lang.typ} bindings to the beginning of a
    type context. */

let concat_type: (list(type_binding), type_ctx) => type_ctx;

/** Adds a polymorphic type variable to a type context. */

let add_poly: (poly_binding, type_ctx) => type_ctx;

/** Concatenates a list of polymorphic type variables to the beginning of a type
    context. */

let concat_poly: (list(poly_binding), type_ctx) => type_ctx;

/** [peel_type gamma] "pattern matches" on [gamma]. If [gamma] is nonempty, the
    first name-to-{!Lang.typ} binding in it is returned along with the result
    of removing that binding from [gamma] (i.e., the "rest" of [gamma]).
    If [gamma] is empty, [None] is returned. */

let peel_type: type_ctx => option((type_binding, type_ctx));

/** [names gamma] returns a list of all bound names in [gamma];
    that is, both the names of the name-to-{!Lang.typ} bindings as well as the
    polymorphic type variables of [gamma]. */

let names: type_ctx => list(string);
