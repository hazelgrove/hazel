/** Evaluation environments.

    This module provides functions that operate on the evaluation environment
    data structure used in {e Smyth}.

    Entries closer to the "beginning" of the environment shadow entries
    appearing later. Entries are typically "consed" to the beginning of an
    environment (for example, with {!add_res}). */;

open Lang;

/** The empty evaluation environment. */

let empty: env;

/** [all_res env] returns all name-to-{!Lang.res} bindings in the environment
    [env]; that is, the assignment of expression variables to results. */

let all_res: env => list((string, res));

/** [all_type env] returns all name-to-{!Lang.typ} bindings in the environment
    [env]; that is, the assignment of type variables to types. */

let all_type: env => list((string, typ));

/** Concatenates a list of environments. */

let concat: list(env) => env;

/** Adds a name-to-{!Lang.res} binding to the beginning of an environment. */

let add_res: ((string, res), env) => env;

/** Concatenates a list of name-to-{!Lang.res} bindings to the beginning of an
    environment. */

let concat_res: (list((string, res)), env) => env;

/** Adds a name-to-{!Lang.typ} binding to the beginning of an environment. */

let add_type: ((string, typ), env) => env;

/** Concatenates a list of name-to-{!Lang.typ} bindings to the beginning of an
    environment. */

let concat_type: (list((string, typ)), env) => env;
