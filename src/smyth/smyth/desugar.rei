/** Helper module for constructing {!Lang.exp} values.

    See {!Sugar} for some inverse operations. */;

open Lang;

/** [lett tau x binding body] constructs the equivalent of the following
    let-binding expression:
    {[let (x : tau) = binding in body]} */

let lett: (typ, string, exp, exp) => exp;

/** [func_params [x0, ..., xN] body] constructs the equivalent of the following
    nested lambda expression:
    {[\x0 -> ... \xN -> body]} */

let func_params: (list(param), exp) => exp;

/** [app head [e0, ..., eN] body] constructs the equivalent of the following
    application expression:
    {[head e0 ... eN]} */

let app: (exp, list(exp_arg)) => exp;

/* Precondition: input >= 0 */
/** [nat n] constructs the expression [S (... S (Z ()) ...)], where the [S]
    constructor is nested [n] times (i.e., [S]{^[n]}[(Z ())]).


    {b Precondition:} [n >= 0]. */

let nat: int => exp;

/** [listt [e0, ..., eN] taus] constructs the following polymorphic list
    expression:

    {[Cons<taus> (e0, ... Cons<taus> (eN, Nil<taus>))]} */

let listt: (list(exp), list(typ)) => exp;

/** Packages up datatypes, function definitions, assertions, and the main
    expression of a program into a single type. */

type program = {
  datatypes: datatype_ctx,
  definitions: list((string, (typ, exp))),
  assertions: list((exp, exp)),
  main_opt: option(exp),
};

/** Desugars a {!program} value into the corresponding {!Lang.exp} value (along
    with its datatype context). */

let program: program => (exp, datatype_ctx);
