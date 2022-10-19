/** Pattern helpers. */;

open Lang;

/** [syntactically_equal p1 p2] determines if [p1] and [p2] have the same
    abstract syntax tree. */

let syntactically_equal: (pat, pat) => bool;

/** Performs pattern-matching to bind a pattern to a result. */

let bind_res: (pat, res) => option(env);

/** Performs pattern-matching to bind a (possibly [None]) recursive function
    name to a result. */

let bind_rec_name_res: (option(string), res) => env;

/** Performs pattern-matching to bind a pattern to a type. */

let bind_typ: (bind_spec, pat, typ) => option(type_ctx);

/** Performs pattern-matching to bind a (possibly [None]) recursive function
    name to a type. */

let bind_rec_name_typ: (option(string), typ) => type_ctx;
