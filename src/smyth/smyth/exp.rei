/** Expression helpers. */;

open Lang;

/** [syntactically_equal e1 e2] determines if [e1] and [e2] have the same
    abstract syntax tree. */

let syntactically_equal: (exp, exp) => bool;

/** [largest_hole e] returns the greatest-numbered hole in [e] */

let largest_hole: exp => hole_name;

/** [has_special_recursion e] determines if [e] or one of its subexpressions has
    an application marked as "special". See {!Lang.exp} for details about
    "special" applications. */

let has_special_recursion: exp => bool;

/** [fill_hole  (h, e) root] replaces a hole ??{_h} with the expression [e]
    in the expression [root]. */

let fill_hole: ((hole_name, exp), exp) => exp;
