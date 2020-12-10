(** Expression helpers. *)

open Lang

val syntactically_equal : exp -> exp -> bool
(** [syntactically_equal e1 e2] determines if [e1] and [e2] have the same
    abstract syntax tree. *)

val largest_hole : exp -> hole_name
(** [largest_hole e] returns the greatest-numbered hole in [e] *)

val has_special_recursion : exp -> bool
(** [has_special_recursion e] determines if [e] or one of its subexpressions
    has an application marked as "special". See {!Lang.exp} for details about
    "special" applications. *)

val fill_hole : hole_name * exp -> exp -> exp
(** [fill_hole (h, e) root] replaces a hole ??{_ h} with the expression [e]
    in the expression [root]. *)
