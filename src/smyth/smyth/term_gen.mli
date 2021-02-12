(** Efficient type-directed guessing consistent with {b Figure 17} of the
    ICFP 2020 paper (Appendix A).

    This module uses extensive memoization and other efficiency techniques,
    so the implementation looks nothing like {b Figure 17} of the ICFP 2020
    paper. See Section 7.3.4 of Peter-Michael Osera's PhD thesis for details
    about how efficient raw-term enumeration is achieved in {e Myth} (the
    details of which transfer readily to {e Smyth}).

    {b Warning:} this module uses mutation to manage the memoization cache.
    See {!clear_cache}. *)

open Lang

val fresh_ident : string list -> char -> string
(** [fresh_ident gamma c] returns an identifier starting with the character
    [c] that does not appear in the type context [gamma] *)

val function_char : char
(** The canonical first character for function names. *)

val variable_char : char
(** The canonical first character for variable names. *)

val match_char : char
(** The canonical first character for variable names used in pattern matching
    in a [case] expression. *)

val clear_cache : unit -> unit
(** Clears the memoization cache. {b Important note:} make sure to call
    [clear_cache] once synthesis is fully complete for a problem, and not any
    sooner or later! *)

val up_to : datatype_ctx -> int -> gen_goal -> exp Nondet.t
(** [up_to_e sigma n goal] nondeterministically generates terms at a goal
    [goal] up to (and including) AST size [n]. *)
