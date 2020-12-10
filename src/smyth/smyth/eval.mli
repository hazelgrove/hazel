(** Live evaluation and resumption, as defined in {b Figure 4} of the ICFP
    2020 paper.

    {b Figure 7} also defines "program evaluation"; that concept is simply
    included in the definition of live evaluation here. *)

open Lang

(** The return type of live evaluation and resumption. Live evaluation and
    resumption may fail (e.g., by running out of fuel), in which case they
    will return a {!type:string} describing the error that occurred. *)
type eval_result = (res * resumption_assertions, string) result

val eval : env -> exp -> eval_result
(** [eval env exp] live evaluates the expression [exp] in the environment
    [env]. *)

val resume : hole_filling -> res -> eval_result
(** [resume hf res] resumes the result [res] with the hole-filling [hf]. *)
