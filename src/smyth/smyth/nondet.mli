(** Nondeterministic computations via collections semantics. *)

(** The type of a nondeterministic computation; conceptually, a list. *)
type 'a t

(** {1:construction Construction} *)

val none : 'a t
(** The empty nondeterministic computation (i.e., one with no possible
    outcomes); useful for cutting off a nondeterministic computation. *)

val from_list : 'a list -> 'a t
(** Treats each entry in the list as a possible nondeterministic outcome. *)

(** {1:collection Collection} *)

val to_list : 'a t -> 'a list
(** Collects all possible nondeterministic outputs. *)

(** {1:core Core functions} *)

val map : ('a -> 'b) -> 'a t -> 'b t

val pure : 'a -> 'a t

val join : 'a t t -> 'a t

(** {1:generic Generic library functions} *)

val pure_bind : 'a t -> ('a -> 'b) -> 'b t

val bind : 'a t -> ('a -> 'b t) -> 'b t

val and_then : ('a -> 'b t) -> 'a t -> 'b t

val guard : bool -> unit t

(** {1:specific Specific library functions} *)

val union : 'a t list -> 'a t
(** Takes a list of nondeterministic computations and (conceptually)
    nondeterministically chooses {i just one} of the computations to perform;
    the "sum" operation. *)

val one_of_each : 'a t list -> 'a list t
(** Takes a list of nondeterministic computations and (conceptually) chooses
    one computation {i from each} to perform; the "product" operation. *)

val is_empty : 'a t -> bool
(** Checks if a nondeterministic computation is empty (no possible outcomes). *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** Filters all possible nondeterministic outcomes with a predicate. *)

val dedup : 'a t -> 'a t
(** Deduplicates nondeterministic outcomes. *)

val collapse_option : 'a option t -> 'a t
(** Filters all possible nondeterministic outcomes to include only those that
    are [Some]thing. *)

val take : int -> 'a t -> 'a t
(** [take n nd] limits the nondeterministic computation [nd] to at most [n]
    possible outcomes. *)

val curb_overflow : int -> 'a t -> 'a t
(** [curb_overflow n nd] entirely eliminates {i all} possible outcomes of the
    nondeterministic computation [nd] if it has more than [n] possible
    outcomes; useful for eliminating pathological branches of computation. *)

(** {1:lifting Lifting} *)

val lift_option : 'a option -> 'a t

val lift_result : ('a, 'e) result -> 'a t

(** {1:syntax Syntax} *)

module Syntax : sig
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end
