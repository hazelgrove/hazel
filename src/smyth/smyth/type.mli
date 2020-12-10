(** Type helpers and type-checking. *)

open Lang

(** {1:helpers Helpers} *)

val equal : typ -> typ -> bool
(** [equal tau1 tau2] determines if [tau1] and [tau2] have the same abstract
    syntax tree. *)

val wildcard : typ
(** The "wildcard" type. *)

val matches : typ -> typ -> bool
(** [matches tau1 tau2] determines if [tau1] and [tau2] have the same
    abstract syntax tree modulo any wildcards. *)

val is_base : typ -> bool
(** [is_base tau] returns determines if [tau] is a base type. *)

(* Suppose tau = a -> (b -> (c -> d)). Then: *)
val domain_of_codomain : codomain:typ -> typ -> typ option
(** [domain_of_codomain ~codomain root] looks for [codomain] on the
    right-hand side of a type arrow in [root] that is not on the left-hand
    side of any type arrow and, if found, returns the left-hand side of that
    type arrow.

    For example, suppose [tau = a -> (b -> (c -> d))]. Then:

    {[
      domain_of_codomain ~codomain:d tau = Some c
      domain_of_codomain ~codomain:(c -> d) tau = Some b
      domain_of_codomain ~codomain:(b -> (c -> d)) tau = Some a
      domain_of_codomain ~codomain:(a -> (b -> (c -> d))) tau = None
    ]}

    Also:

    {[
      domain_of_codomain ~codomain:a a = None
      domain_of_codomain ~codomain:a (() -> a) = Some ()
    ]} *)

val bind_spec : type_ctx -> exp -> bind_spec
(** [bind_spec gamma e] returns the computed binding specification of [e]
    with respect to [gamma]. In particular:

    - Projections decrease the binding specification of their argument
    - Variables are looked up in [gamma] *)

val sub_bind_spec : bind_spec -> bind_spec
(** [sub_bind_spec b] "decreases" the binding specification [b]. In
    particular, if [b] is a binding specification for the argument of some
    function, then [sub_bind_spec b] will be a binding specification for
    {i decreasing} on that function. *)

val structurally_decreasing_bind_spec :
  head_spec:bind_spec -> arg_spec:bind_spec -> bool
(** [structurally_decreasing_bind_spec ~head_spec ~arg_spec] determines if
    [arg_spec] is decreasing on [head_spec]. If [head_spec] is not a binding
    specification for a recursive function, then
    [structurally_decreasing ~head_spec ~arg_spec] will return [true]. *)

val structurally_decreasing : type_ctx -> head:exp -> arg:exp -> bool
(** [structurally_decreasing gamma ~head ~arg] determines if [arg] is
    structurally decreasing on [head] in the context [gamma]. *)

val matches_dec : string option -> bind_spec -> bool
(** [matches_dec (Some f) bind_spec] determines if [bind_spec] is
    structurally decreasing on the recursive function [f].
    [matches_dec None bind_spec] will always return [true]. *)

val peel_forall : typ -> string list * typ
(** Peels all outer universal quantifiers on a type into a list. For example:

    {[
      peel_forall (forall a.forall b.a * b)
      = ([("a", "b")], a * b) peel_forall a
      = ([], a) peel_forall (int * int)
      = ([], int * int) peel_forall (forall c.int)
      = (["c"], int) peel_forall (forall c.d)
      = (["c"], d)
    ]} *)

(** {1:substitution Substitution} *)

val substitute : before:string -> after:typ -> typ -> typ
(** [substitute ~before ~after tau] substitutes replaces free occurrences of
    [before] in [tau] with [after]. *)

val substitute_many : bindings:(string * typ) list -> typ -> typ
(** Performs many substitutions one after another; see {!substitute}. *)

(** {1:typechecking Type-checking}

    The following functions implement a bidirectional type checker. *)

(** Type-checking errors. *)
type error =
  | VarNotFound of string
  | CtorNotFound of string
  | PatternMatchFailure of typ * pat
  | WrongNumberOfTypeArguments of int * int
  | GotFunctionButExpected of typ
  | GotTupleButExpected of typ
  | GotTypeAbstractionButExpected of typ
  | GotButExpected of typ * typ
  | BranchMismatch of string * string
  | CannotInferFunctionType
  | CannotInferCaseType
  | CannotInferHoleType
  | ExpectedArrowButGot of typ
  | ExpectedTupleButGot of typ
  | ExpectedForallButGot of typ
  | ExpectedDatatypeButGot of typ
  | TupleLengthMismatch of typ
  | ProjectionLengthMismatch of typ
  | ProjectionOutOfBounds of int * int
  | TypeAbstractionParameterNameMismatch of string * string
  | AssertionTypeMismatch of typ * typ

val check :
  datatype_ctx -> type_ctx -> exp -> typ -> (hole_ctx, exp * error) result
(** Bidirectional type "checking". *)

val infer :
  datatype_ctx -> type_ctx -> exp -> (typ * hole_ctx, exp * error) result
(** Bidirectional type "inference" or "synthesis". *)
