/** Type helpers and type-checking. */;

open Lang;

/** {1:helpers Helpers} */;

/** [equal tau1 tau2] determines if [tau1] and [tau2] have the same
    abstract syntax tree. */

let equal: (typ, typ) => bool;

/** The "wildcard" type. */

let wildcard: typ;

/** [matches tau1 tau2] determines if [tau1] and [tau2] have the same
    abstract syntax tree modulo any wildcards. */

let matches: (typ, typ) => bool;

/** [is_base tau] returns determines if [tau] is a base type. */

let is_base: typ => bool;

/* Suppose tau = a -> (b -> (c -> d)). Then:
 */
/** [domain_of_codomain ~codomain root] looks for [codomain] on the right-hand
    side of a type arrow in [root] that is not on the left-hand side of any type
    arrow and, if found, returns the left-hand side of that type arrow.

    For example, suppose [tau = a -> (b -> (c -> d))]. Then:
      {[domain_of_codomain ~codomain:d tau = Some c
      domain_of_codomain ~codomain:(c -> d) tau = Some b
      domain_of_codomain ~codomain:(b -> (c -> d)) tau = Some a
      domain_of_codomain ~codomain:(a -> (b -> (c -> d))) tau = None]}

    Also:
      {[domain_of_codomain ~codomain:a a = None
      domain_of_codomain ~codomain:a (() -> a) = Some ()]} */

let domain_of_codomain: (~codomain: typ, typ) => option(typ);

/** [bind_spec gamma e] returns the computed binding specification of [e] with
    respect to [gamma]. In particular:

    - Projections decrease the binding specification of their argument
    - Variables are looked up in [gamma] */

let bind_spec: (type_ctx, exp) => bind_spec;

/** [sub_bind_spec b] "decreases" the binding specification [b]. In particular,
    if [b] is a binding specification for the argument of some function, then
    [sub_bind_spec b] will be a binding specification for {i decreasing} on that
    function. */

let sub_bind_spec: bind_spec => bind_spec;

/** [structurally_decreasing_bind_spec ~head_spec ~arg_spec] determines if
    [arg_spec] is decreasing on [head_spec]. If [head_spec] is not a binding
    specification for a recursive function, then
    [structurally_decreasing ~head_spec ~arg_spec] will return [true]. */

let structurally_decreasing_bind_spec:
  (~head_spec: bind_spec, ~arg_spec: bind_spec) => bool;

/** [structurally_decreasing gamma ~head ~arg] determines if [arg] is
    structurally decreasing on [head] in the context [gamma]. */

let structurally_decreasing: (type_ctx, ~head: exp, ~arg: exp) => bool;

/** [matches_dec (Some f) bind_spec] determines if [bind_spec] is structurally
    decreasing on the recursive function [f]. [matches_dec None bind_spec] will
    always return [true]. */

let matches_dec: (option(string), bind_spec) => bool;

/** Peels all outer universal quantifiers on a type into a list. For example:

  {[peel_forall (forall a . forall b . a * b) = (["a", "b"], a * b)
  peel_forall a = ([], a)
  peel_forall (int * int) = ([], int * int)
  peel_forall (forall c . int) = (["c"], int)
  peel_forall (forall c . d) = (["c"], d)]} */

let peel_forall: typ => (list(string), typ);

/** {1:substitution Substitution} */;

/** [substitute ~before ~after tau] substitutes replaces free occurrences of
    [before] in [tau] with [after]. */

let substitute: (~before: string, ~after: typ, typ) => typ;

/** Performs many substitutions one after another; see {!substitute}. */

let substitute_many: (~bindings: list((string, typ)), typ) => typ;

/** {1:typechecking Type-checking}
    The following functions implement a bidirectional type checker. */;

/** Type-checking errors. */

type error =
  | VarNotFound(string)
  | CtorNotFound(string)
  | PatternMatchFailure(typ, pat)
  | WrongNumberOfTypeArguments(int, int)
  | GotFunctionButExpected(typ)
  | GotTupleButExpected(typ)
  | GotTypeAbstractionButExpected(typ)
  | GotButExpected(typ, typ)
  | BranchMismatch(string, string)
  | CannotInferFunctionType
  | CannotInferCaseType
  | CannotInferHoleType
  | ExpectedArrowButGot(typ)
  | ExpectedTupleButGot(typ)
  | ExpectedForallButGot(typ)
  | ExpectedDatatypeButGot(typ)
  | TupleLengthMismatch(typ)
  | ProjectionLengthMismatch(typ)
  | ProjectionOutOfBounds(int, int)
  | TypeAbstractionParameterNameMismatch(string, string)
  | AssertionTypeMismatch(typ, typ);

/** Bidirectional type "checking". */

let check:
  (datatype_ctx, type_ctx, exp, typ) => result(hole_ctx, (exp, error));

/** Bidirectional type "inference" or "synthesis". */

let infer:
  (datatype_ctx, type_ctx, exp) => result((typ, hole_ctx), (exp, error));
