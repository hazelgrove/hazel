# Function-Valued Differentiation Plan

## Goal

Support the following Hazel program:

```text
let f(x) = x**2 + 3*x + 5 in
let f_prime = diff(f) in
f_prime(5)
```

with the behavior:

```text
diff(f)
→ diff(fun x -> x**2 + 3*x + 5)
→ fun x -> diff(x**2 + 3*x + 5, x)
→ fun x -> 2*x + 3

f_prime(5)
→ 13
```

Unary `diff` must infer the differentiation variable from the function
parameter and return a function. It must never expose the function's bound
parameter as a free variable or capture variables from an enclosing scope.

## Scope

### Version 1

- Unary numeric functions of one numeric variable in Hazel.
- `diff(f)` preserves the argument function's inferred arrow type:

  ```text
  (a -> b) -> (a -> b)
  ```

- Version 1 does not add a first-class `Real` type or new real-number syntax to
  Hazel. Integer-looking teaching examples can therefore remain written with
  `*` and `**`. For the supported symbolic calculus fragment, Rocq export gives
  the expression its proof-level interpretation over `R`.
- `diff(expression, variable)` remains supported as the explicit,
  expression-level form used by the calculus stepper.
- Function sugar such as `let f(x) = ...` continues to desugar to an ordinary
  Hazel `Fun`.
- The existing Profile determines which derivative rules and cleanup policies
  may simplify the body of the returned function.
- Rocq export certifies the returned function pointwise.
- Unsupported function bodies remain partially differentiated or stuck; they
  must not invoke a broader tactic that bypasses the Profile.

### Deferred

- Multivariable functions and partial derivatives.
- Choosing among multiple parameters automatically.
- Vector-valued derivatives, gradients, and Jacobians.
- Differentiating arbitrary opaque or recursive functions.
- General splice/projector support such as `^^diff` and `^^diff_proof`.
- Treating derivative values as a new primitive runtime representation.

## Taylor Derivative Teaching Workflow

Add a small, separate teaching operation:

```text
taylor_derivatives(f, n) in body
```

For `n > 0`, it expands to a capture-safe chain of ordinary function-valued
derivatives:

```text
let f_deriv_1 = diff(f) in
let f_deriv_2 = diff(f_deriv_1) in
let f_deriv_3 = diff(f_deriv_2) in
body
```

The generated bindings use fresh ordinary Hazel identifiers. There is no
special prime rendering. `n` is the number of differentiations, and the body
can use any generated derivative while it remains in scope. For example,
`taylor_derivatives(f, 3) in f_deriv_1(5)` can step and evaluate the first
derivative at `5`. At order zero, the expansion leaves the body unchanged.
The standalone form `taylor_derivatives(f, n)` remains supported as shorthand
whose continuation is a hole.

This operation deliberately does not construct a Taylor polynomial yet. Future
work may add:

```text
taylor(f, center, n)
```

which reuses the derivative-chain builder to construct the degree-`n`
polynomial, including derivative evaluation, powers, and factorials. Rocq
export for that polynomial and its remainder theorem is also deferred.

## Current Architecture and Failure

### Static signature

`diff` is currently declared in:

```text
src/language/builtins/BuiltinsBase.re
```

as a function whose argument is a pair:

```text
(unknown, Float) -> Float
```

`CustomStatics.diff_variable_statics` verifies that the second component is a
variable. Unary `diff(f)` therefore needs a statics path that distinguishes:

- explicit expression differentiation: `diff(expression, x)`; and
- function differentiation: `diff(f)`.

### Function differentiation rewrite

`DifferentiationRewrite.applicable_at_root` currently recognizes:

```text
diff(fun x -> body, x)
```

and applies `calc.diff_function` by returning:

```text
diff(body, x)
```

This discards the `Fun` binder. In the demonstrated program, the resulting
`x` becomes free inside an unrelated outer function parameterized by `x'`.

### Substitution risk

`src/language/dynamics/Substitution.re` describes itself as capture-avoiding,
but explicitly documents a limitation for unbound variables that can be
captured. Function-valued differentiation must not depend on textual
replacement or assume that variable names are globally unique.

`ProofHacks.replace_exp` uses static co-context information to avoid replacing
through conflicting binders, but it stops at capture boundaries rather than
providing the complete alpha-renaming operation needed here.

### Proof backend

The calculus proof backend currently extracts a body and explicit variable
from `diff(expression, variable)`. It produces a pointwise
`derivable_pt_lim` certificate. Unary function differentiation should reuse
that certificate machinery, but retain the function abstraction in Hazel and
state the Rocq result pointwise.

## Semantic Design

### Core rule

Introduce a distinct, general rule:

```text
calc.diff_function_value

diff(fun x -> body)
→ fun x -> diff(body, x)
```

The rule preserves the binder. It does not simplify the derivative body by
itself; existing visible calculus rules and enabled cleanup policies do that.

The rule should be separate from the existing explicit form:

```text
calc.diff_function

diff(fun x -> body, x)
→ diff(body, x)
```

If the explicit rule remains, it must only unwrap the body when the explicit
variable is the same binder represented by the function. It must not be used
to implement unary `diff`.

### Binder invariant

For:

```text
diff(fun p -> body)
```

the result must be alpha-equivalent to:

```text
fun fresh_p -> diff(body[p ↦ fresh_p], fresh_p)
```

where `fresh_p` cannot occur:

- free in `body`;
- free in the surrounding context;
- in an expression that will subsequently be substituted into the result; or
- as a conflicting nested binder.

The implementation may preserve `p` when it is already safe. Correctness must
be based on binding identity/co-context, not on string equality alone.

### Free-variable invariant

If `FV` denotes free variables, then:

```text
FV(diff(fun x -> body))
⊆ FV(fun x -> body)
```

Differentiation must not introduce the function parameter as a new free
variable.

Free variables already captured by the function closure remain free in the
function syntax and retain their original meaning:

```text
let a = 3 in
let f(x) = a*x in
diff(f)
```

should produce a function equivalent to:

```text
fun x -> a
```

without turning `a` into the differentiation variable.

### Evaluation behavior

`diff` should be symbolic rather than an unrestricted runtime numerical
derivative:

- If its argument evaluates to a supported unary `Fun`, expose the
  binder-preserving calculus step.
- If the body can be differentiated using the active Profile, continue with
  those rules.
- If the body is unsupported or a required rule is disabled, retain a
  well-scoped residual `diff(body, parameter)` inside the returned function.
- If the argument is not a unary function, report a static error rather than
  guessing a variable.

## Implementation Phases

## Phase 1: Statics and surface behavior

### 1. Represent both accepted call shapes

Extend the custom statics for `diff` so that:

```text
diff(expression, x) : numeric
diff(f)             : a -> b
```

when:

```text
f : a -> b
```

Do not model unary `diff` by weakening its return type to `unknown`. The
resulting function must typecheck when applied:

```text
diff(f)(5.0)
```

### 2. Preserve explicit-variable validation

The existing `DiffVariableRequired` behavior remains for the two-argument
form. Examples such as:

```text
diff(1.0, 2.0)
```

must remain statically invalid.

Unary `diff` does not perform this check because its variable comes from the
function binder.

### 3. Restrict Version 1 to unary real functions

Reject or leave unsupported:

```text
diff(3.0)
diff(fun (x, y) -> x + y)
diff(fun x -> "not real")
```

Use explicit diagnostics so the restriction is understandable.

## Phase 2: Capture-safe binder transformation

### 1. Preserve the existing binder when possible

The core transformation retains the original `Fun` pattern and places the
explicit derivative inside its body. It therefore does not move the body
across a binding boundary and does not need textual substitution or
alpha-renaming:

```text
diff(fun x -> body)
→ fun x -> diff(body, x)
```

If a future transformation must move a body across binders, it should use a
shared capture-safe alpha-renaming utility with independent tests. Do not add
a differentiation-specific textual renamer.

### 2. Construct the function-valued derivative

Extend `DifferentiationRewrite` to recognize unary applications of `diff`.
For a unary `Fun`, produce a `Fun` whose body is the existing explicit
derivative form:

```text
diff(fun x -> body)
→ fun x -> diff(body, x)
```

If alpha-renaming is needed:

```text
diff(fun x -> body)
→ fun x_fresh -> diff(body[x ↦ x_fresh], x_fresh)
```

### 3. Reuse existing body rules

Once the explicit `diff(body, parameter)` is inside the returned function,
reuse:

- sum and difference linearity;
- product, quotient, power, and chain rules;
- basic derivative cleanup;
- identity-power cleanup;
- other Profile-controlled algebra cleanup.

Do not create parallel function-specific versions of each calculus rule.

## Phase 3: Stepper integration

### 1. Make the binder-preserving step visible

The first pedagogical step should explain the operation:

```text
diff(f)
→ fun x -> diff(body, x)
```

Suggested label:

```text
differentiate function
```

Subsequent steps operate inside the function body using the existing calculus
labels.

### 2. Preserve local statics

When the stepper enters the new function body:

- the parameter must be present in the mini-editor's local context;
- it must not receive unknown/free-variable highlighting;
- selecting and replacing a derivative inside the body must preserve the
  binder;
- the final function must remain applicable in the surrounding program.

### 3. Respect Profile verbosity

For:

```text
diff(fun x -> x**2)
```

with basic derivatives and identity powers enabled:

```text
fun x -> 2*x
```

With both disabled:

```text
fun x -> 2*x**1*diff(x, x)
```

The Profile must persist as the stepper traverses into and out of the function
body.

### 4. Support all automation modes through the same trace

- **One Step:** the function-lifting rule is one visible operation.
- **Check Result:** validates a proposed derivative function only through
  enabled catalog rules.
- **Auto simplify:** may propose the fully simplified derivative function but
  must retain the exact profile-derived trace.
- **Replace:** installs the validated function without losing its binder.

No mode may use `ring`, `lra`, or another broad tactic to bypass a disabled
calculus or cleanup rule.

## Phase 4: Rocq proof and export

### 1. Use a pointwise derivative specification

For:

```text
f       = fun x => body
f_prime = fun x => derivative_body
```

export a theorem equivalent to:

```coq
forall x : R,
  derivable_pt_lim
    (fun x : R => body)
    x
    (derivative_body).
```

This directly matches the current derivative certificate architecture.

### 2. Connect the Hazel function result to the certificate

The exported trace must establish that the body of the returned Hazel
function is the derivative certified at every input. If function equality is
needed, use pointwise equality and functional extensionality deliberately;
do not rely on syntactic equality of differently named binders.

### 3. Preserve alpha-equivalence

Rocq generation may choose stable fresh names independently from Hazel's
display names. The proof must be invariant under:

```text
fun x -> ...
```

versus:

```text
fun t -> ...
```

### 4. Keep imports and tactics modular

Reuse only the calculus lemmas and cleanup tactics required by the recorded
trace. Do not load a fixed, maximally powerful tactic bundle for every
function-valued derivative.

## Phase 5: Testing

### Static tests

Positive:

```text
let f(x) = x**2 in diff(f)
let f(x) = x**2 in diff(f)(5.0)
diff(fun t -> sin(t))
```

Negative:

```text
diff(3.0)
diff(fun (x, y) -> x + y)
diff(fun x -> "hello")
```

Regression:

- The explicit `diff(expression, x)` form still typechecks.
- A non-variable second argument is still rejected.

### Binder and capture tests

Use structurally different cases:

```text
let f(t) = t**2 + 3*t + 5 in diff(f)
```

```text
let a = 3 in
let f(x) = a*x in
diff(f)
```

```text
let x = 10 in
let f(t) = t + x in
diff(f)
```

```text
diff(fun x -> (fun x -> x)(x))
```

```text
let x_prime = 7 in
diff(fun x -> x + x_prime)
```

Assertions:

- No new free variables are introduced.
- Nested shadowing remains intact.
- Renaming the source parameter does not change the result modulo
  alpha-equivalence.
- Applying the derivative function substitutes its own parameter, not a free
  variable from the original body.

### Profile tests

For power, product, and trigonometric functions:

- required rule enabled: accepted;
- required rule disabled: rejected or left residual;
- basic derivatives enabled/disabled;
- identity powers enabled/disabled;
- cleanup toggles persist across every returned-function step.

Include One Step, Check Result, and Auto simplify tests.

## Deferred: Rocq Export Across Hazel Programming Constructs

Do not make the first version prove one monolithic theorem about the entire
Hazel program containing `let`, function application, and evaluation steps.
That would couple the math exporter to substantially more of Hazel's dynamic
semantics than this feature needs.

The preferred follow-up design is compositional:

1. Export an independent Rocq theorem or certificate for each exportable
   mathematical transition selected in the stepper.
2. Keep `let` substitution, function application, and other programming
   transitions in the Hazel derivation trace unless a separate semantics
   exporter is explicitly introduced.
3. Attach enough source/target metadata to relate each small Rocq theorem back
   to its Hazel row without requiring a single theorem over the whole program.
4. Decide later whether those certificates should be packaged into one `.v`
   file, linked by generated lemmas, or left as independent artifacts.

This preserves useful proof export now while deferring the larger research
question of certifying Hazel evaluation and binding constructs in Rocq.

### Rocq tests

Compile exported programs for:

```text
fun x -> x**2 + 3*x + 5
fun t -> sin(t)**2
fun x -> (x + 1)*(x - 2)
fun x -> 1/(x + 1)
```

The quotient case must include its nonzero hypothesis. Also test renamed
parameters and free symbolic coefficients.

### Browser test

Run the complete user flow:

```text
let f(x) = x**2 + 3*x + 5 in
let f_prime = diff(f) in
f_prime(5)
```

Verify:

1. `diff(f)` becomes a function.
2. The parameter remains bound throughout the trace.
3. No parameter is marked as unknown/free.
4. The final result is `13`.
5. Profile-disabled variants retain the expected verbose derivative.
6. Rocq validation and exported `.v` compilation both pass.

## Recommended Commit Sequence

1. **Statics:** type unary `diff` as function-to-function and add static tests.
2. **Binding utility:** add capture-safe expression alpha-renaming with focused
   tests.
3. **Rewrite:** add `calc.diff_function_value` and reuse existing body rules.
4. **Stepper:** preserve local statics, traces, Profile behavior, and Replace.
5. **Rocq:** add pointwise function-derivative certificates and compilation
   tests.
6. **Browser regression:** exercise the complete stored-and-applied derivative
   workflow.

Each commit should keep the explicit two-argument derivative form working.

## Acceptance Criteria

The feature is complete when:

- the dream program evaluates to `13`;
- the derivative result is a reusable function;
- parameter renaming and surrounding scopes cannot change its meaning;
- the result contains no accidentally free function parameter;
- all derivative and cleanup behavior respects the active Profile;
- One Step, Check Result, Auto simplify, and Replace share the same recorded
  rule trace;
- the corresponding Rocq export compiles;
- disabled-rule and capture-negative tests fail safely;
- no example-specific recognizer is used.
