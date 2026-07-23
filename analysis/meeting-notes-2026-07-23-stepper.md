# Stepper Meeting Notes

**Date:** July 23, 2026  
**Participants:** Nishant, Cyrus, and Matt  
**Topic:** Mathematical stepper progress, function-level differentiation, and research direction

## Progress Presented

### Differentiation quality-of-life improvements

The calculus stepper now supports configurable cleanup of common derivative
artifacts, including:

- `diff(x, x) → 1`
- `diff(constant, x) → 0`
- `x**1 → x`
- `x**0 → 1`

These cleanup policies can be enabled or disabled through the active math
Profile. Turning them off exposes the more verbose intermediate form of a
derivative; turning them on produces the familiar simplified result.

One remaining interoperability issue is that Algebrite does not handle
expressions containing unresolved derivative applications especially well,
such as:

```text
diff(x**2, x) + diff(3*x, x) + diff(5, x)
```

Hazel should therefore avoid depending on Algebrite to interpret or simplify
partially evaluated derivative expressions.

### Storing derivatives as values

We discussed several possible ways to define an expression, derive it, and
store the result.

#### Store an expression and its derivative

```text
let f = x**2 + 3*x + 5 in
let f_prime = diff(f, x) in
f_prime
```

This is close to Hazel's existing expression-oriented syntax, but `f` is an
expression with a free variable rather than a first-class function.

#### State a derivative as a theorem

```text
let f = x**2 + 3*x + 5 in
theorem f_prime = 2*x + 3 == diff(f, x) in
f_prime
```

This records the expected derivative as a proposition and asks Hazel to retain
its proof.

The same pattern could express higher derivatives:

```text
let f = x**2 + 3*x + 5 in
theorem f_prime = 2*x + 3 == diff(f, x) in
theorem f_2prime = 2 == diff(diff(f, x), x) in
f_prime
```

The second derivative syntax needs further design work. In particular,
`diff(diff(f, x))` lacks an explicit differentiation variable under the
current interface.

#### Explore a derivative and store the result

```text
let f = x**2 + 3*x + 5 in
let f_prime = diff(f, x) in
explore f_prime end
let f_2prime = diff(f_prime, x) in
f_prime
```

This motivates plumbing the result of an `explore` block back into the
surrounding program so that the completed derivation becomes a reusable value.

#### Define `f` as a first-class function

```text
let f(x) = x**2 + 3*x + 5 in
theorem f_prime = 2*x + 3 == diff(f, x) in
f_prime
```

The preferred long-term design is closer to:

```text
let f(x) = x**2 + 3*x + 5 in
let f_prime = diff(f) in
f_prime(5)
```

In this design:

- `f` is a function rather than an expression with a free variable.
- `diff(f)` returns a new function representing the derivative of `f`.
- The differentiation variable is inferred from the function parameter, so
  `diff` no longer needs a second argument.
- The resulting derivative can be evaluated normally, such as `f_prime(5)`.

This is the clearest user-facing goal, although the prover and export layers
may require additional machinery to represent the resulting function and its
derivative certificate.

### FOIL and polynomial expansion

We demonstrated polynomial multiplication with examples such as:

```text
(x + 2) * (x - 1)
```

Two Profile controls are especially relevant:

- **Allow repeated distribution in one step** determines whether a single
  step may apply distribution repeatedly and expand the whole product.
- **Collect like terms** determines whether terms produced by distribution
  may be combined automatically.

Together, these controls support both introductory, highly visible
distribution steps and more concise polynomial expansion.

## Technical Directions

### A differentiation projector

One proposed interface is a differentiation projector such as:

```text
^^diff
```

The result would be a function representing the derivative of the selected
function.

A corresponding proof projection could expose the certificate:

```text
^^diff_proof
```

The value projection and proof projection should refer to the same derivative
operation:

- `^^diff` returns the computed derivative function.
- `^^diff_proof` returns evidence that this function is the derivative of the
  original function.

This direction is currently blocked on splice support. A first version may not
be able to inject the computed expression or proof back into the surrounding
program.

### Returning results from `explore`

The stepper currently supports interactively exploring an expression, but the
result should eventually be reusable outside that interaction. The desired
flow is:

1. Enter an `explore` block with an expression such as `diff(f)`.
2. Complete the derivation in the stepper.
3. Return the final expression as a value.
4. Store and use that value elsewhere in the Hazel program.
5. Retain the associated proof object for checking and export.

### Integration with statics

Supporting reusable derivative values requires integration with the stepper's
static semantics. Hazel needs to know:

- that the input to `diff` is a function;
- the input and output types of that function;
- that `diff(f)` produces another function with the appropriate type;
- which parameter is the differentiation variable;
- how the resulting function and proof enter the surrounding context.

## Short-Term Product Direction

### Instructor-controlled exercises

A promising near-term deliverable is an instructor-facing exercise mode in
which an instructor can:

- write a mathematical goal or homework problem;
- select and lock a math level or custom Profile;
- choose which visible rewrites and cleanup operations students may use;
- determine whether students see introductory or concise steps;
- require Hazel and Rocq-checkable derivations.

This gives instructor control without requiring the full first-class
derivative-function architecture immediately.

### Case studies at multiple levels

Case studies should demonstrate how the same stepper can be configured for
different audiences and learning objectives. Candidate examples include:

- introductory algebra and FOIL;
- trigonometric identities;
- differentiation;
- engineering calculations, including material from AERO 350;
- more advanced custom Profiles that expose or hide selected mathematical
  rules.

## Research Framing

### Overall goal

The research goal is to support mathematical reasoning beyond the formal
verification of functional programs. The work should be positioned relative
to Matt's paper on Prover while emphasizing the extension into
student-facing, configurable mathematical derivations.

### Incremental contributions

Potential contributions include:

- extending Hazel's stepper to support common mathematical domains;
- configurable, instructor-controlled proof Profiles;
- progressively detailed derivations for different learning stages;
- exact proof replay and Rocq export;
- connecting interactive derivations to reusable program values;
- practical techniques for combining symbolic computation, tactic search, and
  structured proof traces.

Principled, clearly scoped implementation techniques are acceptable even when
they do not immediately generalize to every part of the larger Hazel
architecture.

### Case-study structure

Each case study should document:

1. **Learning objectives:** What should a student understand or practice?
2. **Profile configuration:** Which rules, cleanup policies, and proof methods
   are enabled?
3. **System implementation:** What was added to Hazel and how is the prover
   configured?
4. **Paper proof:** How would the derivation conventionally be written?
5. **Hazel proof:** How does the student construct and check it in the
   stepper?
6. **Rocq evidence:** How is the result independently certified or exported?

### Evaluation

Evaluation should be grounded in specific examples from existing educational
sources rather than synthetic examples alone. Potential sources include:

- Heather's Lean book;
- a standard algebra textbook for FOIL and polynomial manipulation;
- JB's AERO 350 course material;
- other instructor-provided derivations and homework problems.

The goal is to reproduce selected examples as closely as possible in Hazel,
then evaluate:

- whether the intended steps can be expressed;
- whether the Profile captures the instructor's intended boundaries;
- whether students can work at an appropriate level of detail;
- whether the result can be checked and exported reliably.

## Open Questions

- Should differentiation primarily operate on expressions with an explicit
  variable, or on first-class functions?
- What concrete Hazel syntax should define parameterized functions:
  `let f(x) = ...`, `let f = fun x -> ...`, or both?
- How should higher derivatives be written?
- Can a derivative function and its proof be generated together as a single
  structured result?
- What splice support is necessary to return an `explore` result to the
  surrounding program?
- How should the stepper statics represent and type stored mathematical
  results?
- Which parts of Profile configuration should instructors be allowed to lock
  per exercise?
- Which case studies provide the strongest evidence for the research claims?

## Proposed Next Steps

1. Define the desired user-facing semantics of `diff(f)` for a first-class
   unary function.
2. Prototype the `^^diff` and `^^diff_proof` projector interfaces, documenting
   what is blocked by current splice support.
3. Investigate how an `explore` result and its proof can be returned as a
   stored value.
4. Specify the required static-semantics changes for derivative functions.
5. Build a small instructor-controlled exercise/Profile prototype.
6. Select representative source examples for algebra, trigonometry,
   calculus, and engineering case studies.
7. For each case study, record its learning objective, Profile, paper proof,
   Hazel derivation, and Rocq proof.
