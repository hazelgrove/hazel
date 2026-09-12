# Blackboard in Hazel

An MVP of **Blackboard: A Clean Slate Proof Assistant** — a core logic with a
type of all types, an internal typing relation, and no lambda — as a
sub-language of Hazel. Branch `blackboard-mvp`, PR
[#2525](https://github.com/hazelgrove/hazel/pull/2525).

This document records what is actually implemented, what is deliberately a
placeholder, and what is open. It was written to answer two questions from the
paper's author, which are the two questions anyone should ask of a proof
assistant MVP: *are the constructions real?* and *how does it typecheck?*

| Where | What |
|---|---|
| `src/language/blackboard/` | the kernel (`BbTerm`, `BbCheck`, `BbError`, `BbParse`) and the editor sort (`BbSort`, `BbGrammar`, `BbTermBase`, `Bb`, `BbInfo`) |
| `src/language/statics/Statics.re` | `bb_to_info_map`: checking as the editor runs it |
| `src/web/blackboard/BbCursorInspector.re` | what the caret says |
| `mech/Blackboard.v` | the Rocq reference semantics |
| `hazel-programs/docs/blackboard/` | six documentation slides |
| `test/blackboard/` | 34 tests |

## What is real

The checker. `BbCheck` is a genuine bidirectional implementation of the
decidable fragment the paper calls `tychk`: `synth` finds a term's type,
`is_type` checks formation, `has_type` compares
([`BbCheck.re:52`](../src/language/blackboard/BbCheck.re#L52)). Every step is
an instance of a rule of the corrected Figure 1 — `hyp`, `type`, `ap`,
`arrow`, `in`, and `in-` for hypotheses — so whatever passes is derivable.
Much that is derivable does not pass, and the paper says so.

It is not a stub, and the evidence is that it finds the paper's own slips:

| Fixture | What the checker reports |
|---|---|
| `cong-ap` as printed | `B : A -> type` used bare where `B a` is meant; `a1` bound twice, leaving `a2` unbound |
| `cong-ap` repaired | exactly one error: `f2 a2 : B a2` against an expected `B a1` — the step needing `a1 = a2` |
| `int-elim` as printed | two mismatches, `h : M x` where `s` and `p` expect an `int` |
| `quot` | `eq-rel` is never defined |
| `arrow-small` | `A : U` is not a type without the coercion the paper leaves implicit |

These are pinned as tests in `test/blackboard/Test_Blackboard.re`, and they run
on every keystroke in the editor, not only from tests.

## What is a placeholder

**`by proof` is still a placeholder, exactly as in the paper.** So is
`by definition`, `by tychk` and `by quotient`. A tactic is parsed as a bare
name ([`Bb.re:221`](../src/language/blackboard/Bb.re#L221), "a tactic is a
name") and kept as a string.

**There is no definitional mechanism at all** — no defined constants, no
unfolding, no δ.

So what a `construct` block does today is: check its signature entries exactly
as an `assume` block's are (each type a type in the context of the entries
before it), put the names in the context, record the tactic, and report it as
`pending` ([`BbCheck.re:149`](../src/language/blackboard/BbCheck.re#L149)),
which nothing discharges. In the editor the only difference from `assume` is
the block tint and a cursor-inspector line reading *"constructed: a
conservative extension, owing a witness"*. **Construct is assume, plus a
colour, plus an IOU.**

What did change against the paper is that the placeholder now has a precise,
machine-checked meaning. `mech/Blackboard.v` Part III writes the obligation of
a construction block as a term:

```
witness(T1 ... Tn)  =  (M : type) -> ((x1 : T1) -> ... -> (xn : Tn) -> M) -> M
```

and `Theorem conservativity` proves that if the signature is well formed and
`G |- witness Ts`, then anything derivable using the block and not mentioning
the `xi` — and that is itself a type — was already derivable. Rocq 9.2,
`Print Assumptions` closed under the global context on all eight main
theorems, no axioms.

So "by proof" now means something exact: **produce a term of that witness
type.** Nothing in the MVP produces or checks one. There is no proof-term
language beyond the signature language itself, and no tactic engine.

## Typing

### Substitution — yes, in one place

The `ap` rule. When `f : (x : A) -> B` and the argument checks, the result
type is `subst(x, a, cod)`
([`BbCheck.re:66`](../src/language/blackboard/BbCheck.re#L66)). Binders are
named, as users write them, so `subst` is capture-avoiding with freshening
([`BbTerm.re:58`](../src/language/blackboard/BbTerm.re#L58)); the de Bruijn
reference semantics is the Rocq development. A capture test is in
`Test_Blackboard`.

### Conversion — there is none

`alpha_eq` ([`BbTerm.re:79`](../src/language/blackboard/BbTerm.re#L79)) is the
checker's only notion of "same type": α-equivalence and nothing more. No β, no
η, no δ. Grepping the kernel for reduce / whnf / normalize returns nothing.

This is deliberate rather than unfinished. The logic as given has **no
reduction**, so conversion has no content to implement. Where the paper's
examples need an *equation*, the checker reports a mismatch rather than
quietly accepting: the repaired `cong-ap` still errors, and that test asserts
the mismatch is the *only* remaining error.

### The one escape hatch is the paper's own

Because typing is internal, a hypothesis `h : (t : T)` in the context
establishes `t : T`. `has_type` looks for an α-matching membership hypothesis
*before* it synthesizes
([`BbCheck.re:81`](../src/language/blackboard/BbCheck.re#L81),
[`:27`](../src/language/blackboard/BbCheck.re#L27)). That is rules `hyp` and
`in-`, and it is what makes the Section 4 refinement and intersection
eliminators check at all.

It is a lookup, not a congruence: it does not close under subterm replacement,
so it is not conversion wearing a hat.

### `(t : T) : type`

Needs only that `T` is a type and that `t` has *some* type (paper, l. 80), not
that `t` has type `T` ([`BbCheck.re:95`](../src/language/blackboard/BbCheck.re#L95)).

## Error localization

Kernel terms carry no editor ids, deliberately: the kernel does not depend on
tiles or the web layer. So errors carry the subterm they are about
(`BbError.subject`), and the statics layer finds the node by **re-reading** —
walk the editor term, convert each node back to a kernel term, mark the first
one that is α-equal to the subject
([`Statics.re:78`](../src/language/statics/Statics.re#L78)). Unbound names are
special-cased: every occurrence of the name in the entry is marked. If nothing
matches, the error lands on the whole entry.

Checking is **per entry, not per document**. An entry that cannot be read at
all still enters the context so its neighbours are still checked — an
all-or-nothing check goes silent exactly when the user has just made a
mistake. `Test_BlackboardStatics` pins this.

Two known weaknesses, neither yet a problem at paper scale:

- Two α-equal occurrences of the same subterm in one entry are
  indistinguishable to that search, and the first one takes the mark.
- The search re-converts at every node, so it is quadratic in entry size.

## Editor integration

`Bb(BbSort.Term)` is a nested sort following the ALFA derivation sub-language
in `src/language/derivation/`. One sort covers terms, signature entries and
blocks; the distinctions are made by form. Concrete syntax, molding and the
`rotate_entry` convention are documented in
[`src/language/blackboard/README.md`](../src/language/blackboard/README.md).

Two facts worth knowing:

- **The colon binds tighter than the arrow**, so `(x : A) -> x : B` reads as
  the paper writes it. In a signature entry the colon is a declaration
  instead, and is loosest; `Bb.rotate_entry` rotates the spine back.
- **Blackboard is a closed sub-language.** `Insert.effective_sort` does not
  fall back to the enclosing sort inside it, so `type` stays a Blackboard
  token instead of expanding into Hazel's `type _ = _ in`.

A `blackboard ... end` expression is **inert** in Hazel: it has type
`Unknown(Internal)`, and it is already a value — it does not evaluate
(`Transition.re`). Its document is still checked
([`Statics.re:637`](../src/language/statics/Statics.re#L637)).

## Testing

```
dune build test/idb_stub.js test/haz3ltest.bc.js --profile dev
bash test/run_node.sh test Blackboard
```

34 tests across four groups: `Blackboard` (kernel), `Blackboard editor`
(reading editor terms), `Blackboard slides` (every shipped slide reads back as
a document), `Blackboard statics` (the messages the cursor inspector shows, so
a silent editor fails here).

## Open questions

### About the logic — for the paper's author

1. **Consistency of the Section 4 postulate library** (equality with
   `replace`, refinement, intersection, quotients, the small universe with
   combinators). Open, and the obstacle is `type : type`: the extension of
   `(X : type) -> X` is defined in terms of itself, so the usual PER models,
   which need stratified universes, do not apply directly. Candidate routes
   are a coinductive extension relation with a functionality proof, or a
   stratification theorem in the style of Harper and Pollack's typical
   ambiguity.
2. **What is a proof term?** Theorem 2 says what `by proof` owes — an
   inhabitant of `witness(T1 ... Tn)`. The logic has no lambda, so the
   language in which such a witness is *written* is not settled by Figure 1.
   Until it is, `by proof` cannot be discharged even in principle.
3. **How do equations get used?** Several of the paper's own examples need an
   equation as a rewrite (`cong-ap`'s conclusion, `int-elim`'s quotient, the
   `U = refine type small` coercion). `replace` is postulated, but nothing
   applies it. Is the intent that the user applies `replace` explicitly, or
   that some decidable layer does it? The first is checkable today; the second
   needs a design.
4. **`by definition` and `by tychk` as tactics.** `tychk` exists as the
   ambient checker but not as a tactic that discharges an obligation. What
   does a tactic *return*, given (2)?
5. **Implicit arguments and user `syntax`.** The paper uses both; the fixtures
   expand braces by hand and write `eq A a b` for `=`. These are elaboration
   questions that interact with the editor's tile grammar, so they should be
   settled on paper first.

### About the implementation

6. **Nothing surfaces the pending obligations.** `BbCheck.check_doc` computes
   `pending` per block, and only the tests read it. A document with ten
   construct blocks should show ten outstanding obligations somewhere — a goal
   panel, or a count in the sidebar. Today the only signal is the block tint.
7. **Conservativity is not checked, and cannot be flagged as unchecked.**
   Since a construct block enters the context exactly as an assume does, a
   document that has "proved" everything by `by proof` is indistinguishable,
   to the checker, from one that postulated it all. Whether that should be a
   warning is a design call.
8. **Localization by structural re-match** (above) should eventually be
   replaced by threading ids through `term_to_kernel`, which means kernel
   terms parameterized by an annotation — the same shape `BbGrammar` already
   has.

### About Hazel integration

9. **`BbQuote` has no dynamics and no type.** It is inert with type
   `Unknown(Internal)`. What *should* a Blackboard document mean as a Hazel
   value — a checked-document record, a list of obligations, a first-class
   signature? This is the question that decides whether Blackboard is embedded
   in Hazel or merely hosted by it.
10. **`BbInfo` keeps its own `status` rather than joining `Mark.t`**, whose
    declaration order is load-bearing for the other sorts. That is a
    deliberate dodge; the sub-languages will keep paying for it.
11. **The menhir fast path has no Blackboard productions.**
    `src/menhirParser/Conversion.re` raises `Failure("BbQuote not supported")`,
    which is caught, and every Blackboard slide falls back to the quadratic
    parser. This is the cost measured in
    [#2541, DocSlides is ~60% of suite wall time](https://github.com/hazelgrove/hazel/issues/2541).
12. **Names cannot contain hyphens**, so slides use `falsity_eq` where the
    paper writes `falsity-eq`, and `false` is a boolean literal rather than a
    name. This is the shared-tokenizer constraint catalogued in
    [#2542, the tokenizer is the binding constraint for sub-language tiles](https://github.com/hazelgrove/hazel/issues/2542).
