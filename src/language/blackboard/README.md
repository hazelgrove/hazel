# Blackboard kernel (M0)

The core logic of *Blackboard: A Clean Slate Proof Assistant*, as a small
OCaml library with no dependence on tiles or the web layer. Specification:
the author's corrected Figure 1, mechanized in [`../../../mech/Blackboard.v`](../../../mech/Blackboard.v).

| Module | Role |
|---|---|
| `BbTerm` | Terms `x`, `t : T`, `type`, `(x : T₁) → T₂`, `t₁ t₂` with named binders; signatures, blocks, documents; substitution, α-equivalence, printing |
| `BbCheck` | The decidable fragment the paper calls `tychk` (l. 154–156), without conversion: `synth`, `has_type`, `is_type`; `check_signature`, `check_doc` |
| `BbError` | Errors as data: `Unbound`, `NotAType`, `NotAFunction`, `ArgumentMismatch`; located by signature entry |
| `BbParse` | The paper's concrete syntax, line-based documents with `assume`/`construct … by tactic` |

Every step of the checker is an instance of a rule of Figure 1 (`hyp`,
`type`, `ap`, `arrow`, `in`, and `in-` for hypotheses), so what passes is
derivable. Two Blackboard-specific readings matter: `(t : T) : type` needs
only that `T` is a type and `t` has *some* type (l. 80); and a hypothesis
`h : (t : T)` in the context establishes `t : T`, which the refinement and
intersection eliminators of Section 4 rely on.

Not here yet: implicit arguments, user `syntax`, tactics, equations used as
rewriting. Construct blocks have their signatures checked and their `by`
obligation reported as pending.

Tests: `test/blackboard/Test_Blackboard.re`, using the paper's signatures
as fixtures. The checker finds the paper's two slips in `cong-ap` and the
two in `int-elim`, and reports where the paper's examples need equations
the fragment cannot use.

## The editor sort (M1)

`Bb(BbSort.Term)` is a nested sort in `Sort.t`, following the ALFA
derivation sub-language (`src/language/derivation/`). One sort covers
terms, signature entries and blocks; the distinctions are made by form,
as `DrvSort` does for judgments and propositions.

| Module | Role |
|---|---|
| `BbSort` | the sub-sort enum |
| `BbGrammar` | the shape of editor terms, polymorphic in the annotation |
| `BbTermBase` | the same at `IdTagged`, i.e. with ids |
| `Bb` | classes for the cursor inspector, and the reading of editor terms as kernel terms, entries, blocks and documents |

Concrete syntax, as molded by `Form.bb_get`:

```
x                       a name
type                    the type of types
t : T                   membership
A -> B                  function type
(x : A) -> B            dependent function type: a parenthesized membership
f(a)   f(a, b)          application, curried
e1; e2                  signature entries, or blocks
assume <entries> by <tactic>
construct <entries> by <tactic>
blackboard <document> end        embeds a document in an expression
```

Two things are worth knowing:

- **The colon binds tighter than the arrow**, so that `(x : A) -> x : B`
  reads as the paper writes it. In a signature entry the colon is a
  declaration instead, and is loosest; `Bb.rotate_entry` rotates the
  spine back, so `f : A -> B` declares `f` of type `A -> B`.
- **Blackboard is a closed sub-language.** `Insert.effective_sort` does
  not fall back to the enclosing sort inside it, so `type` stays a
  Blackboard token instead of expanding into Hazel's `type _ = _ in`.

Statics and the cursor inspector are M2: today a `blackboard ... end`
expression is inert, with an unknown type, and the checker runs only
from tests.

Documentation slides live in `hazel-programs/docs/blackboard/` and are
registered in `src/docslides/Slides.re` under `Blackboard / …`. They
load through the typing parser, since the menhir fast path has no
Blackboard productions.
