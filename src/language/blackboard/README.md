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

Plan for the editor integration (sort, forms, statics, inspector) follows
the ALFA derivation sub-language (`src/language/derivation/`).
