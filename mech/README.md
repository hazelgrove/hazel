# Mechanized metatheory of Blackboard's core logic

`Blackboard.v` is a Rocq development about the core logic of *Blackboard: A
Clean Slate Proof Assistant* (Figure 1), in both the form first printed and
the author's corrected form, where the rule `arrow+` carries the premise
`Γ ⊢ T₁ : type`. It is the reference for the checker in
`src/language/blackboard/`: whatever the checker accepts must be derivable
here, and the models here say what is *not* derivable.

Checked with Rocq 9.2; `compile-output.txt` is the output of

    rocq compile Blackboard.v

The eight `Closed under the global context` lines are `Print Assumptions`
for the main theorems: no axioms are used.

## Contents

| Part | What | Main names |
|---|---|---|
| I | Syntax with de Bruijn indices; the eight rules, parametric in a flag `s` that selects the corrected (`s = true`) or printed (`s = false`) `arrow+`; a two-point model; **Theorem 1**: the bare logic is consistent, `(X : type) → X` is not derivable | `derivable`, `soundness`, `consistency` |
| II | Substitution algebra; renaming of derivations; weakening; **Theorem 2** for one declaration | `derivable_rename`, `weakening`, `conservativity1` |
| III | **Theorem 2** in general: a construction block with signature `x₁ : T₁, …, xₙ : Tₙ` whose obligation `(M : type) → ((x₁ : T₁) → … → M) → M` is met is a conservative extension, for goals that are types | `witness`, `conservativity` |
| IV | A five-point model with types, an empty type, an inhabited non-type and junk; the printed rules derive `(x : type type) → type` but not its formation, so they are not regular; in this model the type of `eq` forces every equation to hold and the type of `replace` is uninhabited, so the equality block needs genuine functions | `soundness5`, `printed_rules_not_regular`, `replace_ty_uninhabited_5pt` |

After the section, `corrected` and `printed` name the two systems and
`corrected_printed` shows the first is included in the second.

## What is open

Consistency of the Section 4 postulate library (equality with `replace`,
refinement, intersection, quotients, the small universe with combinators).
The obstacle is `type : type`: the extension of `(X : type) → X` is defined
in terms of itself, so the usual PER models, which need stratified
universes, do not apply directly. Candidate routes are a coinductive
extension relation with a functionality proof, or a stratification theorem
in the style of Harper and Pollack's typical ambiguity.
