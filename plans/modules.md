# Hazel Modules: Remaining Plan

See `docs/modules.md` for documentation of what's implemented.
See `plans/modules-phase-1.md` for Phase 1 historical implementation notes.

---

## Phase 2: Separating from Labeled Tuples (Statics & Dynamics)

Currently modules are a syntactic gloss over labeled tuples — they infer Prod types and evaluate as labeled tuple values. Phase 2 gives modules their own type identity and eventually their own runtime representation.

### Phase 2.1: Proper Sig Types

Make Sig a first-class type rather than desugaring to Prod.

#### What's Done (Phase 1.5A: Sig Sort Concrete Syntax)

- Sig sort infrastructure: Sort.re, Grammar.re, Form.re, Segment.re, Skel.re, Arms.re, Insert.re, MakeTerm.re
- Sig as a Typ variant in Grammar.re (`Sig(list(sig_t))`)
- `desugar_sig` function in Typ.re (Sig -> Prod conversion for current statics)
- Type-directed module expansion for error attribution
- Cursor inspector integration (Mod/Sig cls, colors, keyword bolding)
- ExpToSegment: full Module and Sig pretty-printing with whitespace preservation
- Sig `pretty_print` in Typ.re renders `{ let x : Int }` syntax
- Module abbreviation in Abbreviate.re (recursive item abbreviation)
- Menhir parser: Sig syntax supported (AST.re, Parser.mly, Conversion.re)
- All 1502 tests pass

#### What Remains

1. **Sig `meet` semantics** (Typ.re): Add `(Sig, Sig)` consistency checking.
   - **Recommended: open meet** — shared members must have consistent types, extra members on either side are fine.
   - `meet({x:Int, y:Bool}, {x:Int}) = {x:Int, y:Bool}` (OK)
   - `meet({x:Int}, {x:Bool}) = None` (x inconsistent)
   - `meet({x:Int}, {y:Bool}) = {x:Int, y:Bool}` (no overlap, both fine)
   - Open meet alone doesn't catch missing members — that's handled by explicit module-vs-sig checking (item 3).

2. **Sig-Prod cross-compatibility**: Decision needed — should `{ let x : Int }` and `(x=Int)` be compatible types? Affects migration path.

3. **Module inference produces Sig types** (Statics.re): The core semantic change.
   - **Approach**: Still use expansion internally for item-level type-checking, but build a Sig type from the items rather than using the expanded Prod type.
   - Expand with `~ana=Syn` (synthesize) to get item types without annotation influence.
   - `build_sig_from_items(items, statics_map)`: Walk items, look up inferred types, create `SigLet(Asc(pat, inferred_type))` entries. Skip ModExp (bare expressions don't contribute to type). Handle shadowing (only last binding per name).
   - `check_module_against_sig(sig_type, ana)`: When ana is Sig, verify each expected entry has a matching module entry with consistent type. Missing entries -> Inconsistent error. Extra entries -> OK (width flexibility).
   - Co-context comes from expanded expression unchanged.

4. **Dot access for Sig types** (Statics.re): Add `Sig(entries)` case to Dot handler.
   - Structurally similar to Prod case: look up label name, return type or `LabelNotFound`.
   - Different lookup: `sig_lookup_val(entries, name)` finds SigLet with matching pat variable (vs `LabeledTuple.find_label` for Prod).
   - Runtime dot access needs no changes — modules are still labeled tuple values from expansion.

5. **Sig type statics checking**: Duplicate value/type names, context threading for type members.
   - Reuse `DuplicateLabels` error pattern from Prod.
   - SigType entries add to context for subsequent entries: `{ type T = Int; let x : T }` — T available when checking x.

6. **Remove `desugar_sig`**: Once Sig types handled natively, remove conversion.

7. **Sig display in cursor inspector**: Partially done — `Typ.pretty_print` and ExpToSegment render Sig properly. But modules still infer Prod types, so the inspector shows `(x=Int, y=Bool)` until item 3 is done.

8. **Error types**: All module errors map to existing error types, no new variants needed:
   - Duplicate entries in sig -> `DuplicateLabels` (typ error)
   - Missing member -> `Inconsistent(Expectation({ana, syn}))` (exp error)
   - Type mismatch -> `Inconsistent(Expectation({ana, syn}))` (exp error)
   - Dot member not found -> `LabelNotFound` (exp error)

9. **Tests**: Module type inference, annotation checking, dot access with Sig types, duplicate entries, width flexibility.

#### Implementation Order

Recommended order (critical path: 1 -> 3 -> 4):

1. Sig `meet` semantics (item 1)
2. Sig statics checking (item 5) — can be done in parallel with 1
3. Module inference produces Sig types (item 3) — depends on 1
4. Dot access for Sig types (item 4) — depends on 3
5. Remove `desugar_sig` (item 6) — after 3 and 4 verified working
6. Tests throughout (item 9)

### Phase 2.2: Type Member Access (`M.T`)

Allow `M.T` to access type members defined in module `M`. Requires:
- Type-level dot accessor in the type syntax
- Module types that track type members
- More sophisticated type lookup

### Phase 2.3: Custom Evaluation for Modules

Instead of expanding to labeled tuples at runtime, evaluate modules directly:
- `Module(items)` as a runtime value
- Dot accessor that projects from module values
- More efficient for large modules

---

## Phase 3: Extensions (Undecided)

Features that seem reasonably straightforward but we haven't decided whether to pursue.

### Phase 3.1: Capitalized Module Names

Currently only lowercase names work (`let m = { ... }`). Capitalized identifiers are parsed as constructors.

Design options (see `plans/modules-phase-1.md` section "Capitalized Module Names" for full analysis):
1. **Status quo**: Keep lowercase (current)
2. **Unify Var/Constructor**: Single `Name(string)`, statics resolves from context
3. **Extend `is_var`**: Make `is_var` match capitalized too
4. **Bidirectional resolution**: Use capitalization + scope as hints
5. **Module-specific handling**: Special cases for module patterns/expressions

Key insight: any solution MUST address both the pattern/binding side AND the expression/reference side. Recommendation: Option 2 or 3.

### Phase 3.2: `module` Keyword

`module M = { ... } in ...` as syntax sugar for `let M = { ... } in ...`. No semantic changes needed — just a new form that parses to the same term structure.

### Phase 3.3: Pattern Destructuring for Modules

`let { x, y } = m` — extract fields from modules into scope. Design questions:
- What syntax? `let { x, y } = m`? Or `let { let x; let y } = m`?
- How does it interact with type members?
- Is it just sugar for multiple dot accesses?

Note: After Phase 2.1, modules have Sig types, so `let (x=a) = m` (which works now via Prod types) will stop working. Pattern destructuring becomes the replacement.

### Phase 3.4: Abstract Types / Sealing

Full ML module system feature:
- Signatures can hide type implementations: `type T` without `= Typ`
- Sealing: `M :> Sig` hides internals
- Requires opaque type variables

---

## Menhir Parser: Semicolon Ambiguity

Multi-item modules fail in Menhir round-trip tests due to `;` being both an expression operator (Seq) and module item separator. The tile editor handles this via sort-aware insertion. Statics tests work because Menhir does parse multi-item modules — the conflict only causes issues for the MakeTerm-equivalence round-trip tests. 2 evaluator tests are skipped because of this.

Sig syntax IS now supported in Menhir (AST.re, Parser.mly, Conversion.re). Five Sig round-trip tests pass.

Potential fixes (see `plans/modules-phase-1.md` for full analysis):
1. Grammar duplication (separate `expNoSeq`)
2. GLR parsing
3. Lexer hack (different semicolon tokens)
4. Post-processing
5. Remove expression `;`

---

## Design Decisions Log

### Decided

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Module representation | Syntactic gloss over labeled tuples | Reuses existing infrastructure |
| Module sort | Dedicated `Mod` sort | Sort-aware insertion enables `{` disambiguation |
| Sig sort | Dedicated `Sig` sort | Parallel to Mod, enables `{` in Typ context |
| Bare expressions in modules | Allowed (wrapped as ModExp) | Useful for tests and side effects |
| Mod->Exp fallback | Yes | Bare expressions need Exp forms in Mod context |
| Sig->Typ fallback | Yes | Robustness for partial editing |
| Semicolon preference | Mod/Sig parent -> ModSeq/SigSeq | Intuitive "done with this item" semantics |
| SigLet form | Single delimiter `let`, Pat body handles `:` | Reuses existing Typeann form |
| Module names | Lowercase only (Phase 1) | Avoids constructor ambiguity |
| Sig meet semantics | Open (recommended) | Width flexibility for annotations |

### Open

| Decision | Options | Notes |
|----------|---------|-------|
| Sig-Prod compatibility | Same type family vs separate | Affects migration path from current Prod types |
| Width subtyping | In `meet` vs separate check | Deferred pending experience |
| Capitalized names | See Phase 3.1 | Requires both Pat and Exp side changes |
| `module` keyword | Add vs skip | Pure syntax sugar, low priority |
| Pattern destructuring | Syntax and semantics TBD | Needed after modules have Sig types |
| Abstract types | Scope and timeline TBD | Eventually needed |
