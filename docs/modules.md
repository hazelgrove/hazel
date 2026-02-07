# Hazel Module System

## Overview

Hazel's module system provides ML-style module syntax as a **syntactic gloss over labeled tuples**. Modules use curly-brace syntax with `let` and `type` declarations, but are semantically equivalent to nested definitions producing a labeled tuple value. Module type annotations use **signature syntax** (`Sig` sort) in type position.

## User-Facing Syntax

### Module Expressions

```
{ let x = 1; let y = true }           -- basic module
{ type T = Int; let x : T = 5 }       -- with type alias
{ let a = 1; test a == 1 end }        -- with side-effect expression
{ let m = { let x = 1 }; let y = m.x } -- nested modules
{}                                      -- empty module
```

### Module Type Annotations (Signatures)

```
let m : { let x : Int; let y : Bool } = { let x = 1; let y = true }
let m : { type T = Int; let x : T }   = { type T = Int; let x = 1 }
type MSig = { let x : Int }
let m : MSig = { let x = 1 }
```

### Field Access

```
let m = { let x = 1; let y = 2 } in m.x   -- evaluates to 1
```

### What Works

| Feature | Status |
|---------|--------|
| Module syntax (`{ let ... }`) | Works |
| Type inference (labeled tuple types) | Works |
| Field access via `.` | Works |
| Type aliases inside modules | Works |
| Bare expressions (side effects) | Works |
| Shadowing (last binding wins) | Works |
| Nested modules | Works |
| Signature syntax in type annotations | Works |
| Type-directed error attribution | Works |
| Cursor inspector for Mod/Sig sorts | Works |

### Known Limitations

- **Menhir multi-item modules**: `{ let x = 1; let y = 2 }` fails in the Menhir parser due to a shift-reduce conflict between expression-level `;` (Seq) and module `;` (ModSeq). The tile editor handles this correctly. 2 evaluator tests are skipped because of this.
- **Modules infer Prod types, not Sig types**: Modules currently infer labeled tuple types (`(x=Int, y=Bool)`), not signature types (`{ let x : Int; let y : Bool }`). Sig annotations are desugared to labeled tuples before type checking.
- **No width subtyping**: `let m : { let x : Int } = { let x = 1; let y = 2 }` doesn't restrict `m`'s type to just `{ let x : Int }`. The annotation is desugared to `(x=Int)` which requires exact match.
- **Lowercase module names only**: Capitalized identifiers are parsed as constructors. See `plans/modules.md` for design options.

---

## Architecture

### Sorts

Two sorts were added for the module system:

- **Mod** (`Sort.Mod`): Module items — `let x = 1`, `type T = Int`, bare expressions
- **Sig** (`Sort.Sig`): Signature items — `let x : Int`, `type T = Int`

Both follow the established sort patterns with forms, remolding, and MakeTerm parsing.

### Expansion Model

Modules are a **syntactic sugar**. They are expanded to standard Hazel expressions:

```
{ let a = 1; let b = 2 }
  -->  let a = 1 in let b = 2 in (a=a, b=b)
```

Expansion happens in two places:
1. **Statics** (`Statics.re`): On-demand expansion for type checking
2. **Elaborator** (`Elaborator.re`): Permanent expansion for evaluation

The expanded form uses nested `let`/`type` bindings with a final labeled tuple containing non-shadowed bindings.

### Type-Directed Expansion

When a module has a type annotation (ana type is a labeled tuple), the expansion adds type annotations to `let` patterns for proper error attribution:

```
let m : { let x : Int } = { let x = true }
  -- Expansion with ana=(x=Int):
  -- let (x : Int) = true in (x=x)
  -- Error appears on `true` (type mismatch: Bool vs Int)
```

Without this, type errors would appear on the synthetic tuple node which has no surface representation, making them invisible to the user.

### Sig Desugaring

Sig types in annotations are desugared to labeled tuples via `Typ.desugar_sig`:

```
{ let x : Int; let y : Bool }  -->  (x=Int, y=Bool)
```

This is a targeted transformation that only converts Sig nodes to Prod, preserving Parens and other type structure. It replaces `Typ.normalize` in the Asc cases to avoid stripping Parens wrappers from non-Sig type annotations.

### ID Preservation

Module expansion carefully preserves tile IDs for cursor inspector integration:

- **Curly brace + semicolon IDs** → Module expression annotation (absorbed via `adopted_ids`)
- **ModLet/ModType tile IDs** → Expanded `Let`/`TyAlias` expressions (via `IdTagged.fast_copy`)
- **Synthetic tuple** → Fresh IDs (no surface counterpart)

This ensures clicking on any part of a module shows correct type information in the cursor inspector.

### Cursor Inspector

- **Mod items**: Show as "Let definition", "Type alias definition" etc. (Mod cls, not Exp cls)
- **Sig items**: Show as "Let signature", "Type alias signature" etc. (Sig cls via Secondary info)
- **Sort colors**: Both Mod and Sig have dedicated colors in the cursor inspector header, gamma icon, toggle switch, and dividers

---

## Key Files

### Core Implementation

| File | Purpose |
|------|---------|
| `src/language/term/Sort.re` | Sort enum with `Mod` and `Sig` |
| `src/language/term/Grammar.re` | `mod_term`, `sig_term`, `Module` in exp_term, `Sig` in typ_term |
| `src/language/term/Mod.re` | Mod term utilities |
| `src/language/term/Sig.re` | Sig term utilities |
| `src/language/term/Cls.re` | `Mod(Mod.cls)` and `Sig(Sig.cls)` variants for cursor inspector |
| `src/haz3lcore/lang/Form.re` | Module/Sig forms, `mk_pre_c'` helper |
| `src/haz3lcore/tiles/Mold.re` | `mk_pre'` for heterogeneous prefix forms |
| `src/haz3lcore/tiles/Segment.re` | `remold_mod`/`remold_sig` with fallback patterns |
| `src/haz3lcore/tiles/Skel.re` | ModSeq/SigSeq semicolons chainable |
| `src/haz3lcore/zipper/action/Insert.re` | `effective_sort` with Mod→Exp / Sig→Typ fallback |
| `src/haz3lcore/lang/MakeTerm.re` | Module/Sig parsing with flattening |
| `src/language/statics/ExpandModule.re` | Module expansion to nested let/type + labeled tuple |
| `src/language/statics/Statics.re` | Module type checking, `desugar_sig` in Asc, Mod/Sig item info |
| `src/language/statics/Elaborator.re` | Module elaboration for dynamics |
| `src/language/statics/Info.re` | `sort_of` returns Mod for InfoExp with Mod cls |
| `src/language/term/Typ.re` | `desugar_sig` function |
| `src/haz3lcore/pretty/ExpToSegment.re` | Module and Sig pretty-printing to segments |
| `src/language/term/Abbreviate.re` | Module abbreviation for probe display |

### CSS

| File | What |
|------|------|
| `src/web/www/style/variables.css` | `--token-mod`, `--token-sig`, `--shard-mod`, `--shard-sig` color variables |
| `src/web/www/style/editor.css` | `.child-line.Mod`, `.child-line.Sig`, keyword bolding |
| `src/web/www/style/cursor-inspector.css` | Mod/Sig gamma, toggle-switch, header, divider colors |

### Tests

| File | What |
|------|------|
| `test/statics/Test_Statics_Modules.re` | 14 statics tests |
| `test/evaluator/Test_Evaluator_Modules.re` | 11 evaluator tests (2 skipped for Menhir) |
| `test/Test_MakeTerm.re` | Module parsing tests including nested modules |
| `test/Test_Elaboration.re` | 4 module elaboration tests (module → labeled tuple) |
| `test/Test_ExpToSegment.re` | 6 module/sig roundtrip tests + 1 skipped (empty module structural diff) |
| `test/Test_Editing.re` | 4 module editing tests (brace insertion, let inside module) |
| `test/Test_Abbreviate.re` | 2 module abbreviation tests |
| `test/Test_Menhir.re` | 5 Sig round-trip tests |

---

## Technical Details

### Heterogeneous Prefix Forms (`mk_pre'` / `mk_pre_c'`)

ModLet, ModType, SigLet, and SigType need different out vs body sorts. For example, ModLet's out sort is Mod (the form is a module item) but its body sort is Exp (what comes after `=`). Standard `mk_pre` gives both nibs the same sort. `mk_pre'` in `Mold.re` and `mk_pre_c'` in `Form.re` allow heterogeneous sorts.

### Sort Fallback Patterns

**Mod→Exp**: Bare expressions are valid module items, so when in Mod context and no Mod form exists, try Exp. This affects remolding (`remold_mod`) and expansion (`effective_sort` in `Insert.re`).

**Sig→Typ**: Parallel fallback for signatures. Bare types in sig context fall back to Typ sort for robustness.

**Semicolon special case**: When `;` is typed in Mod/Sig context, prefer ModSeq/SigSeq over CellJoin (Exp-sort `;`).

### Singleton Labeled Tuple Bug Fix

Var patterns analyzed against singleton labeled tuple types (e.g., `(y=Int)`) were incorrectly elaborated. Fixed by checking if the pattern name matches the label before elaborating:
- `let a = (a=1) in a` → elaborate (destructuring) → `a : Int`
- `let m = (y=1) in m` → don't elaborate → `m : (y=Int)`

### Module Semicolon Decoration

In `Arms.re`, module/sig semicolons render as lone shard hexagons (no arms to other pieces). Module/sig curly braces render as a pair with arm between them, filtering out semicolons.
