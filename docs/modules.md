# Hazel Module System

## Overview

Hazel's module system provides ML-style module syntax as a **syntactic gloss over labeled tuples**. Modules use curly-brace syntax with `let` and `type` declarations, but are semantically equivalent to nested definitions producing a labeled tuple value. Module type annotations use **signature syntax** (`Sig` sort) in type position.

## Syntax

### Module Expressions

```
{ let x = 1; let y = true }           -- basic module
{ type T = Int; let x : T = 5 }       -- with type alias
{ let a = 1; test a == 1 end }        -- with bare expression (side effect)
{ let m = { let x = 1 }; let y = m.x } -- nested modules
{}                                      -- empty module
```

Modules are introduced with curly braces `{ }` containing semicolon-separated items. Each item is one of:

- **`let` binding**: `let pat = exp` — binds a value
- **`type` alias**: `type T = typ` — introduces a type alias
- **Bare expression**: Any expression, useful for `test` assertions and side effects

### Module Type Annotations (Signatures)

Signatures use the same curly-brace syntax in type position:

```
let m : { let x : Int; let y : Bool } = { let x = 1; let y = true }
let m : { type T = Int; let x : T }   = { type T = Int; let x = 1 }
type MSig = { let x : Int }
let m : MSig = { let x = 1 }
```

Signature items use `let name : Type` (note: colon, not equals) for value declarations, and `type T = Type` for type aliases.

### Field Access

```
let m = { let x = 1; let y = 2 } in m.x   -- evaluates to 1
let outer = { let inner = { let a = 42 } }
in outer.inner.a                             -- evaluates to 42
```

Field access uses dot notation, the same as labeled tuple access.

### Shadowing

When multiple bindings use the same name, the last one wins:

```
let m = { let x = 1; let x = 2 } in m.x   -- evaluates to 2
```

Only the final binding for each name is exported in the module's type.

### Sequential References

Later bindings can reference earlier ones in the same module:

```
let m = { let x = 1; let y = x + 1 } in m.y   -- evaluates to 2
```

### Module Keyword

The `module` keyword provides a binding form that supports capitalized names:

```
module M = { let x = 1; let y = 2 } in M.x + M.y   -- evaluates to 3
```

With type annotations:

```
module M : { let x : Int } = { let x = 42 } in M.x
module M : (x=Int) = { let x = 1 } in M.x
```

Inside module bodies:

```
{
  module Inner = { let z = 42 };
  let result = Inner.z
}
```

The `module` keyword introduces a new **MPat** sort for module name patterns, which treats both lowercase and capitalized identifiers as variable bindings (not constructors). MPat supports optional type annotation with `:`.

`ModuleExp(mpat, def, body)` expands to `Let(pat, def, body)` during statics (same expansion model as `Module(items)`). `ModuleMod(mpat, def)` expands to `ModLet(pat, def)` inside module bodies.

## What Works

| Feature                              | Status |
| ------------------------------------ | ------ |
| Module syntax (`{ let ... }`)        | Works  |
| Type inference (labeled tuple types) | Works  |
| Field access via `.`                 | Works  |
| Type aliases inside modules          | Works  |
| Bare expressions (side effects)      | Works  |
| Shadowing (last binding wins)        | Works  |
| Nested modules                       | Works  |
| Sequential references between items  | Works  |
| Signature syntax in type annotations | Works  |
| Type-directed error attribution      | Works  |
| Cursor inspector for Mod/Sig sorts   | Works  |
| Empty module `{}`                    | Works  |
| `module` keyword (`module M = ...`)  | Works  |
| Capitalized module names (`M`, `Io`) | Works  |
| MPat type annotations (`M : Sig`)    | Works  |
| Menhir parser (all module forms)     | Works  |

## Known Limitations

### Modules infer Prod types, not Sig types

Modules currently infer labeled tuple types, not signature types:

```
{ let x = 1 }   -- infers (x=Int), NOT { let x : Int }
```

Sig annotations in type position are desugared to labeled tuples before type checking. This means the cursor inspector shows `(x=Int, y=Bool)` rather than `{ let x : Int; let y : Bool }` for module types.

### No width subtyping

Signatures must exactly match the module's exported bindings:

```
-- This ERRORS (module has extra member y):
let m : { let x : Int } = { let x = 1; let y = 2 }

-- Unlike real ML modules, the signature must be precise.
-- In the full implementation, the above should pass.
```

Similarly, a singleton module doesn't match an empty signature, and a singleton signature doesn't match a multi-member module, even though width subtyping would allow this in a full module system.

### Capitalized names require `module` keyword

Capitalized identifiers are parsed as constructors in normal `let` bindings. Use the `module` keyword for capitalized module names:

```
module M = { let x = 1 } in M.x   -- OK: module keyword treats M as binding
let m = { let x = 1 } in m.x      -- OK: lowercase works with let
let M = { let x = 1 } in M.x      -- WRONG: M parsed as Constructor pattern
```

The statics has a fallback: if a capitalized name isn't a known constructor, it checks variable bindings. So `M.x` works in the body.

### Type declarations in signatures don't work

Sig-level `type T = Int` entries are parsed but ignored during desugaring to labeled tuples. Only `let` entries in signatures contribute to the desugared type. This means:

```
-- This doesn't check T properly:
let m : { type T = Int; let x : T } = { type T = Int; let x = 1 }
```

The `type T = Int` in the signature is simply dropped.

### Labeled tuple edge cases

Because modules are syntactic sugar for labeled tuples, some edge cases from the labeled tuple system leak through:

```
-- No error below, even though it arguably should error:
let m : { let x : Int } = { let x = 1 }
-- m.x works, but m also has type (x=Int) which is a labeled tuple
```

## Architecture

### Sorts

Three sorts were added for the module system:

- **Mod** (`Sort.Mod`): Module items — `let x = 1`, `type T = Int`, bare expressions
- **Sig** (`Sort.Sig`): Signature items — `let x : Int`, `type T = Int`
- **MPat** (`Sort.MPat`): Module name patterns — `M`, `M : { let x : Int }`

All follow the established sort patterns with forms, remolding, and MakeTerm parsing.

### Term Structure

**Module items** (`mod_term` in `Grammar.re`):

- `ModLet(pat, exp)` — let binding
- `ModType(tpat, typ)` — type alias
- `ModuleMod(mpat, exp)` — module keyword binding (`module M = ...`)
- `ModExp(exp)` — bare expression (side effects)
- `Invalid`, `EmptyHole`, `MultiHole` — error cases

**Signature items** (`sig_term` in `Grammar.re`):

- `SigLet(pat)` — value declaration (pattern includes optional `: Type`)
- `SigType(tpat, typ)` — type alias declaration
- `Invalid`, `EmptyHole`, `MultiHole` — error cases

**Module name patterns** (`mpat_term` in `Grammar.re`):

- `Var(string)` — name (lowercase or capitalized)
- `Asc(mpat, typ)` — annotated name (`M : { let x : Int }`)
- `Invalid`, `EmptyHole`, `MultiHole` — error cases

**Expression level**: `Module(list(mod_t))` and `ModuleExp(mpat, exp, exp)` in `exp_term`
**Type level**: `Sig(list(sig_t))` in `typ_term`

### Expansion Model

Modules are **syntactic sugar**. They expand to standard Hazel expressions:

```
{ let a = 1; let b = 2 }
  -->  let a = 1 in let b = 2 in (a=a, b=b)
```

Expansion happens in two places:

1. **Statics** (`Statics.re`): On-demand expansion for type checking
2. **Elaborator** (`Elaborator.re`): Permanent expansion for evaluation

The expanded form uses nested `let`/`type` bindings with a final labeled tuple containing non-shadowed bindings. The expansion is implemented in `ExpandModule.re`.

### Type-Directed Expansion

When a module has a type annotation (ana type is a labeled tuple), the expansion adds type annotations to `let` patterns for proper error attribution:

```
let m : { let x : Int } = { let x = true }
  -- Expansion with ana=(x=Int):
  -- let (x : Int) = true in (x=x)
  -- Error appears on `true` (type mismatch: Bool vs Int)
```

Without this, type errors would appear on the synthetic tuple node which has no surface representation, making them invisible to the user. The `extract_ana_labels` function in `ExpandModule.re` handles stripping Parens and extracting the label-to-type mapping.

### Sig Desugaring

Sig types in annotations are desugared to labeled tuples via `Typ.desugar_sig`:

```
{ let x : Int; let y : Bool }  -->  (x=Int, y=Bool)
```

This is a targeted transformation that only converts Sig nodes to Prod, preserving Parens and other type structure. It replaces `Typ.normalize` in the Asc cases to avoid stripping Parens wrappers from non-Sig type annotations.

### ID Preservation

Module expansion carefully preserves tile IDs for cursor inspector integration:

- **Curly brace + semicolon IDs** → Module expression annotation (absorbed via `adopted_ids` in MakeTerm)
- **ModLet/ModType tile IDs** → Expanded `Let`/`TyAlias` expressions (via `IdTagged.fast_copy`)
- **ModExp (bare expression)** → Fresh IDs (synthetic wrapper, no surface counterpart)
- **Synthetic tuple** → Fresh IDs (no surface counterpart)

This ensures clicking on any part of a module shows correct type information in the cursor inspector. The Elaborator keeps inline expansion (rather than calling `ExpandModule.expand` and then `elaborate`) to avoid ID lookup issues — it constructs wrapper expressions directly with preserved IDs.

### Sort Fallback Patterns

**Mod→Exp**: Bare expressions are valid module items. When in Mod context and no Mod form matches, the system falls back to Exp forms. This affects:

- Remolding (`remold_mod` in `Segment.re`)
- Form expansion (`effective_sort` in `Insert.re`)

**Sig→Typ**: Parallel fallback for signatures. Bare types in sig context fall back to Typ sort for robustness during partial editing.

**Semicolon special case**: When `;` is typed in Mod/Sig context, prefer ModSeq/SigSeq over CellJoin (Exp-sort `;`). This ensures semicolons act as item separators inside modules.

### Heterogeneous Prefix Forms

ModLet, ModType, SigLet, and SigType need different out vs body sorts. For example, ModLet's out sort is Mod (the form is a module item) but its body sort is Exp (what comes after `=`). `mk_pre'` in `Mold.re` and `mk_pre_c'` in `Form.re` support this:

```
ModLet  => mk_pre_c'(L, ["let", "="], P.let_, Mod, [Pat], Exp)   -- out=Mod, body=Exp
ModType => mk_pre_c'(L, ["type", "="], P.let_, Mod, [TPat], Typ) -- out=Mod, body=Typ
SigLet  => mk_pre_c'(L, ["let"], P.let_, Sig, [], Pat)           -- out=Sig, body=Pat
SigType => mk_pre_c'(L, ["type", "="], P.let_, Sig, [TPat], Typ) -- out=Sig, body=Typ
```

### Sort-Specific Grout Precedence

When a semicolon is deleted between module items, the resulting grout placeholder needs to separate items the same way semicolons do. In expression context, concave grout has tight precedence (34) so it gets absorbed into let/fun bodies. In Mod/Sig context, `Skel.mk` accepts an optional `~sort` parameter — when `Mod` or `Sig`, grout uses the looser `mod_seq` precedence (47) instead.

The sort is threaded from `MakeTerm.re` where tile children are processed — the child sort is known from the tile's mold, so module body segments (`{...}`) get `sort=Mod` while expression bodies inside modules still get `sort=Exp`.

### Cursor Inspector and Statics Info

- **Mod items inside Module expressions**: Stored as `InfoExp` with `Cls.Mod(...)` (not `InfoMod`). This is because the `Module(items)` case in `uexp_to_info_map` expands items into nested Let/TyAlias expressions that share IDs with the Mod items. The elaborator looks up these IDs expecting `InfoExp` data (`self`, `ty`, etc.), so we cannot replace `InfoExp` with `InfoMod` without breaking elaboration. Instead, after expansion, we overwrite the cls field to `Cls.Mod(...)` so the cursor inspector displays the correct Mod sort and cls.
- **Sig items**: Stored as `InfoSig` (dedicated variant in `Info.t`). Sig items don't go through expression expansion, so there's no elaborator conflict.
- **InfoMod variant**: Exists in `Info.t` but is only used for edge cases (e.g., Mod items in MultiHole from parse errors). Not used for Mod items inside Module expressions.
- **Mod item cls names**: "Let declaration", "Type declaration", "Module expression"
- **Sig item cls names**: "Let declaration", "Type declaration"
- **Sort colors**: Both Mod and Sig have dedicated colors in the cursor inspector header, gamma icon, toggle switch, and dividers
- **Cursor inspector type display**: Mod and Sig items show only the cls name, not a type. The binding type is not directly available from `InfoExp` fields (it lives in the Pat's `InfoPat.ty` in the statics map, which the cursor inspector doesn't receive). Showing the module tuple type would be misleading.

### Module Semicolon Decoration

In `Arms.re`, module/sig semicolons render as lone shard hexagons (no arms to other pieces). Module/sig curly braces render as a pair with arm between them, filtering out semicolons.

---

## Key Files

### Core Implementation

| File                                    | Purpose                                                            |
| --------------------------------------- | ------------------------------------------------------------------ |
| `src/language/term/Sort.re`             | Sort enum with `Mod`, `Sig`, and `MPat`                            |
| `src/language/term/Grammar.re`          | `mod_term`, `sig_term`, `mpat_term`, `Module`/`ModuleExp` in exp   |
| `src/language/term/Mod.re`              | Mod term utilities and cls type                                    |
| `src/language/term/Sig.re`              | Sig term utilities and cls type                                    |
| `src/language/term/MPat.re`             | MPat term utilities and cls type                                   |
| `src/language/term/Cls.re`              | `Mod(Mod.cls)`, `Sig(Sig.cls)`, `MPat(MPat.cls)` variants         |
| `src/haz3lcore/lang/Form.re`            | Module/Sig forms, `mk_pre_c'` helper                               |
| `src/haz3lcore/tiles/Mold.re`           | `mk_pre'` for heterogeneous prefix forms                           |
| `src/haz3lcore/tiles/Segment.re`        | `remold_mod`/`remold_sig`/`remold_mpat` with fallback patterns     |
| `src/haz3lcore/tiles/Skel.re`           | ModSeq/SigSeq semicolons chainable, sort-specific grout precedence |
| `src/haz3lcore/zipper/action/Insert.re` | `effective_sort` with Mod→Exp / Sig→Typ fallback                   |
| `src/haz3lcore/lang/MakeTerm.re`        | Module/Sig parsing with flattening                                 |
| `src/language/statics/ExpandModule.re`  | Module expansion to nested let/type + labeled tuple                |
| `src/language/statics/Statics.re`       | Module type checking, `desugar_sig` in Asc, Mod/Sig item info      |
| `src/language/statics/Elaborator.re`    | Module elaboration for dynamics                                    |
| `src/language/statics/Info.re`          | `sort_of` returns Mod for InfoExp with Mod cls                     |
| `src/language/term/Typ.re`              | `desugar_sig` function                                             |
| `src/haz3lcore/pretty/ExpToSegment.re`  | Module and Sig pretty-printing to segments                         |
| `src/language/term/Abbreviate.re`       | Module abbreviation for probe display                              |
| `src/haz3lcore/tiles/Arms.re`           | Module semicolon decoration                                        |

### CSS

| File                                     | What                                                                       |
| ---------------------------------------- | -------------------------------------------------------------------------- |
| `src/web/www/style/variables.css`        | `--token-mod`, `--token-sig`, `--shard-mod`, `--shard-sig` color variables |
| `src/web/www/style/editor.css`           | `.child-line.Mod`, `.child-line.Sig`, keyword bolding                      |
| `src/web/www/style/cursor-inspector.css` | Mod/Sig gamma, toggle-switch, header, divider colors                       |

### Tests

| File                                       | What                                                                      |
| ------------------------------------------ | ------------------------------------------------------------------------- |
| `test/statics/Test_Statics_Modules.re`     | 58 statics tests + 10 module keyword tests                                |
| `test/evaluator/Test_Evaluator_Modules.re` | 17 evaluator tests (module exprs, keyword, nested, probes)                |
| `test/evaluator/Test_Evaluator_Probes.re`  | Module probe tests (probes inside module bodies)                          |
| `test/evaluator/Test_Evaluator_ProbeCallStack.re` | Module functions in probe call stacks                               |
| `test/Test_MakeTerm.re`                    | Module/keyword parsing tests including nested modules                     |
| `test/Test_Elaboration.re`                 | 4 module elaboration tests (module → labeled tuple)                       |
| `test/Test_ExpToSegment.re`                | 6 module/sig roundtrip tests + 1 skipped (empty module structural diff)   |
| `test/Test_Editing.re`                     | 4 module editing tests (brace insertion, let inside module)               |
| `test/Test_Abbreviate.re`                  | 2 module abbreviation tests                                               |
| `test/Test_Menhir.re`                      | 73 tests including module keyword, sig annotations, QCheck round-trips    |
| `test/Test_Equality.re`                    | Module alpha-equivalence tests (ModLet, MPat, ModuleMod)                  |
| `test/Test_Indentation.re`                 | Module body indentation and nesting tests                                 |
| `test/Test_RefractorSerialization.re`      | Module syntax round-trip through parse/print                              |
| `test/Test_AutoProbe.re`                   | Auto-probe in modules: probes definition exprs, not declarations          |

### In-Editor Documentation

| File                           | What                                                                                    |
| ------------------------------ | --------------------------------------------------------------------------------------- |
| `src/web/init/docs/Modules.ml` | In-editor doc slide with 26 examples covering construction, signatures, and limitations |
