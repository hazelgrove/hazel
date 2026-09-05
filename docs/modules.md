# Hazel Module System

## Overview

Hazel modules are first-class values whose types are **signatures**. A module
is written with curly braces containing semicolon-separated `let` and `type`
items; a signature is written with the same braces in type position and lists
value members (`let x : Int`) and type members (`type T = Int`). Signature
types are distinct from labeled tuple types: `{ let x : Int }` and `(x=Int)`
are inconsistent with each other.

At runtime a module evaluates item by item to a **module value**, a module
whose items are its exported bindings with their values (`{ let x = 1; let y
= 2 }`). Member access `m.x` reads the binding by name.

## Syntax

### Module Expressions

```
{ let x = 1; let y = true }           -- basic module
{ type T = Int; let x : T = 5 }       -- with type member
{ let a = 1; test a == 1 end }        -- with bare expression (side effect)
{ let m = { let x = 1 }; let y = m.x } -- nested modules
{}                                      -- empty module
```

Each item is one of:

- **`let` binding**: `let pat = exp` — binds a value (any pattern; each bound
  variable becomes a member)
- **`type` member**: `type T = typ` — introduces a type member, in scope for
  the items that follow and exported in the module's signature
- **`module` binding**: `module M = exp` — binds a module under a name that
  may be capitalized
- **Bare expression**: Any expression, useful for `test` assertions and side
  effects

Later items can reference earlier ones. When several items bind the same
name, the last binding is exported (`{ let x = 1; let x = 2 }.x` is `2`); a
type member declared twice is exported once, and members defined between the
two declarations see the earlier definition inlined.

### Signatures

```
let m : { let x : Int; let y : Bool } = { let x = 1; let y = true }
let m : { type T = Int; let x : T }   = { type T = Int; let x = 1 }
type MSig = { let x : Int }
let m : MSig = { let x = 1 }
```

Signature items are `let name : Type` (value member), `type T = Type`
(manifest type member), `type T` (abstract type member) and
`module Name : Signature` (sub-module member, which may be capitalized). Items scope sequentially: `let x : T` may mention
a `type T` declared earlier in the same signature. A member written `let x`
or `module M` without a type has type `?`.

```
module Outer : { module Inner : { let x : Int }; let y : Int } =
  { module Inner = { let x = 1 }; let y = 2 }
let outer : { let inner : { let x : Int } } = { let inner = { let x = 1 } }
```

A module synthesizes `module Inner : { … }` for a `module Inner = …` item and
`let inner : { … }` for a `let inner = { … }` item.

A module analyzed against a signature must define every member the signature
declares, each value member must have the declared type (with the signature's
own type members substituted), and each type member must be defined as the
declared type. A member's type error is reported on its definition; missing
members (`ModuleMissingMembers`) are reported on the module; a differing type
member is reported on the `type` item (`ModuleTypeMemberMismatch`).

### Width subtyping

Where an expression is analyzed against a signature (a `let` or `module`
annotation, an ascription, a function argument, a pattern), the module may
export more members than the signature declares. The extra members are
sealed away: the binder has exactly the signature's type, `m.y` on a member
the signature omits is an error, and at runtime the module value keeps only
the signature's members. Value members may themselves be wider than declared,
and a function expecting a narrower module fits where one expecting a wider
module is required (contravariant domain).

```
let m : { let x : Int } = { let x = 1; let y = 2 } in m.x    -- 1; m.y is an error
let f = fun (m : { let x : Int }) -> m.x in f({ let x = 1; let y = 2 })
```

Consistency itself stays exact: `if` branches and other joins require the
same members, and `{ let x : Int }` is not consistent with
`{ let x : Int; let y : Int }`; ascribe the join if you need sealing there.

### Abstract type members and sealing

```
module Counter : { type T; let zero : T; let incr : T -> T; let get : T -> Int } =
  { type T = Int; let zero = 0; let incr = fun c -> c + 1; let get = fun c -> c }
in Counter.get(Counter.incr(Counter.zero))       -- 1
Counter.zero + 1                                  -- error: Counter.T is not Int
let c : Counter.T = Counter.incr(Counter.zero)    -- the path type names the abstract type
module C2 = Counter in (Counter.zero : C2.T)      -- an alias shares the abstract type
```

A signature item `type T` with no definition declares an abstract type
member. A module analyzed against the signature must define `T` (a missing
definition is a `ModuleMissingMembers` error, and a value member of the same
name does not count), and its value members are checked against the module's
own definition of `T`. Outside the module, `T` is known only as the path
`Counter.T`: a stuck type that is consistent with itself and with `?` and
nothing else. Two modules sealed by the same signature have distinct abstract
types. A module bound to another (`module C2 = Counter`, `let c = Counter`)
shares them, because a module variable's type exposes its own abstract members
as paths (`{ type T = Counter.T; ... }`). Only a path (a module variable, or a
member reached through one) can name an abstract type; projecting a member of
abstract type out of any other expression gives `?`, and a signature alias
with abstract members (`type S = { type T }`) does not name them either:
`S.T` is `?`.

In the editor, typing `type` inside a signature produces the bare `type T`
form; typing `=` after its type pattern upgrades it to `type T = …`. In a
module body `type` still produces `type T = …`.

### Member Access and `module`

```
let m = { let x = 1; let y = 2 } in m.x            -- 1
module M = { let x = 1; let y = 2 } in M.x + M.y   -- 3
module M : { let x : Int } = { let x = 42 } in M.x -- 42
```

The `module` keyword binds a module under a lowercase or capitalized name and
accepts an optional signature annotation. Capitalized names are otherwise
parsed as constructors, so `let M = ...` does not bind a module. Inside a
module body, `module Inner = { ... }` is an item.

TyDi (type-directed completion) suggests value member names after `m.` in
expression position and type member names after `M.` in type position.

### Qualified Type Access (`M.T`)

Type members are accessed in type position with dot notation:

```
module M = { type T = Int } in let x : M.T = 6 in x
module M = { type A = Int -> Bool; type B = A } in let f : M.B = fun x -> x > 0 in f
module M = { module P = { type S = Int } } in let x : M.P.S = 5 in x
let m = { type T = Int } in let y : m.T = 6 in y
module M = { type T = Int } in module N = M in let x : N.T = 5 in x
type S = { type T = Int } in let x : S.T = 1 in x
```

`M.T` resolves through the type of the variable `M`: a variable whose type is
a signature with a manifest type member `T`. Nested paths follow value
members (`M.P` is a module-typed member of `M`). A type alias whose
definition is a signature also supports `S.T`. `P.x` on a labeled tuple
*type* alias still projects the label's type.

## What Works

| Feature                              | Status |
| ------------------------------------ | ------ |
| Module syntax (`{ let ... }`)        | Works  |
| Signature types (distinct from Prod) | Works  |
| Width subtyping at analysis positions | Works |
| Type members in signatures (checked) | Works  |
| Member access via `.`                | Works  |
| Module values at runtime             | Works  |
| Bare expressions (side effects)      | Works  |
| Shadowing (last binding wins)        | Works  |
| Nested modules                       | Works  |
| Sequential references between items  | Works  |
| Type-directed error attribution      | Works  |
| Empty module `{}`                    | Works  |
| `module` keyword, capitalized names  | Works  |
| Menhir parser (all module forms)     | Works  |
| TyDi completion (values and types)   | Works  |
| Qualified type access (`M.T`, `M.P.T`) | Works |
| Module aliasing (`module N = M`)     | Works  |
| Abstract type members, sealing, path types (`M.T`) | Works |

## Not Yet Supported

- **Comparing modules with `==`** is a runtime incomparable result; statics
  does not reject it yet.
- `open` / `include` (issues #2260, #2261).

## Architecture

### Sorts

Three sorts implement the surface syntax:

- **Mod** (`Sort.Mod`): module items — `let x = 1`, `type T = Int`, bare
  expressions
- **Sig** (`Sort.Sig`): signature items — `let x : Int`, `type T = Int`, `type T`
- **MPat** (`Sort.MPat`): module name patterns — `M`, `M : { let x : Int }`

### Terms

**Module items** (`mod_term` in `Grammar.re`): `ModLet(pat, exp)`,
`ModType(tpat, typ)`, `ModuleMod(mpat, exp)`, `ModExp(exp)`, holes, and the
dynamics-only `ModVal(name, exp)` — an evaluated binding.

**Signature items** (`sig_term`): `SigLet(pat)` (the pattern carries the
optional `: Type`), `SigType(tpat, typ)`, `SigTypeAbstract(tpat)`,
`SigModule(mpat)` (the module name pattern carries the optional
`: Signature`), holes.

**Expression level**: `Module(list(mod_t))`, `ModuleExp(mpat, exp, exp)`.
**Type level**: `Sig(list(sig_t))`, `ProdProjection(typ, typ)` (`M.T`).

### Signature types

`Typ.Sig(items)` is a first-class type. `Sig.re` exposes a member view
(`Sig.members`: `Val(x, τ)` / `TypeManifest(T, τ)` / `TypeAbstract(T)`),
skipping holes and malformed items. A signature is a dependent record: later items may mention
earlier type members by name, so:

- `Typ.free_vars`, `Typ.subst` and `Typ.normalize` treat type members as
  sequential binders, and value and module members as variables that later
  paths may go through (`{ module Inner : { type T }; let y : Inner.T }`);
  `Ctx.extend_sig_item` binds one item in a context. When a signature is
  checked against a provided one (`Typ.sig_sub`), such a path means what the
  module actually provides for the member, so `Inner` is bound to the
  provided type.
  Member names cannot be alpha-renamed; on capture `subst` falls back to
  substituting `?` into the remaining items.
- `Typ.sig_project_value` / `Typ.sig_project_type` return a member's type
  with the signature's earlier type members substituted, so `x : T` in
  `{ type T = Int; let x : T }` projects to `Int`. An abstract member stands
  for the path `self.T` when the signature is that of a module path
  (`~self`), for `?` otherwise, or for its own bare name under
  `~keep_local` (used when a module body is checked against its signature,
  where `T` resolves to the module's own definition).
- `Typ.meet` on two signatures requires the same value-member names and the
  same type-member names (order-insensitive) and meets members pairwise in a
  context extended with the type members; an abstract member matches only an
  abstract member. A signature is inconsistent with every other type
  constructor, including `Prod`. A stuck path `M.T` (an abstract member
  projected from a module path; `Typ.is_stuck_path_term`) meets only itself
  and `?`.
- `Typ.ana_meet(ctx, ~ana, ~syn)` is used wherever an expression is analyzed
  against a type (`StaticsBase.fixed_typ`, `expectation_mismatch_mark`,
  `syn_ana_ok_common`, and their `_pat` variants with the roles of the
  provided and required types swapped). It tries `meet` first; failing that,
  a signature fits a signature that declares a subset of its members
  (`Typ.sig_sub`, which opens the wider signature's type members under fresh
  names, an abstract one as a fresh abstract type variable), and functions
  are contravariant in their domain. An abstract member of the required
  signature is realized by whatever the provided signature declares for it;
  a manifest member is not satisfied by an abstract one. It returns `ana`
  when a subtyping step was needed, which is what seals the binder's type.
- `Typ.path_sig` resolves a module path (`Var(M)`, `M.P`) to its signature's
  items: a type alias first, then a value variable whose type is a signature.
  For a path rooted at a module value it also returns the path itself, so
  that `weak_head_normalize` (which uses it for `ProdProjection`, falling
  back to the labeled-tuple projection) returns the stuck `M.T` for an
  abstract member. `normalize` leaves a stuck path alone.
- `Typ.strengthen(ctx, ty, ~path)` replaces each abstract member `type T` of
  a signature by the manifest `type T = path.T`; it is the identity on
  signatures without abstract members.

### Statics

The `Module(items)` case in `Statics.re` type-checks the body through
`ModuleHelpers.lower`, which turns the items into nested `Let`/`TyAlias`
wrappers carrying the item ids (so the `Let` machinery, recursion detection
and pattern checking are reused, and the cursor inspector sees each item).
When the module is analyzed against a signature, each bare variable binder is
annotated with its expected member type (`ModuleHelpers.modlet_pat`), so
mismatches land on definitions. A member whose declared type mentions a
signature name the module does not define gets no annotation (the missing
member is reported on the module, and a free name here would be reported on
the signature), and the annotations carry fresh ids so checking them never
records info under the signature's own type nodes. After checking:

- `ModuleHelpers.module_sig_type` reads the module's signature back from the
  recorded pattern infos, in source order, keeping exported type members
  compact and inlining only shadowed ones.
- `ModuleHelpers.check_ana_type_members` marks a `type T = ...` item whose
  definition differs from the signature's (`Mark.ModuleTypeMemberMismatch`).
- `ModuleHelpers.missing_members` produces `Mark.ModuleMissingMembers` on
  the module node (an abstract type member the module does not define counts
  as missing); extra members are sealed away by `ana_meet`.
- `ModuleHelpers.refold_module_elab` rebuilds the elaborated `Module` from
  the checked chain: definitions keep their elaboration, synthetic binder
  annotations are stripped, type items are dropped.

`ModuleExp(mpat, def, body)` is checked as `Let(pat, def, body)` and
elaborates to that `Let`. `Dot(e, x)` on a signature-typed `e` projects the
value member; a member the signature lacks is reported on the label
(`ModuleMemberNotFound`, which also says when `x` names a type member) while
the dot carries only a message, the way `M.Fake` in a type reports on `Fake`
(`ModuleTypeMemberNotFound`). A value used as the root of a type path gets
`TypWantModule`, and a manifest type member differing from the signature is
reported once, on its `type` item: members are checked against the module's
own definition and the module is not reported again. A module variable (a `Var`, or a capitalized `Constructor`
that names a module) synthesizes its strengthened type, so `M : { type T;
let x : T }` is seen as `{ type T = M.T; let x : T }` and `M.x : M.T`; the
context entry itself stays unstrengthened. `Dot` passes the projected path
(`ModuleHelpers.path_of_exp`) as `~self` and strengthens a sub-module
member's type at the extended path (`m.inner.T`). In type position,
`utyp_to_info_map` threads a context through signature items and resolves
`M.T` through `Typ.path_sig`; an abstract member is reported as
`Message.PathAbstract`. TyDi receives the type member names (manifest and
abstract) via `ModuleMemberExpected`.

### Dynamics

`Transition.re` evaluates `Module(items)` item by item: the first pending
item's definition is evaluated (evaluation contexts `EvalCtx.ModuleItem` and
`EvalCtx.ModuleVal`), its pattern is matched, the bindings extend the
environment for the remaining items, and each bound name becomes a
`ModVal(x, v)` item (replacing an earlier binding of the same name). Type
items and bare expressions are discarded once evaluated. A module whose items
are all `ModVal` is a value.

`Dot` on a module value returns the named binding. Ascribing a module value
to a signature (`Ascriptions.re`) keeps the signature's value members in
signature order, ascribes each to its declared type with the signature's
type members substituted (a manifest member by its definition, an abstract
one by `?`, a no-op cast), and drops the rest; type members have no runtime
content. `ModVal` items compare equal to the
literal binding `let x = v` (`Equality.re`), and display as `let x = v`.

### Sort Fallback Patterns, Forms and Decorations

Unchanged from the original implementation: Mod→Exp and Sig→Typ fallbacks in
remolding (`Segment.re`) and insertion (`Insert.re`), the semicolon special
case, the heterogeneous prefix forms (`mk_pre_c'` in `Form.re`), sort-specific
grout precedence (`Skel.re`), module semicolon decoration (`Arms.re`), and the
Menhir parser's `ModuleExp`/`ModItemModule` structure with `Conversion.re`'s
`mpat_of_pat`/`pat_of_mpat`.

The bare `type T` signature form is `Form.SigTypeAbstract`, listed before
`SigType` so that `type` typed in a signature expands to it (expansion takes
the first matching form). `Insert.upgrade_bare_sig_type` runs when `=` is
inserted: if the pieces to the left of the caret are a bare `type` tile in
the Sig sort followed by at most one type-pattern token (and whitespace), the
tile is relabeled to the manifest form with its `=` missing, so the ordinary
backpack put-down completes `type T = ¦`. There is no downgrade; delete the
`=` to get a hole instead. In the text parser the bare form is
`SigItemTypeAbstract` (`Parser.mly`: `TYP tpat` inside a signature).

### Cursor Inspector and Statics Info

Mod items inside `Module` expressions are stored as `InfoExp` with a
`Cls.Mod(...)` class (the expanded `Let`/`TyAlias` shares the item id). After
checking, `ModuleHelpers.reclassify_expanded_module_items` rewrites the class
and replaces the wrapper's type (the type of the rest of the chain) with the
member the item declares: the bound pattern's type for `let`/`module` items,
the declared type for `type` items. Sig items are `InfoSig`. `InfoMod` is
used only for mispositioned items.

---

## Key Files

| File                                    | Purpose                                                            |
| --------------------------------------- | ------------------------------------------------------------------ |
| `src/language/term/Sort.re`             | Sort enum with `Mod`, `Sig`, and `MPat`                            |
| `src/language/term/Grammar.re`          | `mod_term` (incl. `ModVal`), `sig_term`, `mpat_term`, `Module`/`ModuleExp`, `Sig` |
| `src/language/term/Sig.re`              | Sig term utilities and the member view                             |
| `src/language/term/Mod.re`              | Mod term utilities and evaluated-binding helpers                   |
| `src/language/term/MPat.re`             | MPat term utilities                                                |
| `src/language/term/Typ.re`              | Sig normalization, meet, member projection, `path_sig`             |
| `src/language/statics/Ctx.re`           | `extend_sig_item`                                                  |
| `src/language/statics/ModuleHelpers.re` | Lowering for type checking, signature synthesis, refolding         |
| `src/language/statics/Statics.re`       | Module/ModuleExp cases, `Dot` on signatures, `M.T` in types        |
| `src/language/statics/Mark.re`          | `ModuleMissingMembers`, `ModuleTypeMemberMismatch`, `ModuleMemberNotFound`, `ModuleTypeMemberNotFound`, `TypWantModule` |
| `src/language/statics/StaticsBase.re`   | `fixed_typ`/mismatch hooks routed through `Typ.ana_meet`           |
| `src/language/dynamics/transition/Transition.re` | Module evaluation, `Dot` on module values                 |
| `src/language/dynamics/transition/Ascriptions.re` | Sealing a module value to a signature                    |
| `src/language/dynamics/stepper/EvalCtx.re` | `ModuleItem`, `ModuleVal` evaluation contexts                   |
| `src/haz3lcore/lang/Form.re`            | Module/Sig forms, `mk_pre_c'` helper                               |
| `src/haz3lcore/zipper/action/Insert.re` | `=` upgrade of a bare `type T` signature item                      |
| `src/haz3lcore/lang/MakeTerm.re`        | Module/Sig parsing with flattening                                 |
| `src/haz3lcore/pretty/ExpToSegment.re`  | Module, Sig and `ModVal` printing                                  |
| `src/menhirParser/Parser.mly`, `Conversion.re` | Text parser for module forms                                |

### Tests

| File                                       | What                                                                      |
| ------------------------------------------ | ------------------------------------------------------------------------- |
| `test/statics/Test_Statics_Modules.re`     | Signature synthesis, annotations, Sig/Prod distinctness, `M.T`            |
| `test/Test_Typ.re`                         | `Typ.Sig`, `Typ.AnaMeet`, `Typ.SigPaths`: meet, subtyping, paths, strengthening |
| `test/evaluator/Test_Evaluator_Modules.re` | Module values, member access, sealing at runtime                          |
| `test/Test_Elaboration.re`                 | Modules elaborate to modules                                              |
| `test/Test_TyDi.re`                        | Value/type member completion                                              |
| `test/Test_Menhir.re`, `Test_MakeTerm.re`, `Test_ExpToSegment.re` | Parsing and round-trips                            |
| `test/Test_Editing.re`                     | `Editing.SigAbstract`: bare `type T` and the `=` upgrade                 |

### In-Editor Documentation

| File                                         | What                                          |
| -------------------------------------------- | --------------------------------------------- |
| `hazel-programs/docs/reference/modules.hz`   | The Modules doc slide (construction, qualified types, signatures, errors) |
| `hazel-programs/docs/reference/module-signatures.hz` | The Module Signatures slide (width subtyping, abstract types). Kept separate because statics recursion depth grows with the `let` chain, and one slide of both was at the JavaScript stack limit |
| `src/web/app/explainthis/data/Sig*.re`, `Mod*.re`, `DotTyp.re` | Explain-this content         |
