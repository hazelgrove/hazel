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
(manifest type member) and `module Name : Signature` (sub-module member,
which may be capitalized). Items scope sequentially: `let x : T` may mention
a `type T` declared earlier in the same signature. A member written `let x`
or `module M` without a type has type `?`.

```
module Outer : { module Inner : { let x : Int }; let y : Int } =
  { module Inner = { let x = 1 }; let y = 2 }
let outer : { let inner : { let x : Int } } = { let inner = { let x = 1 } }
```

A module synthesizes `module Inner : { … }` for a `module Inner = …` item and
`let inner : { … }` for a `let inner = { … }` item.

A module analyzed against a signature must define exactly the members the
signature declares, each value member must have the declared type (with the
signature's own type members substituted) at its last definition — one a later
item shadows is only synthesized — and each type member must be
defined as the declared type. A member's type error is reported on its
definition; missing members (`ModuleMissingMembers`) and extra members
(`ModuleExtraMembers`) are reported on the module; a differing type member is
reported on its definition type
(`ModuleTypeMemberMismatch`). When one of the items is a hole (a hole item,
or a binder with a hole where a member could be bound) it may still become the
missing members, so they are assumed rather than reported and the module has
the signature's type.

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

## Not Yet Supported

- **Sealing.** A module must export exactly the members its signature
  declares; extra members are an error. Planned: an ascription, annotated
  binder or application argument seals the extras away. The runtime already
  drops them (see Dynamics).
- **Abstract type members.** Signatures cannot yet declare `type T` without a
  definition, so there is no sealing of representations and no path types.
- **Comparing modules with `==`** is a runtime incomparable result; statics
  does not reject it yet.
- `open` / `include` (issues #2260, #2261).

## Architecture

### Sorts

Three sorts implement the surface syntax:

- **Mod** (`Sort.Mod`): module items — `let x = 1`, `type T = Int`, bare
  expressions
- **Sig** (`Sort.Sig`): signature items — `let x : Int`, `type T = Int`
- **MPat** (`Sort.MPat`): module name patterns — `M`, `M : { let x : Int }`

### Terms

**Module items** (`mod_term` in `Grammar.re`): `ModLet(pat, exp)`,
`ModType(tpat, typ)`, `ModuleMod(mpat, exp)`, `ModExp(exp)`, holes, and the
dynamics-only `ModVal(name, exp)` — an evaluated binding.

**Signature items** (`sig_term`): `SigLet(pat)` (the pattern carries the
optional `: Type`), `SigType(tpat, typ)`, `SigModule(mpat)` (the module name
pattern carries the optional `: Signature`), holes.

**Expression level**: `Module(list(mod_t))`, `ModuleExp(mpat, exp, exp)`.
**Type level**: `Sig(signature)`, `ProdProjection(typ, typ)` (`M.T`), where
`signature` is `Open(list(sig_t))` or `Closed(ClosedSig.t(sig_t))`.

### Signature types

`Typ.Sig` is a first-class type, in one of two readings. `Sig.re` exposes a
member view (`Sig.members`: `Val(x, τ)` / `TypeManifest(T, τ)`), skipping
holes and malformed items.

- **Open** is a signature as written. Its items scope sequentially: a later
  item may mention an earlier type member by name, or root a path at an
  earlier module member (`module P : S; let x : P.T`). The parser and
  `MakeTerm` produce only open signatures.
- **Closed** is the reading statics works with. Each item's type is written
  in terms of the context outside the signature, so the items are
  independent of one another and of their order, and a member's name binds
  nothing in its siblings. `ClosedSig.t` is abstract: `Typ.close` builds one
  from written items, and module synthesis builds the module's own
  signature from members it typed outside the module. Nothing else should
  call `ClosedSig.of_items` outside tests.

`Typ.close` closes a type where statics reads it from the syntax (an
annotation's `Info.typ.ty`, alias definitions, type arguments, module
ascriptions). It substitutes earlier type members into later items, resolves
paths rooted at earlier module members, makes a self-mentioning type member
recursive, and expands an outer alias named like a sibling type member.
`Typ.sig_item_scopes` gives each written item the context it is read in, so
closing and the cursor inspector's walk over the written items agree.
Closing a closed signature closes each member alone, never re-reading it as
mentioning a sibling.

The type operations that read members (`subst`, `normalize`, `meet`,
`equal_up_to_aliases`, `fun_in`, `as_sig`, `path_sig` and path
normalization) take closed signatures only, and raise `Typ.OpenSignature` on
an open one: that is a bug in where a type was read, not a user error.
Printing, id collection and other syntactic walks accept both readings.

- `Typ.meet` on two signatures requires the same value-member names and the
  same type-member names (order-insensitive) and meets members pairwise. A
  signature is inconsistent with every other type constructor, including
  `Prod`.
- `Typ.path_sig` resolves a module path (`Var(M)`, `M.P`) to its closed
  signature. A root is whichever of a type alias and a value variable of
  that name was bound later; a value counts only if its type is a
  signature. `weak_head_normalize` uses it for `ProdProjection`, falling
  back to the labeled-tuple projection.
- `Typ.sig_display_items` prints an open signature as written. In a closed
  one, a member type equal to an earlier type member's definition is
  printed by that member's name.

### Statics

The `Module(items)` case in `Statics.re` type-checks the body through
`ModuleHelpers.lower`, which turns the items into nested `Let`/`TyAlias`
wrappers carrying the item ids (so the `Let` machinery, recursion detection
and pattern checking are reused, and the cursor inspector sees each item).
When the module is analyzed against a signature, each bare variable binder is
annotated with its expected member type (`ModuleHelpers.modlet_pat`), so
mismatches land on definitions. The annotation reaches variables inside
destructuring patterns (`let (a, b) = …` is checked as `(a : Int, b : Int)`,
so a wrong component is reported on that component) and `module Inner = …`
items, which receive the sub-signature the outer signature declares for them
(a member the sub-module lacks is reported inside it). A binder the user
annotated keeps its annotation. `refold_module_elab` strips the synthetic
ascriptions again by walking the user's pattern and its elaboration together.
After checking:

- `ModuleHelpers.module_sig_type` reads the module's signature back from the
  recorded pattern infos, in source order, keeping exported type members
  compact and inlining only shadowed ones.
- `ModuleHelpers.check_ana_type_members` marks the definition type of a
  `type T = ...` item that differs from the signature's
  (`Mark.ModuleTypeMemberMismatch`).
- `ModuleHelpers.missing_items` / `extra_members` produce
  `Mark.ModuleMissingMembers` / `Mark.ModuleExtraMembers` on the module node.
- `ModuleHelpers.refold_module_elab` rebuilds the elaborated `Module` from
  the checked chain: definitions keep their elaboration, synthetic binder
  annotations are stripped, type items are dropped.

`ModuleExp(mpat, def, body)` is checked as `Let(pat, def, body)` and
elaborates to that `Let`. `Dot(e, x)` on a signature-typed `e` projects the
value member; a member the signature lacks is reported on the label
(`ModuleMemberNotFound`, or `ModuleMemberIsType` when `x` names a type
member) while the dot carries only a message, the way `M.Fake` in a type
reports on `Fake`
(`ModuleTypeMemberNotFound`). A value used as the root of a type path gets
`TypWantModule`. In type position, `utyp_to_info_map` threads a context
through signature items and resolves `M.T` through `Typ.path_sig`; TyDi
receives the member names via `ModuleMemberExpected`.

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
signature order, ascribes each to its declared type, and drops the rest;
type members have no runtime content. `ModVal` items compare equal to the
literal binding `let x = v` (`Equality.re`), and display as `let x = v`.

### Sort Fallback Patterns, Forms and Decorations

Mod→Exp and Sig→Typ fallbacks in remolding (`Segment.re`) and insertion (`Insert.re`), the semicolon special
case, the heterogeneous prefix forms (`mk_pre_c'` in `Form.re`), sort-specific
grout precedence (`Skel.re`), module semicolon decoration (`Arms.re`), and the
Menhir parser's `ModuleExp`/`ModItemModule` structure with `Conversion.re`'s
`mpat_of_pat`/`pat_of_mpat`.

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
| `src/language/statics/Mark.re`          | `ModuleMissingMembers`, `ModuleExtraMembers`, `ModuleTypeMemberMismatch`, `ModuleMemberNotFound`, `ModuleMemberIsType`, `ModuleTypeMemberNotFound`, `TypWantModule`, `TypeMemberCapture` |
| `src/language/dynamics/transition/Transition.re` | Module evaluation, `Dot` on module values                 |
| `src/language/dynamics/transition/Ascriptions.re` | Sealing a module value to a signature                    |
| `src/language/dynamics/stepper/EvalCtx.re` | `ModuleItem`, `ModuleVal` evaluation contexts                   |
| `src/language/grammar/FormId.re`          | Module/Sig families + labels                                       |
| `src/haz3lcore/lang/Form.re`            | Module/Sig forms, `mk_pre_c'` helper                               |
| `src/haz3lcore/lang/MakeTerm.re`        | Module/Sig parsing with flattening                                 |
| `src/haz3lcore/pretty/ExpToSegment.re`  | Module, Sig and `ModVal` printing                                  |
| `src/menhirParser/Parser.mly`, `Conversion.re` | Text parser for module forms                                |

### In-Editor Documentation

| File                                         | What                                          |
| -------------------------------------------- | --------------------------------------------- |
| `hazel-programs/docs/reference/modules.hz`   | The Modules doc slide (construction, type access) |
| `hazel-programs/docs/reference/module-signatures.hz` | The Module Signatures slide (signatures, signature errors) |
| `src/web/app/explainthis/data/Sig*.re`, `Mod*.re`, `DotTyp.re` | Explain-this content         |
