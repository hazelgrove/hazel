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
or `module M` without a type has type `?`. A signature declares each member
once: a later declaration of a name is an error (`SigDuplicateMember`) and
the first is the member; value and module members share a namespace, type
members have their own. A `let` item declares a member by name, so one whose
pattern is not a name, `let (a, b) : (Int, Int)`, is an error
(`SigLetNotAName`) and declares nothing.

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
  nothing in its siblings. `ClosedSig.t` is abstract: building one asserts
  that of its items.

`Typ.close` closes a type where statics reads it from the syntax (an
annotation's `Info.typ.ty`, alias definitions, type arguments, module
ascriptions). It substitutes earlier type members into later items, resolves
paths rooted at earlier module members, and expands an outer alias named
like a sibling type member. A type member's definition is read by
`Typ.alias_def`, as a `type` alias's and a module's `type` item's are:
closed with its own name abstract, then made recursive if it mentions
itself. `Typ.sig_item_scopes` gives each written item the context it is
read in, so closing and the cursor inspector's walk over the written items
agree. Closing a closed signature closes each member alone, never
re-reading it as mentioning a sibling. `utyp_to_info_map` builds each
node's closed type from its children's, so a type is closed once, where it
is read.

`Sig.closed` gives the items of a closed signature and raises
`Sig.OpenSignature` on an open one. Every operation that reads a member's
type (`has_fun`, `free_vars`, `count_unknowns`, `subst`, `normalize`,
`meet`, `equal_up_to_aliases`, `fun_in`, `as_sig`, `path_sig`, path
normalization and runtime sealing) goes through it, so an open signature
reaching one is a bug in where the type was read, not a user error.
`Sig.items` gives the items in either reading, for syntactic walks:
printing, id collection, counting.

- `Typ.meet` on two signatures requires the same value-member names and the
  same type-member names (order-insensitive) and meets members pairwise. A
  signature is inconsistent with every other type constructor, including
  `Prod`.
- `Typ.path_sig` resolves a module path (`Var(M)`, `M.P`) to its closed
  signature. A root is whichever of a type alias and a value variable of
  that name was bound later; a value counts only if its type is a
  signature. `weak_head_normalize` uses it for `ProdProjection`, falling
  back to the labeled-tuple projection.
- A closed signature prints its members as they are: a written
  `{ type T = Int; let x : T }` displays as `{ type T = Int; let x : Int }`.

### Statics

The `Module(items)` case in `Statics.re` checks the body item by item. A
`let` or `module` item is bound by the `Let` arm's `let_binding` and
`let_finish`, so recursion detection, pattern checking and exhaustiveness
are the `let`'s; its marks (an inexhaustive pattern) land on the item.
When the module is analyzed against a signature,
`ModuleHelpers.expected_members` reads the signature's members in the
body's context (an outer alias named like a type the body declares is
expanded), and `ModuleHelpers.expected_pat_type` assembles from them the
expectation on each binder pattern with the names it holds to it: a
component the signature says nothing about is left to synthesis, and where
two expectations on one component cannot be met together the later names
are left unconstrained, so the module as a whole reports the disagreement.
The definition is analyzed against the pattern's own type with the
expectation filling what the pattern leaves open, so a user annotation is
checked against the signature on the annotation and the definition against
the annotation. Only names no later item rebinds are expected anything of.
After checking:

- The module's type is `ModuleHelpers.sealed`: a member checked against
  the signature on its item takes the signature's type where the two
  disagree (the disagreement having been reported there); any other member
  keeps its own, so the module-level meet reports it. The exported
  signature is closed: the body's own type names are expanded in it.
- A `type T = ...` item whose definition differs from the signature's is
  marked on the definition (`Mark.ModuleTypeMemberMismatch`).
- `ModuleHelpers.missing_items` / `extra_members` produce
  `Mark.ModuleMissingMembers` / `Mark.ModuleExtraMembers` on the module node.
  A hole in a binding position of the module stands in for the members it
  lacks, and a hole item in the signature for the members it does not
  declare; neither is reported.
- A bare expression item shares its ids with its expression (`Mod.of_exp`,
  from both parsers): the expression's info is the item's, and its parent is
  the module.

`ModuleExp(mpat, def, body)` is checked as `Let(pat, def, body)` and
elaborates to that `Let`. `Dot(e, x)` on a signature-typed `e` projects the
value member; a member the signature lacks is reported on the label
(`ModuleMemberNotFound`, or `ModuleMemberIsType` when `x` names a type
member) while the dot carries only a message, the way `M.Fake` in a type
reports on `Fake`
(`ModuleTypeMemberNotFound`). A hole item in the signature could declare
the member, so under one the projection is unknown and nothing is
reported, in both positions. A value used as the root of a type path gets
`TypWantModule`. In type position, `utyp_to_info_map` threads a context
through signature items and resolves `M.T` through `Typ.path_sig`; TyDi
receives the member names via `ModuleMemberExpected`.
`==` on a module whose members include a function is `Mark.CompareFun`,
which names the member (`Typ.fun_in`).

### Dynamics

`Transition.re` evaluates `Module(items)` item by item: the first pending
item's definition is evaluated (evaluation contexts `EvalCtx.ModuleItem` and
`EvalCtx.ModuleVal`), its pattern is matched, the bindings extend the
environment for the remaining items, and each bound name becomes a
`ModVal(x, v)` item (replacing an earlier binding of the same name). A bare
expression item is discarded once evaluated; a `type` item is dropped, and a
`module` item elaborated to a `let` item, before either reaches dynamics. A
module whose items are all `ModVal` is a value.

`Dot` on a module value returns the named binding. Ascribing a module value
to a signature (`Ascriptions.re`) keeps the signature's value members in
signature order, ascribes each to its declared type, and drops the rest,
unless a hole item in the signature could declare them, when they stay as
they are;
type members have no runtime content. `ModVal` items compare equal to the
literal binding `let x = v` (`Equality.re`), and display as `let x = v`.
Two module values compare under `==` and `!=` member by name
(`Mod.paired_bindings`, in `DHExp.ty_comparable` and `DHExp.poly_equal`).

### Sort Fallback Patterns, Forms and Decorations

Mod→Exp and Sig→Typ fallbacks in remolding (`Segment.re`) and insertion (`Insert.re`), the semicolon special
case, the heterogeneous prefix forms (`mk_pre_c'` in `Form.re`), sort-specific
grout precedence (`Skel.re`), module semicolon decoration (`Arms.re`), and the
Menhir parser's `ModuleExp`/`ModItemModule` structure with `Conversion.re`'s
`mpat_of_pat`/`pat_of_mpat`.

### Cursor Inspector and Statics Info

Module items are `InfoMod`, carrying the member the item declares (the
bound pattern's type for `let`/`module` items, the declared type for `type`
items), the item's marks and co-contexts. Items are ancestors of their
patterns and definitions. `Mod.pat` and `Mod.def` give an item's written
parts for code that walks them (composition, probe targets). Sig items are
`InfoSig`.

---

## Key Files

| File                                    | Purpose                                                            |
| --------------------------------------- | ------------------------------------------------------------------ |
| `src/language/term/Sort.re`             | Sort enum with `Mod`, `Sig`, and `MPat`                            |
| `src/language/term/Grammar.re`          | `mod_term` (incl. `ModVal`), `sig_term`, `mpat_term`, `Module`/`ModuleExp`, `Sig` |
| `src/language/term/Sig.re`              | Sig term utilities, the member view, `closed` and `items`          |
| `src/language/term/ClosedSig.re`, `.rei` | Closed signature items, abstract                                  |
| `src/language/term/Mod.re`              | Mod term utilities and evaluated-binding helpers                   |
| `src/language/term/MPat.re`             | MPat term utilities                                                |
| `src/language/term/Typ.re`              | `close`, `sig_item_scopes`, `alias_def`, `extend_ctx`, Sig meet and normalization, `path_sig`, `fun_in` |
| `src/language/statics/Ctx.re`           | `newest_var_or_ctr`, `newest_tvar_or_var`: the newest binding of a capitalized name, and of a path root |
| `src/language/statics/ModuleHelpers.re` | Signature expectations per binder (`expected_members`, `expected_pat_type`), `sealed`, missing and extra members, module references in written types |
| `src/language/statics/Statics.re`       | Module/ModuleExp cases, `Dot` on signatures, `M.T` in types        |
| `src/language/statics/Mark.re`          | `ModuleMissingMembers`, `ModuleExtraMembers`, `ModuleTypeMemberMismatch`, `ModuleMemberNotFound`, `ModuleMemberIsType`, `ModuleTypeMemberNotFound`, `TypWantModule`, `CompareFun`, `SigDuplicateMember`, `SigLetNotAName` |
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
