# Livelits in Hazel

## Background

Hazel implements a version of the live literals (livelits) mechanism described in [our PLDI 2021 paper](https://hazel.org/papers/livelits-pldi2021.pdf), currently limited to:

- No parameters
- No splices

Splices are the remaining gap, and they will arrive as a `SpliceRef` primitive type with
operations over it (`new_splice`, `set_splice`, `eval_splice`, in the paper's `UpdateCmd`
and `ViewCmd` monads). When they land, `expand` extends to return a pair with the list of
`SpliceRef`s, as in Fig. 3 of the paper:

```
expand : Model -> (Exp, List(SpliceRef))
```

The pair is there because the expansion must treat splices parametrically: the first
component takes one argument per listed `SpliceRef` and returns the expansion type.
`expand` itself stays pure — splices bring the monads, but not to `expand`. Until then the
splice list is empty, so `expand : Model -> Expansion` is an equivalent encoding of the
paper's closed expansion rather than a deviation from it.

## Overview

A livelit is a live GUI widget which can be inserted into expressions and generates code by expansion to an expression of some given type. To invoke a livelit, insert the name of the livelit (always prefixed with ^) then press space.

Each livelit maintains an internal model, which we do not intend clients of the livelit to interact with. When testing a livelit you've created, you can unproject the livelit (by clicking the button on the bottom right of the Hazel UI) and edit this internal model directly.

Livelits live in the typing context, so they can be viewed using the context inspector. They can be defined either as OCaml builtins or in Hazel itself.

## User-Defined Livelits

A Hazel program can define a livelit by binding a livelit name to a module:

```
let ^pct = {
  type Model = Int;
  type Action = Int;
  type Expansion = Int;
  let init : Model = 50;
  let update = fun (m, a) : (Model, Action) -> a;
  let view = fun m : Model -> ...;
  let expand = fun m : Model -> m
} in
^pct(25) + ^pct(75)
```

with `update: (Model, Action) => Model`, `view: Model => Html.T` (handlers emit
Actions, as in the MVU apps — see mvu.md), and `expand: Model => Expansion`.
An optional member `shape = Inline(width) | Block(width, height) |
Tab(width, height)` (a `LivelitShape`) sets the projector's footprint in
character cells. Helpers are ordinary additional members.

All three type members are required, and they are the livelit's interface.
`Model` types each use's argument, `Action` types what the view emits, and
`Expansion` is what clients type against. There is no tuple form: a tuple
has nowhere to declare the types.

## Type-Checking the Expansion

A use of `^pct` synthesizes the declared `Expansion`, not the type of whatever
code `expand` produced. The check that earns this is performed for each
expansion, on its output: statics types the expansion (in synthetic mode, on a
throwaway info map) and compares the result with the declaration, marking the
use `BadLivelitExpansion` on an inconsistency — "Livelit expands to type Int,
but declares Expansion = String". The fault is located in the livelit rather
than in the client's surrounding code.

Checking at the use site is the PLDI 2021 paper's own strategy rather than a
deviation from it. §3.2.5 records that Hazel does not statically check
`expand`'s definition, and that the parameterized expansion is instead
"validated at each livelit invocation site, with errors reported to the
client". With no splices that expansion takes no arguments, so validating it
degenerates to checking the expansion at the expansion type, which is what
happens here. The paper names definition-site verification via "a typed
quotation system as in, e.g., MetaOCaml" as the alternative, awkward because
"the type of the quotation depends on the type of each splice in the splice
list".

Consistency, not equality, is the test, as everywhere else in the language: an
expansion that synthesizes `Unknown` (an unannotated `expand` whose body gives
statics nothing to go on, or a builtin livelit generating a hole) stays gradual
and is not marked. What the declaration buys in that case is still real —
clients type against a known type instead of `Unknown`.

The expansion typed is the one built from the *surface* model, since statics
traverses surface syntax only; the elaborated model, which a user-defined
livelit's expansion embeds, is what actually evaluates.

Each use elaborates to `^name.expand(model)` through the runtime `^name`
binding, so shadowing and scoping behave like ordinary lets, and each use's
model lives in its own argument syntax. `^name.member` is also surface
syntax: it accesses the definition record (e.g. `^pct.expand(25)`).

A projected use's `view` runs in the main evaluation (sampled at the
projector, which renders the live Html.T), so probes inside `view` and
`expand` see samples per use. Interactions commit the transition itself as
the new argument — `^name(^name.update(prev, action))` — normalized by the
next evaluation, so the last interaction stays in the program where
`update`'s probes and the stepper can reach it; each commit collapses the
previous transition to its value first. Actions must therefore be
first-order data. `update` alone still evaluates in the builtin environment
(at event time, as a fallback), so helpers belong among the members.

Example programs: `hazel-programs/docs/livelits/` (shipped as the "Livelits"
doc slides, embedded at compile time — an edit there ships on the next
build). The adapter is `src/language/statics/UserLivelit.re`; rendering is
the `user_def` branch of `LivelitProj.re`.

## Creating a Built-in Livelit

A built-in livelit is created by implementing the `BuiltinLivelit` module type. The current structure uses OCaml modules to define livelits, which are converted into raw livelits (which use Hazel language encodings in preparation for future work on user-defined livelits) at compile time.

### Module Type for Built-in Livelits

```reasonml
type model_exp = TermBase.Exp.t;
type expansion_exp = TermBase.Exp.t;
type action_exp = TermBase.Exp.t;

module type BuiltinLivelit = {
  // Livelit name (used with ^ prefix to invoke)
  let name: livelit_name;

  // Model type and related conversions
  type model_t;
  let hazel_model_t: TermBase.Typ.t; // defines the type of model_exp
  let model_to_hazel: model_t => model_exp;
  let model_from_hazel: model_exp => option(model_t);
  let model_default: model_t;

  // Expansion type and related conversions
  type expansion_t;
  let hazel_expansion_t: TermBase.Typ.t; // defines the type of expansion_exp
  let expansion_f: model_t => expansion_t;
  let expansion_to_hazel: expansion_t => expansion_exp;

  // Actions that update the model
  type action_t;
  let hazel_action_t: TermBase.Typ.t; // defines the type of action_exp
  let action_to_hazel: action_t => action_exp;
  let action_from_hazel: action_exp => option(action_t);
  let update: (action_t, model_t) => model_t;

  // View/rendering function. [id] is the projector's persistent unique
  // identifier: it distinguishes one live projector from another, and stays
  // the same as the model is edited. Livelits with self-contained models
  // ignore it; a livelit whose model names external state uses it to tell
  // editing apart from duplication (see the fumola livelit below).
  let view:
    (~id: Id.t, model_t, action_t => Ui_effect.t(unit)) => node_or_list;

  // Shape (footprint) specification
  let shape: ProjectorShape.t;
};
```

## Registering a New Livelit

After creating a module that implements the `BuiltinLivelit` interface, add it to the `livelits` list at the end of the file:

```reasonml
let livelits: list(raw_livelit) =
  [(module Slider), (module Emotion), (module YourNewLivelit)]
  |> List.map(raw_of_builtin);
```

## The Built-in Livelits

### `^slider`

| | |
| --- | --- |
| Expansion type | `Int` |
| Model | `Int` |
| Expansion | The slider's current value, between 0 and 100. |

### `^emotion`

| | |
| --- | --- |
| Expansion type | `String` |
| Model | `Int` |
| Expansion | `"sad"` below 40, `"happy"` above 70, `"neutral"` otherwise. |

### `^js`

| | |
| --- | --- |
| Expansion type | `String` |
| Model | `(String, String)` -- the JavaScript source, and the last result. |
| Expansion | The stored result string. Pressing *Compute* evaluates the source and stores the result in the model. |

### The Fumola livelits, and the value they produce

Three livelits: one declares a runtime, two run programs in one.

#### `^fumola_new`

| | |
| --- | --- |
| Model | `(Int, + Simple + Graphical)` -- instance id, Adapton semantics |
| Expansion type | `Int` -- the id naming the runtime |
| Default | `^fumola_new(0, Graphical)` |

Declares a runtime and the semantics it runs, and expands to the id that names
it. The other two livelits attach to whatever runtime their id names, creating
one with the default semantics if none exists; this is how a program says
which semantics it wants, once, where the runtime is made -- so that every
livelit sharing that id is talking to a runtime whose mode was *declared*
rather than inherited by accident.

Fumola spells this type `{#simple; #graphical}`; Hazel spells the same
structure `+ Simple + Graphical`, and the livelit API uses Hazel's spelling.

Runtimes default to the simple semantics. That is a deliberate temporary
choice: two incremental layers meet here, Hazel re-evaluating and Adapton
repairing, and until how they compose is understood the predictable mode is
the better default. It is intended to become `Graphical`, matching Fumola
itself, with `Simple` asked for explicitly. The cost of simple is that there is
no graph, so the graph introspection -- `Adapton.peekEvents()` and the edge
lists in `peekInfo` -- has nothing to report on.

Asking for the semantics a runtime already runs does nothing. That matters:
a livelit re-expands on every edit, and a reset is destructive, so a
re-expansion must not discard the history the other livelits have built.
Asking for a *different* semantics does reset the runtime, since that is what
requesting another semantics means.

The id would ideally be abstract, hiding the number. Hazel's abstract types
come only from polymorphic binders today -- there is no signature sealing --
so the expansion is an `Int`, and we call it a handle by convention only.

#### `^fumola_put_force`

| | |
| --- | --- |
| Model | `(Int, String, String)` -- instance id, thunk name, program |
| Expansion type | whatever the annotation says; it requires checking mode |
| Default | `^fumola_put_force(0, "`thunk", "1 + 2")` |

Wraps its program as `force(<name> := thunk { ... })`. That wrapper is what
gives an edit its incremental meaning: re-assigning the same name and forcing
it again reuses the thunk's execution history, so state written in one edit is
still there in the next.

The **thunk name** is Fumola source for a symbol, evaluated by Fumola itself
in a runtime kept only for that purpose. So you write a name the way Fumola
spells one -- `` `myThunk ``, `7`, `` `a(`b) `` -- and every form the language
supports works without Hazel knowing about any of them. Naming happens in a
runtime of its own, so a name that happens to have an effect cannot touch the
instance it names a thunk in.

It is written rather than derived, and that is the point: a name taken from a
Hazel id would start a *new* thunk whenever that id changed, quietly losing
the history the thunk exists to keep. Two thunk livelits sharing a runtime
must not share a name, or each overwrites the other's thunk.

#### `^fumola_eval`

| | |
| --- | --- |
| Model | `(Int, String)` -- instance id, program |
| Expansion type | whatever the annotation says; it requires checking mode |
| Default | `^fumola_eval(0, "1 := 2")` |

Evaluates its program at the top level, with no thunk around it -- a
**force-free stack**. No incremental reuse, but bindings it makes outlive it,
and the editor-mode operations belong here.

That is the distinction the two livelits encode. Fumola runs in two modes. The
**archivist** runs inside a force, and its operations are the tracked ones --
put (`:=`), get (`@`), and `force` -- the ones a dependency can be recorded
for, since a computation is in progress to record it against. The **editor**
is the more general mode and always runs on a force-free stack; `reset` and
the graph introspection belong to it alone, and a reset from inside a force is
not a meaningful request, so Adapton refuses it with `UnreachableForceEnd`.

`peek` is the interesting case: it is how the editor reads a cell, and it
stays available inside a force too, on purpose -- reading a cell without
recording a dependency is how you look at a computation while it runs. So a
peek in a thunk is meaningful, merely untracked.

It is strictly more expressive than `^fumola_put_force` -- you can write the
`force(... := thunk { ... })` yourself -- so the thunk livelit's value is that
it generates that wrapper, and generates a distinct name for it.

Both livelits name an instance, so **two livelits carrying the same id share
one runtime** and see each other's state and bindings. An editor can set up
state that a thunk then reads.

#### The peek value

Not a livelit: a value form, `FumolaPeek`, produced when a program returns a
*pointer*. It carries the instance, the program that reads the cell, and the
value that program produced:

```text
1 := 2   ->   FumolaPeek({instance_id, reads: "peek(1)", value: 2})
```

A value rather than something that steps to one, so the reference stays
visible instead of collapsing into what it refers to. Carrying the result is
what lets evaluation continue through it: a reference to a cell holding an
`Int` is usable as an `Int`, and statics synthesizes that type from the
carried value rather than needing an annotation.

`peek` rather than `get`, so that translating a result does not record a
dependency in the runtime being translated; the `!` unwraps peek's option,
assuming the cell is defined.

What a reference *shows* is not quite the program that read it. The `!` is
left off, and the option is put back on the right:

```text
peek(`x) = Some(41)
```

`!` is Fumola's option-unwrap and Hazel has no such operator, so
`peek(`x)! = 41` invites reading the `! =` as Hazel's not-equals. Written
this way the line is true of Fumola on its own terms -- peek answers an
option -- and spelled the way Hazel spells one. A value that could not be
read shows as a bare hole rather than `Some(?)`, which would claim more than
is known.

Pointers nest, each level keeping its own reference. A cell holding a pointer
back to itself stops at a hole rather than being followed forever, and an
unreadable cell keeps its reference with an unknown value.

The carried value is a **snapshot**, sound only because it is regenerated on
every expansion. It must never be persisted into a saved model, where it
would silently disagree with the runtime.

### What is in scope

The Fumola module library is compiled into the runtime, and eleven modules
are bound at the top level of every instance:

| | |
| --- | --- |
| `Adapton` | the adapton library itself |
| `List`, `LazyList`, `PureList` | list structures |
| `LevelTree`, `HashMap` | trees and maps |
| `Counters`, `RandomInput` | counters, and generated input |
| `MergeSort`, `Gcd`, `DelayedPut` | worked examples |

So `Gcd.gcd(12, 18)` works with no import. Everything else in the library is
still reachable by importing its path, e.g.
`import D "fumola/examples/deriveCompare";`.

A pointer's *type* is the type of what it points at, found by dereferencing: translation runs `peek(<name>)!` in the
same instance and takes the type of the answer, following a chain of pointers
for as long as it finds one. `peek` rather than `get`, so that translating a
result does not record a dependency in the runtime being translated.

A Fumola symbol arrives in Hazel as its text, with the backticks dropped at
every depth:

```text
`x                    ->  "x"
`adapton(`settings)   ->  "adapton(settings)"
```

This is also the only way to produce a string from a livelit at all, since
Hazel string literals admit no escapes and so a livelit's program can never
contain a double quote.

Not settled by this implementation: `FumolaInstanceId` is an ordinary `Int` in
the model rather than a distinct opaque value form, only first-order integer
results are translated, and Fumola values whose meaning depends on the runtime
(functions, thunks, references) have no Hazel representation at all.

## Styling Livelits

To add CSS to style your livelit, modify the `src/web/www/style/projectors/proj-livelit.css` file.
