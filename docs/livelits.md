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

with `update: (Model, Action) => Model`, `view: Model => HTML` (handlers emit
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
projector, which renders the live HTML), so probes inside `view` and
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

  // View/rendering function
  let view: (model_t, action_t => Ui_effect.t(unit)) => node_or_list;

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

## Styling Livelits

To add CSS to style your livelit, modify the `src/web/www/style/projectors/proj-livelit.css` file.
