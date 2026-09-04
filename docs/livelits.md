# Livelits in Hazel

## Background

Hazel implements a version of the live literals (livelits) mechanism described in [our PLDI 2021 paper](https://hazel.org/papers/livelits-pldi2021.pdf), currently limited to:

- No parameters
- No splices
- No user-defined livelits (only builtins)

## Overview

A livelit is a live GUI widget which can be inserted into expressions and generates code by expansion to an expression of some given type. To invoke a livelit, insert the name of the livelit (always prefixed with ^) then press space.

Each livelit maintains an internal model, which we do not intend clients of the livelit to interact with. When testing a livelit you've created, you can unproject the livelit (by clicking the button on the bottom right of the Hazel UI) and edit this internal model directly.

Livelits live in the typing context, so they can be viewed using the context inspector, but currently there is no way to add new user-defined livelits.

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

  // Size specification
  let size: ProjectorCore.Shape.t;
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

### `^fumola`

| | |
| --- | --- |
| Expansion type | `Int` |
| Model | `(Int, String)` -- an opaque Fumola instance id, and the Fumola source text. |
| Expansion | The result of running the Fumola program, translated to a Hazel `Int`. A program that does not parse, or whose result is not an integer, expands to a hole. |
| In scope | `pointer(s)`, `get(s)`, `peek(s)` -- see below. |
| Requires | The Fumola wasm runtime; build it with `scripts/build-fumola-wasm.sh`. |

This is the first livelit whose model does not contain all of its own state,
so it is worth spelling out how it differs from the others.

The three livelits above are self-contained: `^slider`'s model *is* its value,
and `^js` stores its last result in its own model. `^fumola` instead holds a
**name for state that lives outside Hazel**. Its model is a pair

```text
(instance_id, program_text)
```

where `instance_id` identifies an entry of an external store

```text
sigma : FumolaInstanceId -> FumolaRuntimeState
```

held by the Fumola wasm module and reached through the `window.fumola` shim in
`src/web/www/prebundle.js`. That runtime state -- the Fumola interpreter and
its Adapton demanded computation graph -- is deliberately *not* serialized into
the Hazel model; only the name is.

Three consequences follow, and each is a thing the livelit has to do
explicitly rather than get for free:

- **Editing preserves the id.** Changing the program text keeps the same
  `instance_id` and re-evaluates against the same live runtime, so Fumola can
  reuse its prior computation instead of starting over. The edit is expressed
  in Fumola rather than through any repair API: the source text is wrapped as
  `force(`topLevel := thunk { <program text> })`, and re-assigning that name
  and re-forcing it is what gives the edit its incremental meaning.

- **Expansion is an observation, not stored state.** `expand` asks sigma for
  the runtime's current result and translates it; the result is never a second
  source of truth in the model. This relies on the invariant that `sigma(i)` is
  synchronized with the model's `program_text`.

- **Ids are generative.** Duplicating a livelit must not produce two widgets
  sharing one execution history. The view uses its `~id` -- the projector's
  persistent identity -- to claim its instance; when a copy turns up claiming
  an id another live projector already owns, it is given a fresh runtime and
  rewrites its own model to name it. The same mechanism covers reload: a saved
  program naming an id this session has never seen has that runtime realized
  on demand.

Every program runs after a small prelude, for two reasons that are easy to
trip over.

First, a livelit's program text is stored as a Hazel string literal, and Hazel
strings have no escapes -- `Token.is_string` permits at most two quote
characters in the whole token. So `prim "adaptonPointer"` cannot be written
inside a livelit at all, and every adapton primitive needs quotes.

Second, `:=` coerces its left side into a pointer but `@` does not: `@` wants
something that already *is* a pointer. So the way to read back what `1 := 2`
wrote is not `@(1)`.

The prelude supplies:

| | |
| --- | --- |
| `pointer(s)` | the pointer that symbol `s` names |
| `get(s)` | the value in that cell, recording a dependency |
| `peek(s)` | the value as an option, without recording a dependency; `null` if never written |

so that `1 := 2` in one edit and `get(1)` in the next reads back `2`.

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
