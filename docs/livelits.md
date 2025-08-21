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

  // View/rendering function
  let view: (model_t, action_t => Ui_effect.t(unit)) => node_or_list;

  // Size specification
  let size: ProjectorShape.t;
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
