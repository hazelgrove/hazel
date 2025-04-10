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

A built-in livelit is created by implementing the `BuiltinLivelit` module type. The current structure uses OCaml modules to define livelits, which are converted into Hazel livelits at compile time.

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
  let model_from_hazel: model_exp => model_t;
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
  let action_from_hazel: action_exp => action_t;
  let update: (action_t, model_t) => model_t;

  // View/rendering function
  let view: (model_t, action_t => Ui_effect.t(unit)) => node_or_list;
  
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

## Styling Livelits

To add CSS to style your livelit, modify the `src/haz3lweb/www/style/projectors/proj-livelit.css` file.

## Example: Slider

```reasonml
module Slider: BuiltinLivelit = {
  let name = "slider";

  type model_t = Bigint.t;
  type expansion_t = Bigint.t;
  type action_t =
    | SetModel(model_t);

  let hazel_model_t: TermBase.Typ.t = Typ.temp(Atom(Int));
  let model_to_hazel: model_t => model_exp =
    (x: model_t) => DHExp.fresh(Atom(Int(x)));
  let model_from_hazel: model_exp => model_t =
    (x: model_exp) =>
      switch (x.term) {
      | Atom(Int(n)) => n
      | _ => Bigint.of_int(-1)
      };
  let model_default: model_t = Bigint.of_int(50);

  let hazel_expansion_t: TermBase.Typ.t = Typ.temp(Atom(Int));
  let expansion_f: model_t => expansion_t =
    (x: model_t) =>
      switch (x) {
      | n => n
      };
  let expansion_to_hazel: expansion_t => expansion_exp =
    (x: expansion_t) =>
      switch (x) {
      | n => DHExp.fresh(Atom(Int(n)))
      };
  let update: (action_t, model_t) => model_t =
    (action: action_t, _model: model_t) => {
      switch (action) {
      | SetModel(n) => n
      };
    };

  let hazel_action_t: TermBase.Typ.t =
    Sum([Variant("SetModel", [], Some(Atom(Int) |> Typ.fresh))])
    |> Typ.fresh;
  let action_to_hazel: action_t => action_exp =
    (action: action_t) =>
      switch (action) {
      | SetModel(n) =>
        Ap(
          Forward,
          Constructor("SetModel", Some(Some(Atom(Int) |> Typ.fresh)))
          |> DHExp.fresh,
          Atom(Int(n)) |> DHExp.fresh,
        )
        |> DHExp.fresh
      };
  let action_from_hazel: action_exp => action_t =
    (action: action_exp) => {
      switch (action.term) {
      | Ap(
          Forward,
          {term: Constructor("SetModel", _), _},
          {term: Atom(Int(n)), _},
        ) =>
        SetModel(n)
      | _ => SetModel(Bigint.of_int(-1))
      };
    };

  let view: (model_t, action_t => Ui_effect.t(unit)) => node_or_list =
    (model: model_t, send_action) => {
      let n = model;

      Node(
        Util.Web.range(
          ~attrs=[
            Attr.on_input((_, v: string) => {
              send_action(SetModel(Bigint.of_string(v)))
            }),
          ],
          ~min="0",
          ~max="100",
          Bigint.to_string(n),
        ),
      );
    };

  let size: ProjectorCore.Shape.t =
    ProjectorCore.Shape.{
      vertical: Inline,
      horizontal: 20,
    };
};
```