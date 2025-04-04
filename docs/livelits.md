# Livelits

## Background
Hazel implements a version of the live literals (livelits) mechanism described in [our PLDI 2021 paper](https://hazel.org/papers/livelits-pldi2021.pdf), currently limited to:

  - No parameters
  - No splices
  - No user-defined livelits (only builtins)

## Overview
A livelit is a live GUI widget which can be inserted into expressions and generates code by expansion to an expression of some given type. To invoke a livelit, insert the name of the livelit (always prefixed with ^) then press space.

Each livelit maintains an internal model, which we do not intend clients of the livelit to interact with. When testing a livelit you've created, you can unproject the livelit (by clicking the button on the bottom right of the Hazel UI) and edit this internal model directly.

Livelits live in the typing context, so they can be viewed using the context inspector, but currently there is no way to add new user-defined livelits. 

## Writing a livelit
A livelit is defined by a record of the following type:

```reasonml
type livelit_entry = {
  name: string,
  model_t: TermBase.Typ.t,
  model_default: string,
  expansion_t: TermBase.Typ.t,
  expansion_f: TermBase.Exp.t => TermBase.Exp.t,
  projector:
    (list(model_piece), Base.piece => Ui_effect.t(unit), Id.t) =>
    node_or_list,
  size: ProjectorCore.Shape.t,
  explain_this: list(string),
};
```

- `name`: The name of the livelit, which is how it can be invoked (for example, `slider` is invoked with `^slider`).
- `model_t`: The type of the internal model, which is a term type.
- `model_default`: The default value of the internal model, written as the literal string.
- `expansion_t`: The type of the expression that will be generated when the livelit is invoked.
- `expansion_f`: A function which takes the model `exp`, returning an `exp` of the type `expansion_t`. This function is used to convert the internal model into an expression.
- `projector`: A function that takes the model pieces of a livelit, a function to update the model, and an ID, returning a node or list of HTML nodes.
- `size`: The size of the livelit, which is a `ProjectorCore.Shape.t` type.
- `explain_this`: A list of strings that will be displayed when the user hovers over the livelit. This is used to explain what the livelit does.

After writing a new entry, generally in `/src/haz3lcore/tiles/Livelit.re`, simply add it to the `livelits` list at the end of the file.

## Styling a livelit

To add css to style your livelit, you can modify the `src/haz3lweb/www/style/projectors/proj-livelit.css` file.

## Example: Slider
```reasonml
let slider: livelit_entry = {
  explain_this: [
    "A slider livelit -- a simple integer input from 0 to 100."
  ],
  name: "slider",
  expansion_t: Typ.temp(Int),
  expansion_f: (model: Exp.t) =>
    switch (model.term) {
    | Int(n) => DHExp.fresh(Int(n))
    | _ => DHExp.fresh(Undefined)
    },
  model_t: Typ.temp(Int),
  model_default: "50",
  projector: (model: list(model_piece), update, id: Id.t) => {
    let Ctx.{model, piece} = List.nth(model, 0);
    let n =
      switch (model.term) {
      | Int(n) => n
      | _ => failwith("Slider livelit: not given int")
      };

    Node(
      Util.Web.range(
        ~attrs=[Attr.on_input((_, v) => update(put(v, Piece.id(piece))))],
        string_of_int(n),
      ),
    );
  },
  size:
    ProjectorCore.Shape.{
      vertical: Inline,
      horizontal: 20,
    },
};
```
