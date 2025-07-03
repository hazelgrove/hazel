# UI Architecture Guide

Last updated 2024-11-22

Since [#1297](https://github.com/hazelgrove/hazel/pull/1297), the UI portion of Hazel is split into components, where each component is a file that follows the following format with inner modules:

```reason
module Model { ... }

module Update { ... }

(optional)
module Selection { ... }

module View { ... }

```

This roughly follows the elm architecture for an application where 
* an application's current state is stored using a `Model.t`, 
* an `Update.update` function takes an action (`Update.t`) and a `Model.t` and returns the next `Model.t`
* a `View.view` function takes the current state of the model, and returns a virtual DOM (our representation of HTML)



## What goes in the `Model.t`?

Anything that describes the current state of the Hazel editor goes in `Model.t`. This includes:

* The `Model.t` of subcomponents

* Any values that can be directly manipulated by the user (Often annotated with a `\\ UPDATED` comment)

* Anything we don't want to recalculate every redraw (Often annotated with a `\\ CALCULATED` comment)

If the `Model.t` includes some things that we may not want to save (e.g. the typing information of the current editor), as well as `Model.t`, we also include a similar `Model.persistent` type, along with functions `Model.persist` and `Model.unpersist` to convert.

## `Update.update` and `Update.calculate`

Inside the `Update` module, there are two important functions:

`Update.update : (~settings: Settings.t, Update.t, Model.t) -> Updated.t(Model.t)`

`Update.calculate : (~settings: Settings.t, Model.t, ...) -> Model.t`

The `update` function always runs first, and makes minimal changes to record the intention of the user in the model. (e.g. if the user types some text, add the text to the segment). The `calculate` function runs next and and works out all the downstream changes. (e.g. updating the statics, and starting evaluation), 

These two functions are separated for a couple reasons:

* An `update` on some specific ui element in the program may want to trigger a `calculate` everywhere else in the app (e.g. to re-evaluate the user's program). 

* Looking to the future, we will want to eventually use the Bonsai library to incrementalize the `calculate` step.

The result of `Update.update` is wrapped in a `Updated.t(...)` which, among other things, records whether the entire app should recalculate after this change. If you return `Updated.return(model)` it will recalculate, and if you return `Updated.return_quiet(model)` it won't recalculate. If you're not sure it's generally safer to use `return`. Look at the optional arguments on `return` if you want more control over what gets recalculated.

## Selection

The `Selection` module is only required if it's possible for this component or a component inside this component to be active (i.e. has the cursor, takes key events).

`Selection.t` is a data structure that can store where within this component the selection currently is.

The other functions in `Selection` help the app make decisions based on the current selection, e.g. what to do on key presses, what type to show at the bottom of the screen.

## View

The view function usually has the following signature:

```
let view =
      (
        ~globals,
        ~selected: option(Selection.t),
        ~inject: Update.t => Ui_effect.t(unit),
        ~signal: event => Ui_effect.t(unit),
        model: Model.t,
      ) => Node.t
```

`~globals` provides access to global values such as settings, fonts, etc. 

`~selected` tells you whether the current element is selected

`~inject` lets you perform an update action at this component, e.g. in response to a click or other user input

`~signal` is a way to propagate events, such as clicks, upwards to this component's parent.

## The Future

This system could be viewed as an in-between state, between the original implementation (with one large model and update type) and a fully-incremental Bonsai implementation (where subcomponent inclusion and downstream calculation are handled fully by Bonsai).

