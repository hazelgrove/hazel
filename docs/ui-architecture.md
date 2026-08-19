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

## Testing a component

`Update.update` and `Update.calculate` are ordinary functions over a `Model.t`, so
they can be driven directly from `test/`. No view layer, no browser. Existing
examples: `Test_CodeWithStatics`, `Test_EvalResult`, `Test_CodeEditable`,
`Test_CellEditor`, `Test_Page`, `Test_Editors`, `Test_StepperView`.

Two seams make the awkward parts reachable:

* **`~queue_worker=None`** (`EvalResult.Update.calculate`, and `CellEditor` /
  `CodeExerciseMode` / … above it) evaluates synchronously through
  `WorkerServer.evaluate_sync` instead of posting to a worker. Passing
  `Some(collector)` instead captures the requests the editor *would* have posted,
  which is how you observe request gating without evaluating anything.
* **`WorkerClient.use_worker := false`** makes `ScratchMode` choose that `None`
  path, so the whole page-level cycle is drivable. Without it, any `calculate`
  with dynamics on dies under node on `Worker is not a constructor`.

Things worth knowing before writing one of these tests:

* **Assert on behaviour, not allocation.** `CachedStatics.init` memoises below
  `CodeWithStatics`, so a "recomputed" frame can return a physically equal
  `info_map`. To tell reuse from recompute, hand `calculate` a model whose
  document has moved on while its `statics` still describes the previous one --
  the state an edit produces -- and ask which document the result describes.
  Where a recomputation does mint new ids (`StepperView`'s
  `Exp.replace_all_ids`), physical equality is a fair observable.
* **Thread `Updated.is_edit` into `calculate`**, as `Main.re` does. The statics
  debounce reads it, so a test that hardcodes `~is_edited=true` models a frame
  sequence the app never produces.
* **One stale frame is correct.** `StaticsDebounce.consume` returns
  `StaticsDefer` whenever `is_edited`, so the frame right after a keystroke
  deliberately shows the previous result; the scheduled `RefreshStatics` finishes
  the job. Assert both halves — "stale for one frame" and "stale forever" look
  identical from a single sample, and only the second reaches users.
* **Mutation-check anything about incrementality.** These decisions fail
  silently in both directions, so a green test proves little until you have seen
  it go red. Break the behaviour, watch the test fail, restore.

`Calc` itself is covered by `Test_Calc`, including that `let.calc` skips the work
when its input is old. That is the contract every `calculate` in the app depends
on.

### Known gap: the proof/derivation components

`AxiomsBox`'s filtered rewrite list is the one guard here that resists testing,
for a reason worth recording rather than rediscovering: with a plain expression
context it comes back empty, and an empty list or map in OCaml is a shared
constant, so `x === y` holds whether the value was reused or rebuilt. A reuse
test written that way passes vacuously -- including under a mutation that forces
a recompute, which is how the vacuity was caught. Covering it needs a fixture
with a real proof context (assumptions and propositions) so the list is
non-empty.

`MissingStep`'s assumption set does not have that problem -- it is seeded from
`Axioms.v` and is non-empty -- which is why `Test_MissingStep` can use physical
equality and its neighbour cannot. When a cached value might be empty, check
whether your test still fails under a mutation before trusting it.

`Test_Theorems` shows the shape of a real-fixture setup: evaluate an actual
`theorem` program with `enable_proof` on and feed the resulting `Dynamics.t` in,
with a fixture guard asserting the program really did contribute a theorem so the
rest cannot pass vacuously.

## The Future

This system could be viewed as an in-between state, between the original implementation (with one large model and update type) and a fully-incremental Bonsai implementation (where subcomponent inclusion and downstream calculation are handled fully by Bonsai).

