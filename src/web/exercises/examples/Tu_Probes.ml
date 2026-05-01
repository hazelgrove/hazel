open Haz3lcore

let exercise : Tutorial.spec =
  {
    id = Option.get (Id.of_string "a0000020-0020-0020-0020-000000000020");
    title = "Probes";
    module_name = "Tu_Probes";
    version = 1;
    prompt =
      {md|**Probes** let you inspect runtime values directly in the editor — no print statements needed. You place a **probe** on an expression or pattern, and the values that appear to the right are called **probe samples**.

## How to Add a Probe

1. **Right-click** any expression and select **"Add probe"**, or
2. Place your cursor on an expression and press **Cmd+E** (Mac) / **Ctrl+E** (Windows/Linux).

A **probe sample** will appear to the right showing the value of that expression. Learning to use probes now will help you debug the rest of the tutorial tasks.

## Probe Samples Inside Functions

When you probe an expression **inside a function**, the probe will have a **probe sample** for **every call** to that function. This makes it easy to see how a function behaves across different inputs. If the probe sample shows `...`, you can **double-click** it to expand and see all the values from every call.

## Truncated Probe Samples

Large values may be abbreviated in the probe sample. If a probe sample appears truncated, you can **Shift+click and drag** on it to expand and inspect the full value.

## Task

The code below defines a function `add_tax` that adds a 50% tax to a price. It is then applied to a list of three prices using `map`.

1. **Add a probe** on `price` inside the function body to see the three input values.
The probe sample will show `...` because there are multiple values. **Double-click** the `...` to expand and see all three probe samples.
2. **Add a probe** on the full body expression `price +. price *. 0.5` — to do this, right-click or place your cursor on the `+.` operator (which is the root of the expression) and select "Add probe".
**Note:** you cannot drag-select a block of code and then add a probe; the probe attaches to the expression at the cursor position.
3. **Add a probe** on `totals` to see the final result list.
Try **Shift+click and drag** on the probe sample to resize it and see the full value.|md};
    display_hint =
      "Right-click an expression and select \"Add probe\",\n\
       or place your cursor on it and press Cmd+E / Ctrl+E.";
    task_reference =
      {md|## Task Reference

### Adding a Probe
Right-click an expression and choose **"Add probe"**, or press **Cmd+E** / **Ctrl+E**. The values that appear to the right are called **probe samples**.

### Removing a Probe
Right-click a probed expression and choose **"Remove probe"**, or press **Cmd+E** / **Ctrl+E** again.

### Probe Samples Inside Functions
Probing inside a function shows a probe sample for each call:
```hazel
let f = fun x -> x + 1 in
map([10, 20], f)
```
Probing `x + 1` shows probe samples: `11`, `21`|md};
    wrapper = false;
    show_report = false;
    setting_overrides = Tutorial.default_setting_overrides;
    your_impl =
      Option.get
        (Haz3lcore.Parser.to_zipper ~root:Exp
           "let add_tax = fun price -> price +. price *. 0.5 in\n\
            let totals = map([3.0, 7.0, 5.0], add_tax) in\n\
            totals\n");
    hidden_tests =
      {
        tests =
          {
            selection = { focus = Left; content = []; mode = Normal };
            relatives =
              {
                siblings =
                  ( [
                      Tile
                        {
                          id = Id.mk ();
                          label = [ "test"; "end" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ Exp ];
                              nibs =
                                ( { shape = Convex; sort = Exp },
                                  { shape = Convex; sort = Exp } );
                            };
                          shards = [ 0; 1 ];
                          children =
                            [
                              [
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                                Tile
                                  {
                                    id = Id.mk ();
                                    label = [ "true" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                              ];
                            ];
                        };
                    ],
                    [ Grout { id = Id.mk (); shape = Convex } ] );
                ancestors = [];
              };
            caret = Outer;
            refractors = Haz3lcore.ZipperBase.Refractor.init;
          };
        hints = [];
      };
  }
