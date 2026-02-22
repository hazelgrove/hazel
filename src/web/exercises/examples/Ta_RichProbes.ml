open Haz3lcore

let exercise : Tutorial.spec =
  {
    id = Option.get (Id.of_string "b2000001-0001-0001-0001-000000000001");
    title = "Rich Probes";
    module_name = "Ta_RichProbes";
    version = 1;
    prompt =
      {md|**Rich probes** enhance the basic probe display by showing tabular data — lists of labeled tuples — as interactive tables directly in the editor.

## How Rich Probes Work

When you probe an expression that evaluates to a **list of labeled tuples** (i.e. a table), and then press the **table button** on the probe, the rich probe renders it as an interactive table instead of raw syntax. The rich probe interface provides **action buttons** to the right of each column name that let you add new columns or change a column's type — directly from the table view.

## Task

The code below defines a `products` table with `name`, `price`, and `qty` columns. Currently `with_totals` is just set to `products` unchanged.

1. **Add a probe** on `products` in the `with_totals` binding (right-click and select **"Add probe"**, or press **Cmd+E** / **Ctrl+E**). Then press the **table button** on the probe to see the rich table interface.
2. **Add a new column** using the rich probe interface: click the **add column button** (to the right of the column names) and set the new column equal to an expression computing the total — for example, `row.price *. float_of_int(row.qty)`.
3. Observe how the rich probe table updates to show the new column with computed values for each row.

## Explore

After completing the task, take some time to play with the rich probe interface. Try changing column types, adding other computed columns, or probing different expressions in the code to see how the table view updates.|md};
    display_hint =
      "After adding a probe and pressing the table button, look for the add \
       column button to the right of the column names. Use it to add a new \
       column with an expression like: row.price *. float_of_int(row.qty)";
    task_reference =
      (let adding_a_probe =
         "### Adding a Probe\n\
          Right-click an expression and choose **\"Add probe\"**, or press \
          **Cmd+E** / **Ctrl+E**."
       in
       let rich_probe_table =
         "### Rich Probe Table Interface\n\
          After adding a probe, press the **table button** to switch to the \
          rich table view.\n\n\
          The rich probe displays **action buttons** to the right of each \
          column name. Use these to:\n\
          - **Add a new column** with a computed expression\n\
          - **Change a column's type**"
       in
       TaskRefDocs.compose
         [
           adding_a_probe;
           rich_probe_table;
           TaskRefDocs.column_projection;
           TaskRefDocs.type_conversions_float_of_int;
           TaskRefDocs.float_arithmetic;
         ]);
    wrapper = false;
    show_report = true;
    setting_overrides = { rich_probes = Some true; display_tables = Some true };
    your_impl =
      Option.get
        (Haz3lcore.Parser.to_zipper
           "let products = ^^table([\n\
            (name=\"Widget\", price=9.99, qty=3),\n\
            (name=\"Gadget\", price=24.50, qty=1),\n\
            (name=\"Gizmo\", price=4.75, qty=10)\n\
            ]) in\n\
            let with_totals = products in\n\
            with_totals\n");
    hidden_tests =
      {
        tests =
          Option.get
            (Haz3lcore.Parser.to_zipper
               "test nth(with_totals, 1).total ==. 24.5 end\n");
        hints =
          [
            "Make sure with_totals has a total column computed as price *. \
             float_of_int(qty)";
          ];
      };
  }
