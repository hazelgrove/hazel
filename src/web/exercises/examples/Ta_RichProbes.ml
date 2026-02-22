open Haz3lcore

let exercise : Tutorial.spec =
  {
    id = Option.get (Id.of_string "b2000001-0001-0001-0001-000000000001");
    title = "Rich Probes";
    module_name = "Ta_RichProbes";
    version = 1;
    prompt =
      {md|**Rich probes** display tabular data — lists of labeled tuples — as interactive tables in the editor. When you add a probe to a table expression and press the **table button**, the rich probe shows a **⋮** menu button beside each column name with options for adding columns or changing types. These actions rewrite the source code directly.

## Task

The code below defines a `products` table with `name`, `price`, and `qty` columns. Add a computed `total` column:

1. **Add a probe** on `products` in the `with_totals` binding (right-click → **"Add probe"**, or **Cmd+E** / **Ctrl+E**), then press the **table button**.
2. Add a new column `total`. Click the **⋮** button next to a column name and select **Add Column**. This rewrites the source code to include a new column.
3. In the textual source code, fill in the expression: `row.price *. float_of_int(row.qty)`. The table updates live as you edit the code.|md};
    display_hint =
      "After adding a probe and pressing the table button, click the ⋮ button \
       next to a column name and select Add Column. This rewrites the source \
       code — name the column total and use the expression row.price *. \
       float_of_int(row.qty)";
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
          - **Change a column's type**\n\n\
          Column actions **rewrite the underlying textual syntax**. After \
          performing an action, fill in the column's value expression directly \
          in the source code."
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
    setting_overrides =
      { rich_probes = Some true; display_tables = Some true; read_only = None };
    your_impl =
      Option.get
        (Haz3lcore.Parser.to_zipper
           "let products = ^^table([\n\
            (name=\"Widget\", price=9.99, qty=3),\n\
            (name=\"Gadget\", price=24.50, qty=1),\n\
            (name=\"Gizmo\", price=4.75, qty=10)\n\
            ]) in\n\
            let with_totals = products in\n\
            with_totals");
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
