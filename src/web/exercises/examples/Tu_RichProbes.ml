open Haz3lcore

let exercise : Tutorial.spec =
  {
    id = Option.get (Id.of_string "b2000001-0001-0001-0001-000000000001");
    title = "Rich Probes";
    module_name = "Tu_RichProbes";
    version = 1;
    prompt =
      {md|**Rich probes** display tabular data — lists of labeled tuples — as interactive tables in the editor. When you add a probe to a table expression and press the **table button**, the rich probe shows a **⋮** menu button beside each column name with options for transforming or filtering columns. These actions rewrite the source code directly.

## Task

The code below defines a `products` table with `name`, `price`, and `qty` columns. Add a computed `total` column:

1. **Add a probe** on `products` in the `with_totals` binding (right-click → **"Add probe"**, or **Cmd+E** / **Ctrl+E**).
2. Press the **table button** to switch to the rich table view.
3. **Convert `qty` to Float**: Click the **⋮** button next to `qty`, select **Transform →**, then **Float**. This rewrites the source code so `qty` values are floats.
4. **Add a new column**: Click the **+** button on the right side of the table header and name it `total`.
5. In the textual source code, fill in the hole in the new column's expression with: `r.price *. r.qty`. The table updates live as you edit the code.|md};
    display_hint =
      "After adding a probe and pressing the table button, first convert qty \
       to Float using the ⋮ button → Transform → Float. Then click the + \
       button on the right side of the table header to add a column named \
       total. In the source code, fill in the expression r.price *. r.qty";
    task_reference =
      TaskRefDocs.compose
        [
          TaskRefDocs.adding_a_probe;
          TaskRefDocs.rich_probe_table;
          TaskRefDocs.column_projection;
          TaskRefDocs.float_arithmetic;
        ];
    wrapper = false;
    show_report = true;
    setting_overrides =
      {
        Tutorial.default_setting_overrides with
        rich_probes = Some true;
        display_tables = Some true;
      };
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
            "Make sure with_totals has a total column computed as price *. qty \
             (convert qty to Float first using Transform → Float)";
          ];
      };
  }
