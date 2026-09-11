open Haz3lcore

let exercise : Tutorial.spec =
  {
    id = Option.get (Id.of_string "b2000001-0001-0001-0001-000000000001");
    title = "Rich Probes";
    module_name = "Tu_RichProbes";
    version = 1;
    prompt =
      {md|**Rich probes** display tabular data — lists of labeled tuples — as interactive tables in the editor. Click a probe's **probe sample** to open its menu, then choose **View as table**; the rich probe then shows a **⋮** menu button beside each column name with options for transforming or filtering columns. These actions rewrite the source code directly.

## Task

The code below defines a `products` table with `name`, `price`, and `qty` columns. Add a computed `total` column:

1. **Add a probe** on `products` in the `with_totals` binding (right-click → **"Add probe"**, or **Cmd+E** / **Ctrl+E**).
2. **Click the probe sample** — the value shown to the right — to open its menu, then choose **View as table**.
3. **Convert `qty` to Float**: Click the **⋮** button next to `qty`, select **Transform →**, then **Float**. This rewrites the source code so `qty` values are floats.
4. **Add a new column**: Click the **+** button on the right side of the table header. The new column arrives as two holes — one for its label, one for its value.
5. In the textual source code, fill the label hole with `total` and the value hole with `r.price *. r.qty`. The table updates live as you edit the code.|md};
    display_hint =
      "Add a probe, click its sample and choose \"View as table\". Convert qty \
       to Float with the ⋮ button → Transform → Float, then click + to add a \
       column. In the source code, name the new column total and give it the \
       expression r.price *. r.qty";
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
    your_impl =
      Option.get
        (Parser.to_zipper ~root:Exp
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
            (Parser.to_zipper ~root:Exp
               "test nth(with_totals, 1).total ==. 24.5 end\n");
        hints =
          [
            "Make sure with_totals has a total column computed as price *. qty \
             (convert qty to Float first using Transform → Float)";
          ];
      };
  }
