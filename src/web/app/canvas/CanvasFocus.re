open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

/* CanvasFocus — the detail strip below the constellation: one selected
   function, flattened into left→right dataflow. Unlike the holistic view,
   type OCCURRENCES are separate (Model-as-input and Model-as-output are
   two chips, so MVU loops unroll), and tuple inputs split into component
   slots. Each slot carries a sample well — the landing site for probe
   values (values of that type observed flowing through this function). */

let node_of = (graph: CanvasGraph.t, key: string): option(CanvasGraph.tynode) =>
  List.find_opt((n: CanvasGraph.tynode) => n.key == key, graph.nodes);

let chip =
    (~inject_jump, ~graph: CanvasGraph.t, ~role: string, key: string): Node.t => {
  let (label, kind_cls, jump) =
    switch (node_of(graph, key)) {
    | Some(n) => (
        n.label,
        CanvasView.kind_cls(n.kind),
        switch (n.n_id) {
        | Some(id) => [Attr.on_click(_ => inject_jump(id))]
        | None => []
        },
      )
    | None => (key, "kind-ghost", [])
    };
  div(
    ~attrs=[clss(["focus-chip", kind_cls, role]), ...jump],
    [text(label)],
  );
};

let well = (): Node.t =>
  div(
    ~attrs=[
      clss(["focus-well"]),
      Attr.title(
        "probe samples will appear here: values observed flowing through this position",
      ),
    ],
    [
      span(~attrs=[clss(["well-glyph"])], [text({js|≡|js})]),
      text(" —"),
    ],
  );

let slot = (~inject_jump, ~graph, ~role, key: string): Node.t =>
  div(
    ~attrs=[clss(["focus-slot"])],
    [chip(~inject_jump, ~graph, ~role, key), well()],
  );

let view =
    (
      ~inject_jump,
      ~on_close: Effect.t(unit),
      ~graph: CanvasGraph.t,
      name: string,
    )
    : option(Node.t) =>
  List.find_opt((e: CanvasGraph.edge) => e.e_name == name, graph.edges)
  |> Option.map((e: CanvasGraph.edge) => {
       let input_keys =
         switch (node_of(graph, e.e_src)) {
         | Some({kind: Product, parts, _}) => parts
         | _ => [e.e_src]
         };
       let head =
         div(
           ~attrs=[clss(["focus-head"])],
           [
             span(
               ~attrs=[
                 clss(["focus-name"] @ (e.e_hole ? ["edge-hole"] : [])),
                 Attr.on_click(_ => inject_jump(e.e_id)),
                 Attr.title("jump to definition"),
               ],
               [text(e.e_name)],
             ),
             span(~attrs=[clss(["focus-ty"])], [text(" : " ++ e.e_ty)]),
             ...List.map(CanvasView.test_pip, e.tests),
           ]
           @ [
             div(
               ~attrs=[
                 clss(["focus-close"]),
                 Attr.on_click(_ => on_close),
                 Attr.title("close"),
               ],
               [text({js|✕|js})],
             ),
           ],
         );
       let doc =
         switch (e.e_doc) {
         | Some(d) => [div(~attrs=[clss(["focus-doc"])], [text(d)])]
         | None => []
         };
       let flow =
         div(
           ~attrs=[clss(["focus-flow"])],
           [
             div(
               ~attrs=[clss(["focus-inputs"])],
               List.map(
                 slot(~inject_jump, ~graph, ~role="role-in"),
                 input_keys,
               ),
             ),
             div(
               ~attrs=[
                 clss(["focus-fn"] @ (e.e_hole ? ["edge-hole"] : [])),
               ],
               [
                 span(~attrs=[clss(["focus-arrow"])], [text({js|⟶|js})]),
                 div(
                   ~attrs=[
                     clss(["focus-fnbox"]),
                     Attr.on_click(_ => inject_jump(e.e_id)),
                   ],
                   [text(e.e_name)],
                 ),
                 span(
                   ~attrs=[clss(["focus-arrow"])],
                   [text({js|⟶|js})],
                 ),
               ],
             ),
             div(
               ~attrs=[clss(["focus-output"])],
               [slot(~inject_jump, ~graph, ~role="role-out", e.dst)],
             ),
           ],
         );
       div(~attrs=[clss(["canvas-focus"])], [head] @ doc @ [flow]);
     });
