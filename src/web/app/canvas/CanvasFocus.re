open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Haz3lcore;

/* CanvasFocus — the detail strip below the constellation: one selected
   function, flattened into left→right dataflow. Unlike the holistic view,
   type OCCURRENCES are separate (Model-as-input and Model-as-output are
   two chips, so MVU loops unroll), and tuple inputs split into component
   slots. Each slot's sample well is a REAL probe view (CanvasProbe) over
   the anchor id (parameter pattern ids for inputs, the function's body
   result for the output), so display and interaction — One/Many
   windowing, sample focus, the 2D drawer — are the probe system's own. */

let node_of = (graph: CanvasGraph.t, key: string): option(CanvasGraph.tynode) =>
  List.find_opt((n: CanvasGraph.tynode) => n.key == key, graph.nodes);

let print_value = (v: Language.DHExp.t): string =>
  v
  |> ExpToSegment.exp_to_segment(
       ~settings=
         ExpToSegment.Settings.of_core(
           ~inline=true,
           Language.CoreSettings.off,
         ),
     )
  |> Printer.of_segment(~holes="");

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

let empty_well = (~probing: bool): Node.t =>
  div(
    ~attrs=[
      clss(["focus-well", "well-empty"]),
      Attr.title(
        probing
          ? "no values observed at this position (not reached?)"
          : "no samples: enable probe-all (or place a probe) and run",
      ),
    ],
    [
      span(~attrs=[clss(["well-glyph"])], [text({js|≡|js})]),
      text(" —"),
    ],
  );

let slot =
    (
      ~globals: Globals.t,
      ~editor: CodeWithStatics.Model.t,
      ~inject_jump,
      ~graph,
      ~probing,
      ~role,
      ~probe_key: string,
      (key: string, sample_id: option(Id.t)),
    )
    : Node.t => {
  let well =
    switch (sample_id) {
    | Some(id) =>
      switch (CanvasProbe.view(~globals, ~editor, ~key=probe_key, id)) {
      | Some(v) => v
      | None => empty_well(~probing)
      }
    | None => empty_well(~probing)
    };
  div(
    ~attrs=[clss(["focus-slot"])],
    [chip(~inject_jump, ~graph, ~role, key), well],
  );
};

/* ---------- type focus: observed inhabitants across all probe sites ---------- */

let max_type_values = 10;

/* pretty type of a probed site, from statics */
let site_ty = (~info_map: Language.Statics.Map.t, id: Id.t): option(string) =>
  switch (Id.Map.find_opt(id, info_map)) {
  | Some(InfoExp(e)) =>
    Some(CanvasGraph.pretty_ty(Language.Info.exp_ty(e)))
  | Some(InfoPat(p)) =>
    Some(CanvasGraph.pretty_ty(Language.Info.pat_ty(p)))
  | _ => None
  };

let type_view =
    (
      ~globals: Globals.t,
      ~editor: CodeWithStatics.Model.t,
      ~inject_jump,
      ~on_close: Effect.t(unit),
      ~dynamics: Language.Dynamics.Map.t,
      ~info_map: Language.Statics.Map.t,
      ~graph: CanvasGraph.t,
      key: string,
    )
    : option(Node.t) =>
  node_of(graph, key)
  |> Option.map((n: CanvasGraph.tynode) => {
       let probing = globals.settings.core.probe_all;
       /* a site inhabits this type if its pretty type matches the node's
          name or (for aliases) its body */
       /* the type this node stands for. Former nodes ("()@Model",
          "[]@Hand") represent their ALIAS's body type, not unit/[?];
          implicit products' keys are literal type syntax ("(Todo, Int)") */
       let former_anchor = (prefix: string): option(string) => {
         let pl = String.length(prefix);
         String.length(key) > pl && String.sub(key, 0, pl) == prefix
           ? Some(String.sub(key, pl, String.length(key) - pl)) : None;
       };
       let names =
         switch (n.kind) {
         | Product =>
           switch (former_anchor("()@"), former_anchor("[]@")) {
           | (Some(alias), _)
           | (_, Some(alias)) =>
             switch (node_of(graph, alias)) {
             | Some({n_ty: Some(body), _}) => [body]
             | _ => [alias]
             }
           | (None, None) => [n.key]
           }
         | _ =>
           [n.label]
           @ (
             switch (n.n_ty) {
             | Some(b) when b != n.label => [b]
             | _ => []
             }
           )
         };
       let tally: Hashtbl.t(string, (int, int, Language.Sample.t)) =
         Hashtbl.create(16);
       Language.Sample.Map.fold(
         (id, samples, ()) =>
           switch (site_ty(~info_map, id)) {
           | Some(t) when List.mem(t, names) =>
             List.iter(
               (s: Language.Sample.t) => {
                 let v = print_value(s.value);
                 let (c, latest, repr) =
                   Option.value(
                     ~default=(0, 0, s),
                     Hashtbl.find_opt(tally, v),
                   );
                 Hashtbl.replace(
                   tally,
                   v,
                   (c + 1, max(latest, s.seq), s.seq >= latest ? s : repr),
                 );
               },
               samples,
             )
           | _ => ()
           },
         dynamics,
         (),
       );
       let entries =
         Hashtbl.fold(
           (v, (c, latest, repr), acc) => [(v, c, latest, repr), ...acc],
           tally,
           [],
         )
         |> List.sort(((_, _, a, _), (_, _, b, _)) => compare(b, a));
       let shown = entries |> List.filteri((i, _) => i < max_type_values);
       let total_obs =
         List.fold_left((acc, (_, c, _, _)) => acc + c, 0, entries);
       let head =
         div(
           ~attrs=[clss(["focus-head"])],
           [
             span(
               ~attrs=
                 [clss(["focus-name"])]
                 @ (
                   switch (n.n_id) {
                   | Some(id) => [
                       Attr.on_click(_ => inject_jump(id)),
                       Attr.title("select definition"),
                     ]
                   | None => []
                   }
                 ),
               [text(n.label)],
             ),
           ]
           @ (
             switch (n.n_ty) {
             | Some(b) when b != n.label => [
                 span(~attrs=[clss(["focus-ty"])], [text(" = " ++ b)]),
               ]
             | _ => []
             }
           )
           @ [
             span(
               ~attrs=[clss(["focus-ty"])],
               [
                 text(
                   Printf.sprintf(
                     "  ·  %d distinct value(s), %d observation(s)",
                     List.length(entries),
                     total_obs,
                   ),
                 ),
               ],
             ),
           ]
           @ (
             probing
               ? []
               : [
                 div(
                   ~attrs=[
                     clss(["focus-probe-btn"]),
                     Attr.on_click(_ =>
                       globals.inject_global(Set(ProbeAll))
                     ),
                     Attr.title(
                       "turn on probe-all so values of this type are sampled",
                     ),
                   ],
                   [text("collect samples")],
                 ),
               ]
           )
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
         switch (n.n_doc) {
         | Some(d) => [div(~attrs=[clss(["focus-doc"])], [text(d)])]
         | None => []
         };

       /* value STRIP, not a probe: one chip per distinct value with
          counts; clicking captures that occurrence */
       let chips =
         shown
         |> List.filter_map(((_, c, _, repr: Language.Sample.t)) =>
              CanvasProbe.value_chip(~globals, ~editor, ~count=c, repr)
            );
       let agg_nodes =
         chips == []
           ? []
           : [
             div(
               ~attrs=[clss(["type-values", "type-agg-well"])],
               chips
               @ (
                 List.length(entries) > max_type_values
                   ? [
                     div(
                       ~attrs=[clss(["well-more"])],
                       [
                         text(
                           Printf.sprintf(
                             "+%d more",
                             List.length(entries) - max_type_values,
                           ),
                         ),
                       ],
                     ),
                   ]
                   : []
               ),
             ),
           ];
       let values_row =
         agg_nodes != []
           ? agg_nodes
           : shown == []
               ? [
                 div(
                   ~attrs=[clss(["type-values-empty"])],
                   [
                     text(
                       probing
                         ? "no values of this type observed"
                         : "no samples — collect samples and run",
                     ),
                   ],
                 ),
               ]
               : [
                 div(
                   ~attrs=[clss(["type-values"])],
                   List.map(
                     ((v, c, _, repr: Language.Sample.t)) =>
                       div(
                         ~attrs=[clss(["type-value"]), Attr.title(v)],
                         [
                           CanvasValue.chip(
                             ~font_metrics=globals.font_metrics,
                             ~available=34,
                             repr.value,
                           ),
                         ]
                         @ (
                           c > 1
                             ? [
                               span(
                                 ~attrs=[clss(["type-value-count"])],
                                 [text(Printf.sprintf({js| ×%d|js}, c))],
                               ),
                             ]
                             : []
                         ),
                       ),
                     shown,
                   )
                   @ (
                     List.length(entries) > max_type_values
                       ? [
                         div(
                           ~attrs=[clss(["well-more"])],
                           [
                             text(
                               Printf.sprintf(
                                 "+%d more",
                                 List.length(entries) - max_type_values,
                               ),
                             ),
                           ],
                         ),
                       ]
                       : []
                   ),
                 ),
               ];
       div(~attrs=[clss(["canvas-focus"])], [head] @ doc @ values_row);
     });

let view =
    (
      ~globals: Globals.t,
      ~editor: CodeWithStatics.Model.t,
      ~inject_jump,
      ~on_close: Effect.t(unit),
      ~ask_agent: option(CanvasGraph.edge => Effect.t(unit))=None,
      ~graph: CanvasGraph.t,
      name: string,
    )
    : option(Node.t) =>
  List.find_opt((e: CanvasGraph.edge) => e.e_name == name, graph.edges)
  |> Option.map((e: CanvasGraph.edge) => {
       let probing = globals.settings.core.probe_all;
       /* pair input slots with anchors: a Product source pairs its parts
          with the flattened component ids; a single named source whose
          pattern destructures (flip : Card -> Card, fun (s, r)) anchors
          at the WHOLE pattern so the well carries the pair */
       let input_slots =
         switch (node_of(graph, e.e_src)) {
         | Some({kind: Product, parts, _}) =>
           List.mapi(
             (i, key) => (key, List.nth_opt(e.e_arg_ids, i)),
             parts,
           )
         | _ =>
           let anchor =
             List.length(e.e_arg_ids) > 1
               ? List.nth_opt(e.e_whole_ids, 0)
               : List.nth_opt(e.e_arg_ids, 0);
           [(e.e_src, anchor)];
         };
       let head =
         div(
           ~attrs=[clss(["focus-head"])],
           [
             span(
               ~attrs=[
                 clss(["focus-name"] @ (e.e_hole ? ["edge-hole"] : [])),
                 Attr.on_click(_ => inject_jump(e.e_id)),
                 Attr.title("select definition"),
               ],
               [text(e.e_name)],
             ),
             span(~attrs=[clss(["focus-ty"])], [text(" : " ++ e.e_ty)]),
             ...List.map(CanvasView.test_pip, e.tests),
           ]
           @ (
             switch (e.e_hole ? ask_agent : None) {
             | Some(ask) => [
                 div(
                   ~attrs=[
                     clss(["focus-probe-btn", "focus-ask-btn"]),
                     Attr.on_click(_ => ask(e)),
                     Attr.title(
                       "post this unwritten function to the agent chat as a task",
                     ),
                   ],
                   [text("@ ask agent to implement")],
                 ),
               ]
             | None => []
             }
           )
           @ (
             probing
               ? []
               : [
                 div(
                   ~attrs=[
                     clss(["focus-probe-btn"]),
                     Attr.on_click(_ =>
                       globals.inject_global(Set(ProbeAll))
                     ),
                     Attr.title(
                       "turn on probe-all so values flowing through this function are sampled",
                     ),
                   ],
                   [text("collect samples")],
                 ),
               ]
           )
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
       let mk_slot = (~role, ~probe_key, sl) =>
         slot(
           ~globals,
           ~editor,
           ~inject_jump,
           ~graph,
           ~probing,
           ~role,
           ~probe_key,
           sl,
         );
       let flow =
         div(
           ~attrs=[clss(["focus-flow"])],
           [
             div(
               ~attrs=[clss(["focus-inputs"])],
               List.mapi(
                 (i, sl) =>
                   mk_slot(
                     ~role="role-in",
                     ~probe_key=Printf.sprintf("%s/in%d", e.e_name, i),
                     sl,
                   ),
                 input_slots,
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
               [
                 mk_slot(
                   ~role="role-out",
                   ~probe_key=e.e_name ++ "/out",
                   (e.dst, e.e_out_id),
                 ),
               ],
             ),
           ],
         );
       div(~attrs=[clss(["canvas-focus"])], [head] @ doc @ [flow]);
     });
