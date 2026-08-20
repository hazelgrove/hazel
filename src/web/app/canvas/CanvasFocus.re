open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Haz3lcore;

/* CanvasFocus — the detail strip below the constellation: one selected
   function, flattened into left→right dataflow. Unlike the holistic view,
   type OCCURRENCES are separate (Model-as-input and Model-as-output are
   two chips, so MVU loops unroll), and tuple inputs split into component
   slots. Each slot carries a sample well showing probe values observed
   flowing through that position (parameter pattern ids for inputs, the
   function's body id for the output). */

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

let max_shown = 3;

/* newest-first sample strings at a probed id */
let samples_at =
    (~dynamics: Language.Dynamics.Map.t, id: Id.t): (list(string), int) => {
  let all =
    Language.Dynamics.Map.lookup(id, dynamics) |> Option.value(~default=[]);
  let strs =
    all
    |> List.rev  /* evaluation order → newest first */
    |> List.filteri((i, _) => i < max_shown)
    |> List.map((s: Language.Sample.t) => print_value(s.value));
  (strs, List.length(all));
};

let truncate = (n: int, s: string): string =>
  String.length(s) > n ? String.sub(s, 0, n - 1) ++ {js|…|js} : s;

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

let well = (~probing: bool, (samples, total): (list(string), int)): Node.t =>
  switch (samples) {
  | [] =>
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
    )
  | _ =>
    div(
      ~attrs=[
        clss(["focus-well", "well-filled"]),
        Attr.title(
          Printf.sprintf(
            "%d observed value(s), newest first:\n%s",
            total,
            String.concat("\n", samples),
          ),
        ),
      ],
      [span(~attrs=[clss(["well-glyph"])], [text({js|≡|js})])]
      @ List.map(
          s =>
            div(~attrs=[clss(["well-value"])], [text(truncate(26, s))]),
          samples,
        )
      @ (
        total > max_shown
          ? [
            div(
              ~attrs=[clss(["well-more"])],
              [text(Printf.sprintf("+%d", total - max_shown))],
            ),
          ]
          : []
      ),
    )
  };

let slot =
    (
      ~inject_jump,
      ~graph,
      ~dynamics,
      ~probing,
      ~role,
      (key: string, sample_id: option(Id.t)),
    )
    : Node.t => {
  let samples =
    switch (sample_id) {
    | Some(id) => samples_at(~dynamics, id)
    | None => ([], 0)
    };
  div(
    ~attrs=[clss(["focus-slot"])],
    [chip(~inject_jump, ~graph, ~role, key), well(~probing, samples)],
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
       let names =
         [n.label]
         @ (
           switch (n.n_ty) {
           | Some(b) when b != n.label => [b]
           | _ => []
           }
         );
       let tally: Hashtbl.t(string, (int, int)) = Hashtbl.create(16);
       Language.Sample.Map.fold(
         (id, samples, ()) =>
           switch (site_ty(~info_map, id)) {
           | Some(t) when List.mem(t, names) =>
             List.iter(
               (s: Language.Sample.t) => {
                 let v = print_value(s.value);
                 let (c, latest) =
                   Option.value(
                     ~default=(0, 0),
                     Hashtbl.find_opt(tally, v),
                   );
                 Hashtbl.replace(tally, v, (c + 1, max(latest, s.seq)));
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
           (v, (c, latest), acc) => [(v, c, latest), ...acc],
           tally,
           [],
         )
         |> List.sort(((_, _, a), (_, _, b)) => compare(b, a));
       let shown = entries |> List.filteri((i, _) => i < max_type_values);
       let total_obs =
         List.fold_left((acc, (_, c, _)) => acc + c, 0, entries);
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
       let values_row =
         shown == []
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
                 ((v, c, _)) =>
                   div(
                     ~attrs=[clss(["type-value"]), Attr.title(v)],
                     [text(truncate(36, v))]
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
      ~inject_jump,
      ~on_close: Effect.t(unit),
      ~dynamics: Language.Dynamics.Map.t,
      ~graph: CanvasGraph.t,
      name: string,
    )
    : option(Node.t) =>
  List.find_opt((e: CanvasGraph.edge) => e.e_name == name, graph.edges)
  |> Option.map((e: CanvasGraph.edge) => {
       let probing = globals.settings.core.probe_all;
       let input_keys =
         switch (node_of(graph, e.e_src)) {
         | Some({kind: Product, parts, _}) => parts
         | _ => [e.e_src]
         };
       /* pair input slots with parameter ids positionally */
       let input_slots =
         List.mapi(
           (i, key) => (key, List.nth_opt(e.e_arg_ids, i)),
           input_keys,
         );
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
       let mk_slot = slot(~inject_jump, ~graph, ~dynamics, ~probing);
       let flow =
         div(
           ~attrs=[clss(["focus-flow"])],
           [
             div(
               ~attrs=[clss(["focus-inputs"])],
               List.map(mk_slot(~role="role-in"), input_slots),
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
               [mk_slot(~role="role-out", (e.dst, e.e_out_id))],
             ),
           ],
         );
       div(~attrs=[clss(["canvas-focus"])], [head] @ doc @ [flow]);
     });
