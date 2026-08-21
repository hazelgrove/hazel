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
let chip_budget = 24;

/* newest-first samples at a probed id */
let samples_at =
    (~dynamics: Language.Dynamics.Map.t, id: Id.t)
    : (list(Language.Sample.t), int) => {
  let all =
    Language.Dynamics.Map.lookup(id, dynamics) |> Option.value(~default=[]);
  (List.rev(all) /* evaluation order → newest first */, List.length(all));
};

let truncate = (n: int, s: string): string =>
  String.length(s) > n ? String.sub(s, 0, n - 1) ++ {js|…|js} : s;

/* Expanded 2D sample panel, shared by the fn and type views. */
let expand_panel =
    (
      ~globals: Globals.t,
      ~cols: int,
      ~label: string,
      ~nav: option((int, int, int => Effect.t(unit))),
      ~on_collapse: Effect.t(unit),
      v: Language.DHExp.t,
    )
    : Node.t => {
  let nav_nodes =
    switch (nav) {
    | Some((idx, total, go)) when total > 1 => [
        div(
          ~attrs=[
            clss(["focus-expand-nav"] @ (idx <= 0 ? ["nav-disabled"] : [])),
            Attr.on_click(_ => idx <= 0 ? Effect.Ignore : go(idx - 1)),
            Attr.title("newer sample"),
          ],
          [text({js|‹|js})],
        ),
        span(
          ~attrs=[clss(["focus-expand-count"])],
          [text(Printf.sprintf("%d/%d", idx + 1, total))],
        ),
        div(
          ~attrs=[
            clss(
              ["focus-expand-nav"]
              @ (idx >= total - 1 ? ["nav-disabled"] : []),
            ),
            Attr.on_click(_ =>
              idx >= total - 1 ? Effect.Ignore : go(idx + 1)
            ),
            Attr.title("older sample"),
          ],
          [text({js|›|js})],
        ),
      ]
    | _ => []
    };
  div(
    ~attrs=[clss(["focus-expand"])],
    [
      div(
        ~attrs=[clss(["focus-expand-head"])],
        [span(~attrs=[clss(["focus-expand-label"])], [text(label)])]
        @ nav_nodes
        @ [
          div(
            ~attrs=[
              clss(["focus-close"]),
              Attr.on_click(_ => on_collapse),
              Attr.title("collapse"),
            ],
            [text({js|✕|js})],
          ),
        ],
      ),
      div(
        ~attrs=[clss(["focus-expand-body"])],
        [CanvasValue.expanded(~font_metrics=globals.font_metrics, ~cols, v)],
      ),
    ],
  );
};

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

let well =
    (
      ~globals: Globals.t,
      ~probing: bool,
      ~pick: option(int => Effect.t(unit)),
      (samples, total): (list(Language.Sample.t), int),
    )
    : Node.t =>
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
    let shown = samples |> List.filteri((i, _) => i < max_shown);
    div(
      ~attrs=[
        clss(["focus-well", "well-filled"]),
        Attr.title(
          Printf.sprintf(
            "%d observed value(s), newest first:\n%s%s",
            total,
            shown
            |> List.map((s: Language.Sample.t) => print_value(s.value))
            |> String.concat("\n"),
            Option.is_some(pick) ? "\n(click a value to expand)" : "",
          ),
        ),
      ],
      [span(~attrs=[clss(["well-glyph"])], [text({js|≡|js})])]
      @ List.mapi(
          (i, s: Language.Sample.t) =>
            div(
              ~attrs=
                [
                  clss(["well-value"] @ (pick == None ? [] : ["well-pick"])),
                ]
                @ (
                  switch (pick) {
                  | Some(go) => [Attr.on_click(_ => go(i))]
                  | None => []
                  }
                ),
              [
                CanvasValue.chip(
                  ~font_metrics=globals.font_metrics,
                  ~available=chip_budget,
                  s.value,
                ),
              ],
            ),
          shown,
        )
      @ (
        total > max_shown
          ? [
            div(
              ~attrs=
                [clss(["well-more"] @ (pick == None ? [] : ["well-pick"]))]
                @ (
                  switch (pick) {
                  | Some(go) => [
                      Attr.on_click(_ => go(max_shown)),
                      Attr.title("expand older samples"),
                    ]
                  | None => []
                  }
                ),
              [text(Printf.sprintf("+%d", total - max_shown))],
            ),
          ]
          : []
      ),
    );
  };

let slot =
    (
      ~globals: Globals.t,
      ~inject_jump,
      ~graph,
      ~dynamics,
      ~probing,
      ~pick: option(int => Effect.t(unit)),
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
    [
      chip(~inject_jump, ~graph, ~role, key),
      well(~globals, ~probing, ~pick, samples),
    ],
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

/* Columns available to the expanded sample view, from the strip's pixel
   width (falling back generously when unmeasured). */
let expand_cols = (~globals: Globals.t, avail_width: option(float)): int =>
  switch (avail_width) {
  | Some(w) => int_of_float((w -. 46.) /. globals.font_metrics.col_width)
  | None => 60
  };

let type_view =
    (
      ~globals: Globals.t,
      ~inject_jump,
      ~on_close: Effect.t(unit),
      ~dynamics: Language.Dynamics.Map.t,
      ~info_map: Language.Statics.Map.t,
      ~avail_width: option(float)=None,
      ~graph: CanvasGraph.t,
      key: string,
    )
    : option(Node.t) =>
  node_of(graph, key)
  |> Option.map((n: CanvasGraph.tynode) => {
       let probing = globals.settings.core.probe_all;
       let set_expand = x =>
         globals.inject_global(Set(Sidebar(SetCanvasExpand(x))));
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
       let tally: Hashtbl.t(string, (int, int, Language.DHExp.t)) =
         Hashtbl.create(16);
       Language.Sample.Map.fold(
         (id, samples, ()) =>
           switch (site_ty(~info_map, id)) {
           | Some(t) when List.mem(t, names) =>
             List.iter(
               (s: Language.Sample.t) => {
                 let v = print_value(s.value);
                 let (c, latest, _) =
                   Option.value(
                     ~default=(0, 0, s.value),
                     Hashtbl.find_opt(tally, v),
                   );
                 Hashtbl.replace(
                   tally,
                   v,
                   (c + 1, max(latest, s.seq), s.value),
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
           (v, (c, latest, ex), acc) => [(v, c, latest, ex), ...acc],
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
               List.mapi(
                 (i, (v, c, _, ex)) =>
                   div(
                     ~attrs=[
                       clss(["type-value", "well-pick"]),
                       Attr.title(v ++ "\n(click to expand)"),
                       Attr.on_click(_ => set_expand(Some((i, 0)))),
                     ],
                     [
                       CanvasValue.chip(
                         ~font_metrics=globals.font_metrics,
                         ~available=34,
                         ex,
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
       let expanded =
         switch (globals.settings.sidebar.canvas_expand) {
         | Some((vi, _)) =>
           switch (List.nth_opt(shown, vi)) {
           | Some((_, c, _, ex)) => [
               expand_panel(
                 ~globals,
                 ~cols=expand_cols(~globals, avail_width),
                 ~label=
                   Printf.sprintf(
                     "%s value%s",
                     n.label,
                     c > 1 ? Printf.sprintf({js| (×%d)|js}, c) : "",
                   ),
                 ~nav=
                   Some((
                     vi,
                     List.length(shown),
                     v => set_expand(Some((v, 0))),
                   )),
                 ~on_collapse=set_expand(None),
                 ex,
               ),
             ]
           | None => []
           }
         | None => []
         };
       div(
         ~attrs=[clss(["canvas-focus"])],
         [head] @ doc @ values_row @ expanded,
       );
     });

let view =
    (
      ~globals: Globals.t,
      ~inject_jump,
      ~on_close: Effect.t(unit),
      ~dynamics: Language.Dynamics.Map.t,
      ~ask_agent: option(CanvasGraph.edge => Effect.t(unit))=None,
      ~avail_width: option(float)=None,
      ~graph: CanvasGraph.t,
      name: string,
    )
    : option(Node.t) =>
  List.find_opt((e: CanvasGraph.edge) => e.e_name == name, graph.edges)
  |> Option.map((e: CanvasGraph.edge) => {
       let probing = globals.settings.core.probe_all;
       let set_expand = x =>
         globals.inject_global(Set(Sidebar(SetCanvasExpand(x))));
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
       let all_slots = input_slots @ [(e.dst, e.e_out_id)];
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
       let mk_slot = (~role, si, sl) =>
         slot(
           ~globals,
           ~inject_jump,
           ~graph,
           ~dynamics,
           ~probing,
           ~pick=Some(vi => set_expand(Some((si, vi)))),
           ~role,
           sl,
         );
       let n_inputs = List.length(input_slots);
       let flow =
         div(
           ~attrs=[clss(["focus-flow"])],
           [
             div(
               ~attrs=[clss(["focus-inputs"])],
               List.mapi(mk_slot(~role="role-in"), input_slots),
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
               [mk_slot(~role="role-out", n_inputs, (e.dst, e.e_out_id))],
             ),
           ],
         );
       let expanded =
         switch (globals.settings.sidebar.canvas_expand) {
         | Some((si, vi)) =>
           switch (List.nth_opt(all_slots, si)) {
           | Some((key, Some(id))) =>
             let (samps, total) = samples_at(~dynamics, id);
             switch (List.nth_opt(samps, vi)) {
             | Some(s) => [
                 expand_panel(
                   ~globals,
                   ~cols=expand_cols(~globals, avail_width),
                   ~label=
                     Printf.sprintf(
                       "%s %s",
                       key,
                       si >= n_inputs ? "(output)" : "(input)",
                     ),
                   ~nav=Some((vi, total, v => set_expand(Some((si, v))))),
                   ~on_collapse=set_expand(None),
                   s.value,
                 ),
               ]
             | None => []
             };
           | _ => []
           }
         | None => []
         };
       div(
         ~attrs=[clss(["canvas-focus"])],
         [head] @ doc @ [flow] @ expanded,
       );
     });
