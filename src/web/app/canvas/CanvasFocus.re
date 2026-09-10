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

/* sites inside a livelit definition (`let ^name = { … }`) are a VIEW's
   insides, not program values: the type panel and the cards skip them.
   Decided by source position against the definitions' spans. */
let livelit_spans =
    (~graph: CanvasGraph.t, ~syntax: CachedSyntax.t)
    : list((Measured.Point.t, Measured.Point.t)) =>
  List.filter_map(
    id => TermData.extreme_measures(id, syntax.term_data, syntax.measured),
    graph.livelit_defs,
  );

let inside_livelit =
    (
      ~spans: list((Measured.Point.t, Measured.Point.t)),
      ~syntax: CachedSyntax.t,
      id: Id.t,
    )
    : bool =>
  spans != []
  && (
    switch (TermData.extreme_measures(id, syntax.term_data, syntax.measured)) {
    | Some((a, b)) =>
      List.exists(
        ((l, r)) =>
          Measured.Point.compare(l, a) <= 0
          && Measured.Point.compare(b, r) <= 0,
        spans,
      )
    | None => false
    }
  );

/* pretty type of a probed site, from statics. Memoized per info_map
   (every card scans every site on every render; pretty-printing each
   site's type each time was a per-keystroke cost). */
let site_ty_memo:
  ref((option(Language.Statics.Map.t), Hashtbl.t(Id.t, option(string)))) =
  ref((Option.none, Hashtbl.create(64)));
let site_ty = (~info_map: Language.Statics.Map.t, id: Id.t): option(string) => {
  let tbl =
    switch (site_ty_memo^) {
    | (Option.Some(m), tbl) when m === info_map => tbl
    | _ =>
      let tbl = Hashtbl.create(256);
      site_ty_memo := (Option.some(info_map), tbl);
      tbl;
    };
  switch (Hashtbl.find_opt(tbl, id)) {
  | Option.Some(r) => r
  | Option.None =>
    let r: option(string) =
      switch (Id.Map.find_opt(id, info_map)) {
      | Option.Some(InfoExp(e)) =>
        Option.some(CanvasGraph.pretty_ty(Language.Info.exp_ty(e)))
      | Option.Some(InfoPat(p)) =>
        Option.some(CanvasGraph.pretty_ty((p: Language.Info.pat).ana))
      | _ => Option.none
      };
    Hashtbl.replace(tbl, id, r);
    r;
  };
};

/* the info panel for an orbiting constant: name (jump), type, and the
   definition rendered as a value chip */
let value_info =
    (
      ~globals: Globals.t,
      ~editor: CodeWithStatics.Model.t,
      ~inject_jump: Id.t => Ui_effect.t(unit),
      /* thunk: closing clears a module ref, which must happen at CLICK
         time, not while building the vdom */
      ~on_close: unit => Ui_effect.t(unit),
      v: CanvasGraph.value,
    )
    : Node.t =>
  div(
    ~attrs=[clss(["canvas-focus"])],
    [
      div(
        ~attrs=[clss(["focus-head"])],
        [
          div(
            ~attrs=[
              clss(["focus-name"]),
              Attr.on_click(_ => inject_jump(v.v_id)),
            ],
            [text(v.v_name)],
          ),
          div(~attrs=[clss(["focus-ty"])], [text(": " ++ v.v_ty)]),
          div(
            ~attrs=[clss(["focus-close"]), Attr.on_click(_ => on_close())],
            [text({js|✕|js})],
          ),
        ],
      ),
      div(
        ~attrs=[clss(["focus-value-def"])],
        {
          /* prefer the probe SAMPLE display (rich renderers included):
             with ambient sampling on, the definition site has samples;
             the plain syntax chip is only the no-samples fallback */
          /* the sampled site is the def minus wrappers (parens,
             ascriptions, filters) — the raw def's rep id is often the
             wrapper's */
          let sampled =
            Language.Sample.Map.lookup(
              Language.Exp.rep_id(CanvasGraph.strip_exp(v.v_def)),
              editor.dynamics,
            )
            |> Util.OptUtil.and_then(ss =>
                 switch (List.rev(ss)) {
                 | [] => None
                 | [newest, ..._] => Some((newest, List.length(ss)))
                 }
               )
            |> Util.OptUtil.and_then(((s: Language.Sample.t, n)) =>
                 CanvasProbe.value_chip(
                   ~globals,
                   ~editor,
                   ~count=n,
                   ~target_cols=80,
                   s,
                 )
               );
          switch (sampled) {
          | Some(chip) => [chip]
          | None => [
              CanvasValue.chip(
                ~font_metrics=globals.font_metrics,
                ~available=48,
                v.v_def,
              ),
            ]
          };
        },
      ),
    ],
  );

/* the pretty type names a node stands for: its label, its alias body,
   or (former nodes "()@Model", "[]@Hand") the alias's body type */
let node_type_names = (~graph: CanvasGraph.t, n: CanvasGraph.tynode) => {
  let key = n.key;
  let former_anchor = (prefix: string): option(string) => {
    let pl = String.length(prefix);
    String.length(key) > pl && String.sub(key, 0, pl) == prefix
      ? Some(String.sub(key, pl, String.length(key) - pl)) : None;
  };
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
};

/* the sample site to show for a type node: among the probed sites whose
   type is this node's, prefer one a rich renderer applies to (a site
   inside a livelit's own definition can't be shown through that
   livelit — its name isn't in scope there), then the newest sample; the
   well is then navigable through that site's history */
let rec value_site =
        (
          ~dynamics: Language.Dynamics.Map.t,
          ~info_map: Language.Statics.Map.t,
          ~graph: CanvasGraph.t,
          ~focus: option(Language.Sample.Focus.t)=?,
          ~within: option((Measured.Point.t, Measured.Point.t))=?,
          ~syntax: option(CachedSyntax.t)=?,
          key: string,
        )
        : option(Id.t) => {
  /* memo: the same inputs (by identity for the maps) give the same site;
     every render of every card asks */
  let hit =
    List.find_opt(
      ((k, d, i, f, w, g, _)) =>
        k == key
        && d === dynamics
        && i === info_map
        && f == focus
        && w == within
        && g === graph,
      value_site_memo^,
    );
  switch (hit) {
  | Option.Some((_, _, _, _, _, _, r)) => r
  | Option.None =>
    let r =
      value_site_impl(
        ~dynamics,
        ~info_map,
        ~graph,
        ~focus?,
        ~within?,
        ~syntax?,
        key,
      );
    value_site_memo :=
      [
        (key, dynamics, info_map, focus, within, graph, r),
        ...Util.ListUtil.take(31, value_site_memo^),
      ];
    r;
  };
}
and value_site_memo:
  ref(
    list(
      (
        string,
        Language.Dynamics.Map.t,
        Language.Statics.Map.t,
        option(Language.Sample.Focus.t),
        option((Measured.Point.t, Measured.Point.t)),
        CanvasGraph.t,
        option(Id.t),
      ),
    ),
  ) =
  ref([])
and value_site_impl =
    (
      ~dynamics: Language.Dynamics.Map.t,
      ~info_map: Language.Statics.Map.t,
      ~graph: CanvasGraph.t,
      /* the global dynamic focus: a site with a sample IN the focused
         call outranks every other, so expanded cards follow the focus
         in tandem (select a function's output; its input card shows
         the input of that same call) */
      ~focus: option(Language.Sample.Focus.t)=?,
      /* the selected function's source range: its own sites (params,
         body, the pieces of a returned tuple) outrank the rest, so
         selecting `advance` on the canvas puts every card on that
         function's inputs and outputs — one call per sample */
      ~within: option((Measured.Point.t, Measured.Point.t))=?,
      ~syntax: option(CachedSyntax.t)=?,
      key: string,
    )
    : option(Id.t) =>
  switch (node_of(graph, key)) {
  | None => None
  /* a LIVELIT's node (`^game`): its card is the app — the projected
     invocation of that livelit, live and interactive */
  | Some(n) when Language.UserLivelit.is_livelit_name(n.label) =>
    switch (syntax) {
    | None => None
    | Some(syntax) =>
      List.find_opt(
        (id: Id.t) =>
          switch (
            Id.Map.find_opt(id, syntax.projectors),
            Id.Map.find_opt(id, info_map),
          ) {
          | (Some(p), Some(Language.Info.InfoExp({user_term, ctx, _})))
              when p.kind == ProjectorCore.Kind.Livelit =>
            /* the site's term is the Projector wrapper around the use */
            let inner =
              switch (Language.Exp.term_of(user_term)) {
              | Projector(_, e) => e
              | _ => user_term
              };
            switch (Language.UserLivelit.use_parts(ctx, inner)) {
            | Some((name, _)) => "^" ++ name == n.label
            | None => false
            };
          | _ => false
          },
        syntax.projector_list,
      )
    }
  | Some(n) =>
    let names = node_type_names(~graph, n);
    let spans =
      switch (syntax) {
      | Some(syntax) => livelit_spans(~graph, ~syntax)
      | None => []
      };
    let in_view = (id: Id.t): bool =>
      switch (syntax) {
      | Some(syntax) => inside_livelit(~spans, ~syntax, id)
      | None => false
      };
    let is_app_site = (id: Id.t): bool =>
      switch (syntax) {
      | Some(syntax) =>
        List.mem(id, syntax.projector_list)
        && (
          switch (Id.Map.find_opt(id, syntax.projectors)) {
          | Some(p) => p.kind == ProjectorCore.Kind.Livelit
          | None => false
          }
        )
      | None => false
      };
    let inside = (id: Id.t): bool =>
      switch (within, syntax) {
      | (Some((l, r)), Some(syntax)) =>
        switch (
          TermData.extreme_measures(id, syntax.term_data, syntax.measured)
        ) {
        | Some((a, b)) =>
          Measured.Point.compare(l, a) <= 0
          && Measured.Point.compare(b, r) <= 0
        | None => false
        }
      | _ => false
      };
    let aligned = (id: Id.t, samples: list(Language.Sample.t)): bool =>
      switch (focus, Id.Map.find_opt(id, info_map)) {
      | (Some(cursor), Some(info)) when cursor.anchor != None =>
        let ap_id = Language.Sample.Focus.cur_var_ap(info);
        let is_pat =
          switch (info) {
          | Language.Info.InfoPat(_) => true
          | _ => false
          };
        List.exists(
          (s: Language.Sample.t) => {
            let r =
              Language.Sample.Focus.relation(
                ~trimmed=true,
                ~ap_id,
                cursor,
                s,
              );
            r.is_call_cursor
            /* a PARAMETER's sample is taken one frame up from the body's
               sites of the same call: still the same call */
            || is_pat
            && (
              switch (r.relative_level_to_cursor) {
              | Above(1)
              | Below(1) => true
              | _ => false
              }
            );
          },
          samples,
        );
      | _ => false
      };
    /* NOMINAL first: a site typed by the alias's own name outranks one
       that merely matches its body ((Int, Int) is not a Point until the
       program says so); the body is the fallback */
    let nominal = (t: string): bool => t == n.label || t == n.key;
    /* CHEAP renderability: a livelit in scope for the site's type (no
       view evaluation here — that happens once, in the card), else the
       structural renderers' own checks */
    let rich_ok = (id: Id.t, s: Language.Sample.t): bool => {
      let statics = Id.Map.find_opt(id, info_map);
      LivelitRenderer.candidates(statics) != []
      || List.exists(
           (r: RichProbe.packed_renderer) =>
             r.id != "table"
             && r.id != "livelit"
             && r.can_handle(~statics, Sort.Exp, s.value),
           RichProbeRegistry.renderers,
         );
    };
    Language.Sample.Map.fold(
      (id, samples, best) =>
        switch (site_ty(~info_map, id), List.rev(samples)) {
        | (Some(t), [newest_s, ..._])
            when List.mem(t, names) && !in_view(id) =>
          let newest =
            List.fold_left(
              (m, s: Language.Sample.t) => max(m, s.seq),
              -1,
              samples,
            );
          let n_samples = List.length(samples);
          let depth =
            List.fold_left(
              (m, s: Language.Sample.t) =>
                min(m, List.length(s.call_stack)),
              max_int,
              samples,
            );
          /* HISTORY = distinct values, not samples: a step function's
             output has one sample per step and all of them differ; a
             helper's parameter has hundreds of copies of the same few */
          let distinct =
            List.fold_left(
              (reps, s: Language.Sample.t) =>
                List.exists(r => Language.Exp.fast_equal(r, s.value), reps)
                  ? reps : [s.value, ...reps],
              [],
              /* bounded: 240 samples of a helper's parameter need no
                 exact count to lose to a step function's 10 */
              Util.ListUtil.take(48, samples),
            )
            |> List.length;
          let rank = (
            /* an app's own site last: its stream is the app's, and the
               app has its own node */
            is_app_site(id) ? 0 : 1,
            aligned(id, samples) ? 1 : 0,
            inside(id) ? 1 : 0,
            nominal(t) ? 1 : 0,
            rich_ok(id, newest_s) ? 1 : 0,
            /* the richest history (← → walk it) … */
            distinct,
            /* … told in the fewest samples … */
            - n_samples,
            /* … at the shallowest call (the step function's own frame,
               where the other cards' sites align exactly) */
            - depth,
            newest,
          );
          switch (best) {
          | Some((_, b)) when compare(b, rank) >= 0 => best
          | _ => Some((id, rank))
          };
        | _ => best
        },
      dynamics,
      None,
    )
    |> Option.map(fst);
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
       let names = node_type_names(~graph, n);
       let tally: Hashtbl.t(string, (int, int, Language.Sample.t)) =
         Hashtbl.create(16);
       let spans = livelit_spans(~graph, ~syntax=editor.editor.syntax);
       Language.Sample.Map.fold(
         (id, samples, ()) =>
           switch (site_ty(~info_map, id)) {
           | Some(_)
               when inside_livelit(~spans, ~syntax=editor.editor.syntax, id) =>
             ()
           | Some(t) when List.mem(t, names) =>
             List.iter(
               (s: Language.Sample.t) =>
                 /* an app site's stream mixes its VIEW samples (HTML)
                    with its values: at a site not typed HTML, the views
                    are not values of this type */
                 if (t != "HTML" && MvuShape.is_html(s.value)) {
                   ();
                 } else {
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
                     "  ·  %d distinct value(s), %d sample(s)",
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
