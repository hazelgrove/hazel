open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Haz3lcore;

/* CanvasSidebar — the Constellation panel: an architectural graph of the
   current program (types as nodes, functions as edges), derived fresh from
   cached statics each render. Tests attach to the functions they mention;
   the agent appears as an avatar at its current work site. See
   plans/agent-canvas.md. */

let current_code =
    (editors: Editors.Model.t): option(ScratchMode.Scratchpad.code) =>
  switch (editors) {
  | Scratch(m)
  | Documentation(m) =>
    switch (List.nth_opt(m.scratchpads, m.current)) {
    | Some({kind: Code(code), _}) => Some(code)
    | _ => None
    }
  | Tutorial(_)
  | Exercises(_) => None
  };

/* free-panning headroom (screen px, constant across zoom) around the
   board; the scroll position that puts the board's top-left at the pane
   corner is exactly (pan_slack, pan_slack) */
let pan_slack = 392.;
/* set to force a re-anchor on the next render (fit button) */
let pending_anchor: ref(bool) = ref(false);
let last_anchor_slide: ref(string) = ref("");
/* anchor the scroll to the board (skipping the slack margin) whenever
   the pane is fresh, the slide changed, or an anchor was requested */
let ensure_scroll_anchor = (slide: string): unit =>
  Js_of_ocaml.(
    ignore(
      Dom_html.window##requestAnimationFrame(
        Js.wrap_callback(_ => {
          switch (Util.JsUtil.get_elem_by_id_opt("canvas-scroll")) {
          | None => ()
          | Some(el) =>
            let el' = Js.Unsafe.coerce(el);
            let fresh = !Js.Optdef.test(Js.Unsafe.get(el', "__panAnchored"));
            if (fresh || last_anchor_slide^ != slide || pending_anchor^) {
              Js.Unsafe.set(el', "__panAnchored", Js.bool(true));
              last_anchor_slide := slide;
              pending_anchor := false;
              el'##.scrollLeft := pan_slack;
              el'##.scrollTop := pan_slack;
            };
          }
        }),
      ),
    )
  );

/* pinch-zoom plumbing: the wheel listener must be non-passive (to
   preventDefault the browser's page zoom on ctrl+wheel), so it is
   installed raw on the scroll element; these refs carry the current
   zoom and dispatcher across renders */
let zoom_now: ref(float) = ref(1.);
let zoom_send: ref(option(float => unit)) =
  ref(None: option(float => unit));
let install_zoom_listener = (): unit => {
  Js_of_ocaml.(
    switch (Util.JsUtil.get_elem_by_id_opt("canvas-scroll")) {
    | None => ()
    | Some(el) =>
      let el' = Js.Unsafe.coerce(el);
      let installed: bool =
        Js.Optdef.test(Js.Unsafe.get(el', "__zoomInstalled"));
      if (!installed) {
        Js.Unsafe.set(el', "__zoomInstalled", Js.bool(true));
        let last = ref(0.);
        let cb =
          Js.Unsafe.callback((evt: Js.t(Js.Unsafe.any)) => {
            let ctrl: bool = Js.to_bool(Js.Unsafe.coerce(evt)##.ctrlKey);
            if (ctrl) {
              ignore(Js.Unsafe.meth_call(evt, "preventDefault", [||]));
              let dy: float = Js.Unsafe.coerce(evt)##.deltaY;
              let now: float =
                Js.Unsafe.coerce(Js.Unsafe.global)##._Date##now();
              if (now -. last^ > 40.) {
                last := now;
                let z0 = zoom_now^;
                let z = z0 *. exp(-. dy *. 0.008);
                /* detent at 1:1 so pinching back to normal lands exactly */
                let z = abs_float(z -. 1.) < 0.06 ? 1. : z;
                let z = max(0.4, min(2.5, z));
                switch (zoom_send^) {
                | Some(send) =>
                  send(z);
                  /* keep the content point under the CURSOR fixed:
                     correct the scroll once the new zoom has rendered */
                  let rect =
                    Js.Unsafe.meth_call(el', "getBoundingClientRect", [||]);
                  /* measure from the scroll ORIGIN (inside the border),
                     not the border box, or the fixed point drifts by
                     clientLeft * (r - 1) per step */
                  let mx: float =
                    Js.Unsafe.coerce(evt)##.clientX
                    -.
                    Js.Unsafe.coerce(rect)##.left
                    -.
                    Js.Unsafe.coerce(el')##.clientLeft;
                  let my: float =
                    Js.Unsafe.coerce(evt)##.clientY
                    -.
                    Js.Unsafe.coerce(rect)##.top
                    -.
                    Js.Unsafe.coerce(el')##.clientTop;
                  let sl: float = Js.Unsafe.coerce(el')##.scrollLeft
                  and st: float = Js.Unsafe.coerce(el')##.scrollTop;
                  ignore(
                    Js.Unsafe.global##setTimeout(
                      Js.Unsafe.callback(() => {
                        let r = z /. z0;
                        /* the pan slack doesn't scale with zoom, so the
                           fixed-point math runs in board coordinates */
                        Js.Unsafe.coerce(el')##.scrollLeft :=
                          (sl +. mx -. pan_slack) *. r +. pan_slack -. mx;
                        Js.Unsafe.coerce(el')##.scrollTop :=
                          (st +. my -. pan_slack) *. r +. pan_slack -. my;
                      }),
                      60,
                    ),
                  );
                | None => ()
                };
              };
            };
          });
        ignore(
          Js.Unsafe.meth_call(
            el',
            "addEventListener",
            [|
              Js.Unsafe.inject(Js.string("wheel")),
              Js.Unsafe.inject(cb),
              Js.Unsafe.inject(
                Js.Unsafe.obj([|
                  ("passive", Js.Unsafe.inject(Js.bool(false))),
                |]),
              ),
            |],
          ),
        );
      };
    }
  );
};

/* avatar continuity: hold the last successfully resolved site so the
   avatar persists through interstitials (turn start, pathless tools,
   paths gone stale mid-burst) instead of blinking out */
let last_avatar_id: ref(option(Id.t)) = ref(None: option(Id.t));

/* While a node drag is in progress the pane measurements are FROZEN:
   any mid-drag change (scrollbar appearing, overlay quirks, content
   growth) would otherwise re-derive the fit/centering frame and bounce
   every node between adjacent grid cells under the user's cursor. */
let drag_active: ref(bool) = ref(false);
let cached_avail_w: ref(option(float)) = ref(None: option(float));
let cached_avail_h: ref(option(float)) = ref(None: option(float));

/* Sticky frame for manually-arranged slides: pins/offsets are stored in
   the pre-normalization frame, so re-deriving the frame from a changed
   program re-anchors EVERY manual position (placement pins landed away
   from the click; pinned nodes flew off-pane on unrelated edits). Once a
   slide has any manual position, its frame (origin + scales) is frozen
   here — re-derived only on pane resize, slide switch, or reset layout. */
type frame_cache = {
  fc_slide: string,
  fc_origin: CanvasLayout.pos,
  fc_x_scale: float,
  fc_y_scale: float,
  fc_avail_w: float,
};
let cached_frame: ref(option(frame_cache)) =
  ref(None: option(frame_cache));

/* keys of recently placed nodes (canvas gestures AND agent-created
   arrivals); their node views get a grow-in animation for a moment */
let last_placed: ref(list((string, float))) = ref([]);
let note_placed = (key: string): unit =>
  last_placed :=
    [(key, CanvasBuffer.now())]
    @ List.filter(((k, _)) => k != key, last_placed^);

/* previous render's nodes, for removal detection: a node that vanishes
   gets a suction ripple (negative amplitude) at its last position.
   Slide switches and wholesale rewrites are gated by the removal count. */
let last_node_snapshot: ref((string, list((string, (float, float))))) =
  ref(("", []));

/* canvas right-click context menu: contents are canvas-specific but the
   state machine, rendering, keyboard handling, and open/close listeners
   are all the shared Util.Menu machinery (same as the editor menu).
   State is transient (module refs); repaints ride Set(CanvasTick). */
module CanvasMenuListener =
  Util.MenuListener.Make({
    let menu_class = "canvas-context-menu";
    let supports_keys = true;
    let scroll_into_view = false;
    let close_on_scroll = false;
  });
let canvas_menu: ref(Util.Menu.t) = ref(Util.Menu.closed);
/* viewport coords for the fixed-position menu box */
let canvas_menu_client: ref((float, float)) = ref((0., 0.));
/* model coords of the right-click, for inserts */
let canvas_menu_at: ref((float, float)) = ref((0., 0.));
/* Some((key, type syntax)) when opened on a node */
let canvas_menu_node: ref(option((string, string))) =
  ref(None: option((string, string)));
let last_avatar_pos: ref(option(CanvasLayout.pos)) =
  ref(None: option(CanvasLayout.pos));

let current_slide = (editors: Editors.Model.t): string =>
  switch (editors) {
  | Scratch(m)
  | Documentation(m) =>
    switch (List.nth_opt(m.scratchpads, m.current)) {
    | Some(sp) => sp.name
    | None => ""
    }
  | Tutorial(_)
  | Exercises(_) => ""
  };

/* the node's name as type syntax, for canvas-authored signatures */
let ty_syntax = (n: CanvasGraph.tynode): string =>
  switch (n.kind) {
  | Builtin
  | Ghost => n.label /* dup keys carry @anchors; holes print as ? */
  | _ => n.key /* alias / [T] / (A, B) keys are literal type syntax */
  };

let px_float = (s: string): float =>
  try(float_of_string(String.sub(s, 0, String.length(s) - 2))) {
  | _ => 0.
  };

let test_results_of =
    (editors: Editors.Model.t): option(Language.TestResults.t) =>
  switch (current_code(editors)) {
  | Some({editor: cell, _}) => EvalResult.Model.test_results(cell.result)
  | None => None
  };

/* The agent's current work site: the last tool result's jump path, resolved
   through the same HighLevelNodeMap plumbing the chat rows use. */
let avatar_target =
    (~editor: CodeWithStatics.Model.t, editors: Editors.Model.t)
    : option((Id.t, string)) =>
  switch (current_code(editors)) {
  | None => None
  | Some({agent, _}) =>
    let cs = agent.chat_system;
    switch (Id.Map.find_opt(cs.current, cs.chat_map)) {
    | None => None
    | Some(chat) =>
      let last_tool =
        Chat.Utils.linearize(chat)
        |> List.rev
        |> List.find_map((m: Message.Model.t) =>
             switch (m.role) {
             | ToolResult(tr) => Some(tr)
             | _ => None
             }
           );
      switch (last_tool) {
      | None => None
      | Some(tr) =>
        let busy =
          agent.awaiting_response == Some(cs.current)
          || agent.pending_dispatch_send == Some(cs.current);
        let state =
          if (!tr.success) {
            "err";
          } else if (busy) {
            "edit";
          } else {
            "";
          };
        let node_map = {
          let z = editor.editor.state.zipper;
          HighLevelNodeMap.build(z, editor.statics.info_map);
        };
        switch (ToolCallSummary.of_tool_call(tr.tool_call)) {
        | None => None
        | Some(summary) =>
          ToolResultView.first_resolving_id(~node_map, summary.jump_paths)
          |> Option.map(id => (id, state))
        };
      };
    };
  };

/* Map a syntax id to a canvas position: direct hit on an edge/node/value
   anchor, else climb the id's statics ancestors to find one. */
let locate =
    (~info_map: Language.Statics.Map.t, lay: CanvasLayout.t, id: Id.t)
    : option(CanvasLayout.pos) => {
  let direct = (id: Id.t): option(CanvasLayout.pos) => {
    let edge =
      List.find_opt(
        (el: CanvasLayout.edge_layout) => Id.compare(el.edge.e_id, id) == 0,
        lay.edges,
      )
      |> Option.map((el: CanvasLayout.edge_layout) => el.label_p);
    let node = () =>
      List.find_opt(
        (nl: CanvasLayout.node_layout) =>
          switch (nl.node.n_id) {
          | Some(nid) => Id.compare(nid, id) == 0
          | None => false
          },
        lay.nodes,
      )
      |> Option.map((nl: CanvasLayout.node_layout) => nl.p);
    let value = () =>
      List.find_opt(
        (vl: CanvasLayout.value_layout) =>
          Id.compare(vl.value.v_id, id) == 0,
        lay.values,
      )
      |> Option.map((vl: CanvasLayout.value_layout) => vl.p);
    switch (edge) {
    | Some(p) => Some(p)
    | None =>
      switch (node()) {
      | Some(p) => Some(p)
      | None => value()
      }
    };
  };
  switch (direct(id)) {
  | Some(p) => Some(p)
  | None =>
    switch (Id.Map.find_opt(id, info_map)) {
    | Some(info) =>
      Language.Info.ancestors_of(info) |> List.find_map(direct)
    | None => None
    }
  };
};

let view =
    (
      ~globals: Globals.t,
      ~editors: Editors.Model.t,
      ~editors_inject: Editors.Update.t => Effect.t(unit),
      ~editor: CodeWithStatics.Model.t,
      /* false when hosted in the main-area split, whose width is the
         pane's own, not the sidebar setting's */
      ~use_sidebar_width=true,
      (),
    )
    : Node.t => {
  let test_results = test_results_of(editors);
  let graph = CanvasGraph.extract(~test_results?, editor.statics);
  let slide = current_slide(editors);
  /* temporal pacing: within an agent burst the canvas renders queued
     snapshots at a max rate so each tool call reads as its own beat */
  let editor = {
    let schedule_tick = (delay: float) => {
      open Js_of_ocaml;
      let cb =
        Js.Unsafe.callback(() => {
          CanvasBuffer.tick_fired();
          globals.inject_global(Set(CanvasTick))
          |> Bonsai.Effect.Expert.handle;
        });
      ignore(Js.Unsafe.global##setTimeout(cb, delay));
    };
    CanvasBuffer.observe(
      ~enabled=globals.settings.canvas_pace,
      ~schedule_tick,
      editor,
    );
  };
  let zoom = globals.settings.canvas_zoom;
  zoom_now := zoom;
  CanvasBuffer.canvas_zoom := zoom;
  zoom_send :=
    Some(
      z =>
        globals.inject_global(Set(SetCanvasZoom(z)))
        |> Bonsai.Effect.Expert.handle,
    );
  install_zoom_listener();
  let offsets =
    globals.settings.canvas_node_offsets
    |> List.filter_map((((s, k), d)) => s == slide ? Some((k, d)) : None);
  let pins =
    globals.settings.canvas_node_pins
    |> List.filter_map((((s, k), d)) => s == slide ? Some((k, d)) : None);
  /* stretch columns to fill the panel when the graph is narrower than it;
     panel width is read from the (pre-patch) DOM, so the first render
     after a panel switch or drag-resize uses the previous width */
  let avail_width = {
    /* prefer the scroll area's own client width (it accounts for panel
       padding and the scrollbar); fall back to the sidebar setting/DOM */
    let from_scroll =
      switch (Util.JsUtil.get_elem_by_id_opt("canvas-scroll")) {
      | Some(el) =>
        let w = Js_of_ocaml.Js.Unsafe.coerce(el)##.clientWidth;
        w > 50 ? Some(float_of_int(w)) : None;
      | None => None
      };
    switch (
      from_scroll,
      use_sidebar_width ? globals.settings.sidebar.width : None,
    ) {
    | (Some(w), _) => Some(w)
    | (None, Some(w)) => Some(float_of_int(w) -. 6.)
    | (None, None) =>
      switch (Util.JsUtil.get_elem_by_id_opt("canvas-sidebar")) {
      | Some(el) =>
        let w = Js_of_ocaml.Js.Unsafe.coerce(el)##.offsetWidth;
        w > 50 ? Some(float_of_int(w)) : None;
      | None => None
      }
    };
  };
  let avail_height =
    switch (Util.JsUtil.get_elem_by_id_opt("canvas-scroll")) {
    | Some(el) =>
      let h = Js_of_ocaml.Js.Unsafe.coerce(el)##.clientHeight;
      h > 100 ? Some(float_of_int(h)) : None;
    | None => None
    };
  /* frame freeze during drags (see drag_active) */
  let avail_width =
    if (drag_active^) {
      switch (cached_avail_w^) {
      | Some(_) as c => c
      | None => avail_width
      };
    } else {
      cached_avail_w := avail_width;
      avail_width;
    };
  let avail_height =
    if (drag_active^) {
      switch (cached_avail_h^) {
      | Some(_) as c => c
      | None => avail_height
      };
    } else {
      cached_avail_h := avail_height;
      avail_height;
    };
  let lay = {
    let manual = offsets != [] || pins != [];
    let aw = Option.value(~default=0., avail_width);
    let runtime_frame =
      switch (cached_frame^) {
      | Some(fc)
          when
            manual
            && fc.fc_slide == slide
            && Float.abs(fc.fc_avail_w -. aw) < 2. =>
        Some((fc.fc_origin, fc.fc_x_scale, fc.fc_y_scale))
      | _ => None
      };
    /* fresh session: the frame the pins were laid in, from settings
       (pane size may differ — positions beat centering) */
    let persisted_frame =
      switch (runtime_frame) {
      | Some(_) => runtime_frame
      | None =>
        manual
          ? List.assoc_opt(slide, globals.settings.canvas_frames)
            |> Option.map(((ox, oy, xs, ys)) =>
                 (
                   CanvasLayout.{
                     x: ox,
                     y: oy,
                   },
                   xs,
                   ys,
                 )
               )
          : None
      };
    switch (persisted_frame) {
    | Some((origin, xs, ys)) =>
      /* sticky frame: manual positions never re-anchor (see cached_frame) */
      cached_frame :=
        Some({
          fc_slide: slide,
          fc_origin: origin,
          fc_x_scale: xs,
          fc_y_scale: ys,
          fc_avail_w: aw,
        });
      CanvasLayout.layout(
        ~x_scale=xs,
        ~y_scale=ys,
        ~origin_override=Some(origin),
        ~offsets,
        ~pins,
        graph,
      );
    | None =>
      /* ALL frame decisions (fit scales, normalization origin) derive
         from the VIRGIN layout — no user offsets/pins — so dragging a
         node can never rescale or re-anchor the rest of the graph. The
         final layout applies the frozen frame plus the user's edits. */
      let virgin = CanvasLayout.layout(graph);
      let y_scale =
        switch (avail_height) {
        | Some(h) => min(2.1, max(1., (h -. 40.) /. virgin.height))
        | None => 1.
        };
      let x_scale =
        switch (avail_width) {
        | Some(avail) =>
          let target = avail -. 16.;
          let s1 = min(1.8, max(0.7, target /. virgin.width));
          if (s1 >= 1.8 || s1 <= 0.7) {
            s1;
          } else {
            let v1 = CanvasLayout.layout(~x_scale=s1, ~y_scale, graph);
            v1.width >= target -. 30. && v1.width <= target +. 30.
              ? s1 : min(1.8, max(0.7, s1 *. target /. v1.width));
          };
        | None => 1.
        };
      let framed =
        CanvasLayout.layout(
          ~x_scale,
          ~y_scale,
          ~center_within=avail_width,
          graph,
        );
      cached_frame :=
        Some({
          fc_slide: slide,
          fc_origin: framed.origin,
          fc_x_scale: x_scale,
          fc_y_scale: y_scale,
          fc_avail_w: aw,
        });
      offsets == [] && pins == []
        ? framed
        : CanvasLayout.layout(
            ~x_scale,
            ~y_scale,
            ~origin_override=Some(framed.origin),
            ~offsets,
            ~pins,
            graph,
          );
    };
  };
  /* auto-fit while the agent works: if the (paced) graph has outgrown
     the pane at the current zoom, ease the zoom down one step toward
     fitting. Discrete steps, rate-limited; CSS transitions smooth the
     hop where supported. */
  if (globals.settings.canvas_pace && CanvasBuffer.in_burst()) {
    switch (avail_width, avail_height) {
    | (Some(aw), Some(ah)) =>
      let need =
        min(
          (aw -. 10.) /. max(1., lay.width),
          (ah -. 10.) /. max(1., lay.height),
        );
      if (need < zoom -. 0.05 && CanvasBuffer.autofit_due()) {
        let target = max(0.4, need);
        let stepped = max(target, zoom -. 0.15);
        let send = () =>
          globals.inject_global(Set(SetCanvasZoom(stepped)))
          |> Bonsai.Effect.Expert.handle;
        ignore(
          Js_of_ocaml.Js.Unsafe.global##setTimeout(
            Js_of_ocaml.Js.Unsafe.callback(send),
            80,
          ),
        );
      };
    | _ => ()
    };
  };
  /* canvas clicks SELECT the definition (caret at front, cell focused) */
  let inject_jump = (id: Id.t) =>
    Effect.Many([
      globals.inject_global(SelectTile(id)),
      Effect.Stop_propagation,
    ]);
  let set_focus = (f: option(string)) =>
    globals.inject_global(Set(Sidebar(SetCanvasFocus(f))));
  /* clicking a function: focus it in the detail strip AND select its
     def. If the dynamic cursor isn't aligned with any of the function's
     samples (its wells would show ⊖), capture the first sample at the
     first input anchor so values appear immediately. */
  let on_edge_click = (e: CanvasGraph.edge) => {
    let align: list(Effect.t(unit)) = {
      let anchor =
        switch (e.e_whole_ids, e.e_arg_ids, e.e_out_id) {
        | ([a, ..._], _, _) => Some(a)
        | ([], [a, ..._], _) => Some(a)
        | ([], [], out) => out
        };
      switch (
        Option.bind(anchor, a =>
          Language.Dynamics.Map.lookup(a, editor.dynamics)
        )
      ) {
      | Some([_, ..._] as samples) =>
        let sf = editor.editor.state.zipper.refractors.sample_focus;
        let cursor_stack = Language.Sample.Focus.effective_stack(sf);
        let aligned =
          List.exists(
            (smp: Language.Sample.t) =>
              Language.CallStack.equal(smp.call_stack, cursor_stack),
            samples,
          );
        if (aligned) {
          [];
        } else {
          let first =
            List.fold_left(
              (best: Language.Sample.t, smp: Language.Sample.t) =>
                smp.seq < best.seq ? smp : best,
              List.hd(samples),
              samples,
            );
          [
            globals.inject_global(
              ActiveEditor(
                Project(
                  SampleFocus(
                    Capture(Language.Sample.capture_of_sample(first), None),
                  ),
                ),
              ),
            ),
          ];
        };
      | _ => []
      };
    };
    Effect.Many(
      [
        set_focus(Some(e.e_name)),
        globals.inject_global(SelectTile(e.e_id)),
        Effect.Stop_propagation,
      ]
      @ align,
    );
  };
  /* ---- canvas authoring: stubs go through the agent's own edit tools
     (same executor, guardrails, whitespace normalization). Stubs insert
     after the LAST TOP-LEVEL binding; the path is resolved from the
     binding's id and verified top-level, because a bare name can be
     shadowed by a nested binding (which would land the stub inside a
     function). ---- */
  let node_map =
    HighLevelNodeMap.build(
      editor.editor.state.zipper,
      editor.statics.info_map,
    );
  let insert_path: option(string) =
    switch (graph.last_def, node_map) {
    | (None, _) => None /* empty program: pathless is the documented case */
    | (Some((name, _)), None) => Some(name)
    | (Some((name, id)), Some(map)) =>
      let top_level = rid =>
        try(
          HighLevelNodeMap.parent_of(map, HighLevelNodeMap.find(map, rid))
          == None
        ) {
        | _ => false
        };
      /* exact: our binding id is a top-level node in the map */
      let via_id =
        try(
          top_level(id) ? Some(HighLevelNodeMap.id_to_name(map, id)) : None
        ) {
        | _ => None
        };
      switch (via_id) {
      | Some(p) => Some(p)
      | None =>
        /* probe name and name#k, requiring a top-level resolution */
        [name]
        @ List.map(k => name ++ "#" ++ string_of_int(k), [1, 2, 3, 4, 5])
        |> List.find_opt(p =>
             switch (HighLevelNodeMap.path_to_id_opt(map, p)) {
             | Some(rid) => top_level(rid)
             | None => false
             }
           )
        |> (
          fun
          | Some(p) => Some(p)
          | None => Some(name)
        )
      };
    };
  let insert_stub = (code: string) =>
    Effect.Many([
      editors_inject(
        Editors.Update.Scratch(
          ScratchMode.Update.AgentAction(
            Agent.Update.Action.DirectEdit(
              "insert_after",
              `Assoc(
                [("code", `String(code))]
                @ (
                  switch (insert_path) {
                  | Some(p) => [("path", `String(p))]
                  | None => []
                  }
                ),
              ),
            ),
          ),
        ),
      ),
      /* the insert junction consumes leading linebreaks (regrout caret
         trim), so run the canonical reformat to put the new binding on
         its own line */
      editors_inject(
        Editors.Update.Scratch(
          ScratchMode.Update.CellAction(
            CellEditor.Update.MainEditor(
              CodeEditable.Update.Perform(Format(Pretty)),
            ),
          ),
        ),
      ),
    ]);
  let fresh_name = (prefix: string): string => {
    let used =
      List.map((n: CanvasGraph.tynode) => n.label, graph.nodes)
      @ List.map((e: CanvasGraph.edge) => e.e_name, graph.edges)
      @ List.map((v: CanvasGraph.value) => v.v_name, graph.values);
    let rec go = i => {
      let cand = prefix ++ string_of_int(i);
      List.mem(cand, used) ? go(i + 1) : cand;
    };
    go(1);
  };
  let connect = globals.settings.sidebar.canvas_connect;
  let set_connect = c =>
    globals.inject_global(Set(Sidebar(SetCanvasConnect(c))));
  let place = globals.settings.sidebar.canvas_place;
  let set_place = p =>
    globals.inject_global(Set(Sidebar(SetCanvasPlace(p))));
  /* plain click (no drag, no connect mode): focus the type's values;
     aliases also select their definition */
  let click_effect = (n: CanvasGraph.tynode) =>
    Effect.Many(
      [
        globals.inject_global(Set(Sidebar(SetCanvasFocusTy(Some(n.key))))),
      ]
      @ (
        switch (n.n_id) {
        | Some(id) => [globals.inject_global(SelectTile(id))]
        | None => []
        }
      ),
    );
  /* drag-vs-click on a node: document listeners move the div imperatively;
     release either commits a layout delta or fires the click */
  /* persist the live frame alongside any pin/offset commit so a reload
     re-lays manual positions in the frame they were made in */
  let persist_frame = (): Effect.t(unit) =>
    switch (cached_frame^) {
    | Some(fc) =>
      globals.inject_global(
        Set(
          SetCanvasFrame(
            slide,
            fc.fc_origin.x,
            fc.fc_origin.y,
            fc.fc_x_scale,
            fc.fc_y_scale,
          ),
        ),
      )
    | None => Effect.Ignore
    };
  let start_node_drag =
      (
        n: CanvasGraph.tynode,
        evt: Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.mouseEvent),
      )
      : Effect.t(unit) => {
    open Js_of_ocaml;
    let sx: int = Js.Unsafe.coerce(evt)##.clientX;
    let sy: int = Js.Unsafe.coerce(evt)##.clientY;
    /* pinned nodes update their pin; others accumulate a drag delta */
    let pin = List.assoc_opt(n.key, pins);
    let base =
      Option.value(~default=(0., 0.), List.assoc_opt(n.key, offsets));
    let commit = ((dx, dy)) =>
      Effect.Many([
        switch (pin) {
        | Some((px, py)) =>
          globals.inject_global(
            Set(SetCanvasNodePin(slide, n.key, px +. dx, py +. dy)),
          )
        | None =>
          globals.inject_global(
            Set(
              SetCanvasNodeOffset(
                slide,
                n.key,
                fst(base) +. dx,
                snd(base) +. dy,
              ),
            ),
          )
        },
        persist_frame(),
      ]);
    let now = (): float => Js.Unsafe.coerce(Js.Unsafe.global)##._Date##now();
    let last_live = ref(now());
    let orig =
      switch (Util.JsUtil.get_elem_by_id_opt(CanvasView.node_dom_id(n.key))) {
      | Some(el) =>
        let st = Js.Unsafe.coerce(el)##.style;
        Some((
          el,
          px_float(Js.to_string(st##.left)),
          px_float(Js.to_string(st##.top)),
        ));
      | None => None
      };
    drag_active := true;
    let moved = ref(false);
    let delta = ref((0., 0.));
    let rec on_move = e => {
      let x: int = Js.Unsafe.coerce(e)##.clientX;
      let y: int = Js.Unsafe.coerce(e)##.clientY;
      /* CSS zoom scales screen deltas; convert to layout px */
      let z = max(0.2, globals.settings.canvas_zoom);
      let dx = float_of_int(x - sx) /. z
      and dy = float_of_int(y - sy) /. z;
      if (abs_float(dx) +. abs_float(dy) > 4.) {
        moved := true;
      };
      if (moved^) {
        /* snap the drag itself to the dot lattice: the visual position
           is always exactly what the committed layout will produce, so
           the imperative style can never disagree with the re-render
           (which skips patching when the snapped value is unchanged) */
        let dx = CanvasLayout.snap(dx)
        and dy = CanvasLayout.snap(dy);
        delta := (dx, dy);
        switch (orig) {
        | Some((el, l, t)) =>
          let st = Js.Unsafe.coerce(el)##.style;
          st##.left := Js.string(Printf.sprintf("%.1fpx", l +. dx));
          st##.top := Js.string(Printf.sprintf("%.1fpx", t +. dy));
          /* bow wave: the unsnapped position, so the field glides */
          CanvasRipple.set_field(
            Some((
              l +. float_of_int(x - sx) /. z,
              t +. float_of_int(y - sy) /. z,
            )),
          );
        | None => ()
        };
        /* throttled live commits so edges follow during the drag (a
           re-render per mousemove would fight the whole-page vdom cost) */
        if (now() -. last_live^ > 120.) {
          last_live := now();
          Effect.Expert.handle_non_dom_event_exn(commit((dx, dy)));
        };
      };
      ();
    }
    and on_up = _ => {
      drag_active := false;
      if (moved^) {
        /* settle: a soft splash where the node lands */
        switch (orig) {
        | Some((_, l, t)) =>
          let (dx, dy) = delta^;
          CanvasRipple.splash(~amp=4.5, (l +. dx, t +. dy));
        | None => ()
        };
      };
      CanvasRipple.set_field(None);
      let doc = Js.Unsafe.coerce(Dom_html.document);
      let _ = doc##removeEventListener("mousemove", on_move);
      let _ = doc##removeEventListener("mouseup", on_up);
      Effect.Expert.handle_non_dom_event_exn(
        moved^ ? commit(delta^) : click_effect(n),
      );
      ();
    };
    let doc = Js.Unsafe.coerce(Dom_html.document);
    let _ = doc##addEventListener("mousemove", on_move);
    let _ = doc##addEventListener("mouseup", on_up);
    Effect.Prevent_default;
  };
  let on_node_mousedown = (n: CanvasGraph.tynode, evt) =>
    switch (connect, place) {
    | (Some(srcs), _) =>
      let shift = Js_of_ocaml.(Js.to_bool(Js.Unsafe.coerce(evt)##.shiftKey));
      if (srcs == [] || shift) {
        /* first click, or shift-click: accumulate another source */
        Effect.Many([
          set_connect(Some(srcs @ [ty_syntax(n)])),
          Effect.Stop_propagation,
          Effect.Prevent_default,
        ]);
      } else {
        /* plain click with sources collected: this is the target */
        let src =
          switch (srcs) {
          | [a] => a
          | many => "(" ++ String.concat(", ", many) ++ ")"
          };
        let stub =
          Printf.sprintf(
            "let %s : %s -> %s = ? in",
            fresh_name("f"),
            src,
            ty_syntax(n),
          );
        /* a multi-source fn materializes an implicit product node: pin it
           at the sources' barycenter instead of wherever the layered
           layout drops it (pin BEFORE the edit — see on_canvas_click) */
        let pin_product =
          switch (srcs) {
          | []
          | [_] => []
          | many =>
            let positions =
              List.filter_map(
                sstr =>
                  lay.nodes
                  |> List.find_opt((nl: CanvasLayout.node_layout) =>
                       ty_syntax(nl.node) == sstr
                     )
                  |> Option.map((nl: CanvasLayout.node_layout) => nl.p),
                many,
              );
            switch (positions) {
            | [] => []
            | ps =>
              let count = float_of_int(List.length(ps));
              let cx =
                List.fold_left((a, p: CanvasLayout.pos) => a +. p.x, 0., ps)
                /. count;
              let cy =
                List.fold_left((a, p: CanvasLayout.pos) => a +. p.y, 0., ps)
                /. count;
              /* between all endpoints, biased toward the inputs:
                 2/3 sources centroid + 1/3 target */
              let tgt =
                lay.nodes
                |> List.find_opt((nl: CanvasLayout.node_layout) =>
                     nl.node.key == n.key
                   )
                |> Option.map((nl: CanvasLayout.node_layout) => nl.p);
              let (bx, by) =
                switch (tgt) {
                | Some(t) => (
                    (2. *. cx +. t.x) /. 3.,
                    (2. *. cy +. t.y) /. 3.,
                  )
                | None => (cx, cy)
                };
              let product_key = "(" ++ String.concat(", ", many) ++ ")";
              note_placed(product_key);
              CanvasRipple.splash((
                CanvasLayout.snap(bx),
                CanvasLayout.snap(by),
              ));
              [
                globals.inject_global(
                  Set(
                    SetCanvasNodePin(
                      slide,
                      product_key,
                      CanvasLayout.snap(bx) -. lay.origin.x,
                      CanvasLayout.snap(by) -. lay.origin.y,
                    ),
                  ),
                ),
                persist_frame(),
              ];
            };
          };
        /* the new edge lands with a soft splash at its target */
        switch (
          lay.nodes
          |> List.find_opt((nl: CanvasLayout.node_layout) =>
               nl.node.key == n.key
             )
        ) {
        | Some(nl) => CanvasRipple.splash(~amp=5., (nl.p.x, nl.p.y))
        | None => ()
        };
        Effect.Many(
          pin_product
          @ [
            insert_stub(stub),
            set_connect(None),
            Effect.Stop_propagation,
            Effect.Prevent_default,
          ],
        );
      };
    | (None, Some(("tuple", comps))) =>
      Effect.Many([
        set_place(Some(("tuple", comps @ [ty_syntax(n)]))),
        Effect.Stop_propagation,
        Effect.Prevent_default,
      ])
    | (None, Some(("list", _))) =>
      Effect.Many([
        set_place(Some(("list", [ty_syntax(n)]))),
        Effect.Stop_propagation,
        Effect.Prevent_default,
      ])
    | (None, Some(_)) =>
      /* type mode ignores node clicks — only canvas clicks place */
      Effect.Many([Effect.Stop_propagation, Effect.Prevent_default])
    | (None, None) => start_node_drag(n, evt)
    };
  /* insert a `type T = <body> in` stub and pin the new node (and its
     former, when it has components) at the given model point (pins are
     recorded in the pre-normalization frame via lay.origin) */
  let place_stub_at =
      (~kind: string, ~comps: list(string), (x: float, y: float))
      : Effect.t(unit) => {
    let name = fresh_name("T");
    let body =
      switch (kind, comps) {
      | ("alias", [b]) => b
      | ("tuple", []) => "(?, ?)"
      | ("tuple", [a]) => "(" ++ a ++ ", ?)"
      | ("tuple", cs) => "(" ++ String.concat(", ", cs) ++ ")"
      | ("list", [a]) => "[" ++ a ++ "]"
      | ("list", _) => "[?]"
      | _ => "?"
      };

    note_placed(name);
    CanvasRipple.splash((CanvasLayout.snap(x), CanvasLayout.snap(y)));
    /* the alias's former ("()"/"[]") sits midway between its
       component nodes and the alias, instead of auto-docking */
    let former_pins = {
      let comp_pts =
        List.filter_map(
          cstr =>
            lay.nodes
            |> List.find_opt((nl: CanvasLayout.node_layout) =>
                 ty_syntax(nl.node) == cstr
               )
            |> Option.map((nl: CanvasLayout.node_layout) => nl.p),
          comps,
        );
      switch (comp_pts, kind) {
      | ([], _)
      | (_, "type") => []
      | (pts, _) =>
        let count = float_of_int(List.length(pts));
        let cx =
          List.fold_left((a, p: CanvasLayout.pos) => a +. p.x, 0., pts)
          /. count;
        let cy =
          List.fold_left((a, p: CanvasLayout.pos) => a +. p.y, 0., pts)
          /. count;
        let former_key = (kind == "list" ? "[]@" : "()@") ++ name;
        [
          globals.inject_global(
            Set(
              SetCanvasNodePin(
                slide,
                former_key,
                CanvasLayout.snap((cx +. x) /. 2.) -. lay.origin.x,
                CanvasLayout.snap((cy +. y) /. 2.) -. lay.origin.y,
              ),
            ),
          ),
        ];
      };
    };
    Effect.Many(
      [
        /* pin + frame land BEFORE the edit: a render between these
           effects re-derived the frame with no pins yet, moving
           every node and putting the new one off the click */
        globals.inject_global(
          Set(
            SetCanvasNodePin(
              slide,
              name,
              /* snapped: the node materializes exactly on the
                 previewed lattice dot */
              CanvasLayout.snap(x) -. lay.origin.x,
              CanvasLayout.snap(y) -. lay.origin.y,
            ),
          ),
        ),
      ]
      @ former_pins
      @ [
        persist_frame(),
        insert_stub(Printf.sprintf("type %s = %s in", name, body)),
      ],
    );
  };
  /* a canvas-background click in place mode inserts the stub where you
     clicked; a double-click on blank canvas does the same with no mode */
  let on_canvas_click: option(((float, float)) => Effect.t(unit)) =
    switch (place) {
    | None => None
    | Some((kind, comps)) =>
      Some(
        pt =>
          Effect.Many([
            place_stub_at(~kind, ~comps, pt),
            set_place(None),
            Effect.Stop_propagation,
          ]),
      )
    };
  let on_canvas_dblclick = (pt: (float, float)): Effect.t(unit) =>
    Effect.Many([
      place_stub_at(~kind="type", ~comps=[], pt),
      Effect.Stop_propagation,
      Effect.Prevent_default,
    ]);
  /* ---- canvas context menu ---- */
  let nudge = globals.inject_global(Set(CanvasTick));
  let menu_close = (): unit => {
    canvas_menu := Util.Menu.closed;
    canvas_menu_node := None;
  };
  /* icon palette: gestures are glyphs with tooltips, not text rows.
     Payloads are THUNKS — building the menu must not run the gesture. */
  let menu_act = (thunk: unit => Effect.t(unit)): Effect.t(unit) => {
    menu_close();
    Effect.Many([thunk(), nudge]);
  };
  let menu_icon =
      (
        ~cls="",
        glyph: string,
        tooltip: string,
        thunk: unit => Effect.t(unit),
      )
      : Node.t =>
    div(
      ~attrs=[
        clss(["cmenu-icon"] @ (cls == "" ? [] : [cls])),
        Attr.title(tooltip),
        Attr.on_pointerdown(_ => menu_act(thunk)),
      ],
      [text(glyph)],
    );
  let menu_rows: list(Node.t) =
    switch (canvas_menu_node^) {
    | Some((key, syntax)) => [
        div(
          ~attrs=[clss(["cmenu-row"])],
          [
            menu_icon(
              {js|ƒ|js}, "function from this node: click the target next", () =>
              set_connect(Some([syntax]))
            ),
            menu_icon(
              "()",
              "tuple with this component: click more nodes, then the canvas",
              () =>
              set_place(Some(("tuple", [syntax])))
            ),
            menu_icon("[]", "list of this type, placed beside it", () => {
              let pos =
                lay.nodes
                |> List.find_opt((nl: CanvasLayout.node_layout) =>
                     nl.node.key == key
                   )
                |> Option.map((nl: CanvasLayout.node_layout) => nl.p);
              let pt =
                switch (pos) {
                | Some(p) => (p.x +. 84., p.y)
                | None => canvas_menu_at^
                };
              place_stub_at(~kind="list", ~comps=[syntax], pt);
            }),
          ],
        ),
      ]
    | None =>
      let at = canvas_menu_at^;
      [
        div(
          ~attrs=[clss(["cmenu-row"])],
          [
            menu_icon({js|τ|js}, "type T = ? in", () =>
              place_stub_at(~kind="type", ~comps=[], at)
            ),
            menu_icon("()", "type T = (?, ?) in", () =>
              place_stub_at(~kind="tuple", ~comps=[], at)
            ),
            menu_icon("[]", "type T = [?] in", () =>
              place_stub_at(~kind="list", ~comps=[], at)
            ),
          ],
        ),
        div(
          ~attrs=[clss(["cmenu-row"])],
          List.map(
            b =>
              menu_icon(~cls="cmenu-base", b, "type T = " ++ b ++ " in", () =>
                place_stub_at(~kind="alias", ~comps=[b], at)
              ),
            ["Int", "Float", "Bool", "String"],
          ),
        ),
      ];
    };
  CanvasMenuListener.sync(
    ~menu_open=Util.Menu.is_open(canvas_menu^),
    ~on_close=
      () => {
        menu_close();
        nudge;
      },
    ~handle_key=
      key =>
        key == "Escape"
          ? Some(
              {
                menu_close();
                nudge;
              },
            )
          : None,
    (),
  );
  let open_canvas_menu =
      (
        ~node: option((string, string)),
        ~client: (float, float),
        ~at: (float, float),
      )
      : Effect.t(unit) => {
    canvas_menu_node := node;
    canvas_menu_client := client;
    canvas_menu_at := at;
    canvas_menu := Util.Menu.opened;
    nudge;
  };
  let on_canvas_contextmenu = (at: (float, float), client: (float, float)) =>
    open_canvas_menu(~node=None, ~client, ~at);
  let on_node_contextmenu = (n: CanvasGraph.tynode, client: (float, float)) =>
    open_canvas_menu(
      ~node=Some((n.key, ty_syntax(n))),
      ~client,
      ~at=(0., 0.),
    );
  let menu_layer =
    div(
      ~attrs=[clss(["canvas-menu-layer"])],
      switch (canvas_menu^) {
      | None => []
      | Some(_) =>
        let (mx, my) = canvas_menu_client^;
        [
          div(
            ~attrs=[
              clss([
                "context-menu",
                "canvas-context-menu",
                "open-down-right",
              ]),
              Attr.create(
                "style",
                Printf.sprintf(
                  "position: fixed; left: %.0fpx; top: %.0fpx;",
                  mx,
                  my,
                ),
              ),
            ],
            menu_rows,
          ),
        ];
      },
    );
  let focused = globals.settings.sidebar.canvas_focus;
  let focused_ty = globals.settings.sidebar.canvas_focus_ty;
  /* hand a hole-bodied function to the agent as an obligation */
  let ask_agent =
    switch (current_code(editors)) {
    | Some({agent, _}) =>
      Some(
        (e: CanvasGraph.edge) => {
          let doc =
            switch (e.e_doc) {
            | Some(d) => " Intent: " ++ d
            | None => ""
            };
          let content =
            Printf.sprintf(
              "Please implement the unwritten function `%s : %s` — its body is currently a hole.%s",
              e.e_name,
              e.e_ty,
              doc,
            );
          Effect.Many([
            editors_inject(
              Editors.Update.Scratch(
                ScratchMode.Update.AgentAction(
                  Agent.Update.Action.SendMessage(
                    Message.Utils.mk_user_message(content),
                    agent.chat_system.current,
                  ),
                ),
              ),
            ),
            Effect.Stop_propagation,
          ]);
        },
      )
    | None => None
    };
  let (agent_busy, reasoning_tail) =
    switch (current_code(editors)) {
    | Some({agent, _}) => (
        agent.awaiting_response == Some(agent.chat_system.current)
        || agent.pending_dispatch_send == Some(agent.chat_system.current),
        agent.pending_assistant_reasoning,
      )
    | None => (false, "")
    };
  let avatar = {
    let resolved =
      avatar_target(~editor, editors)
      |> Util.OptUtil.and_then(((id, state)) =>
           switch (locate(~info_map=editor.statics.info_map, lay, id)) {
           | Some(p) =>
             last_avatar_id := Some(id);
             last_avatar_pos := Some(p);
             Some((p, state));
           | None =>
             /* target no longer resolves (edits moved on): try the last
                good id against the current layout, else hold position */
             switch (
               last_avatar_id^
               |> Util.OptUtil.and_then(
                    locate(~info_map=editor.statics.info_map, lay),
                  )
             ) {
             | Some(p) =>
               last_avatar_pos := Some(p);
               Some((p, state));
             | None => last_avatar_pos^ |> Option.map(p => (p, state))
             }
           }
         );
    switch (resolved) {
    | Some((p, state)) =>
      /* busy with no fresh edit landing = thinking */
      Some((p, agent_busy && state == "" ? "think" : state))
    | None =>
      /* nothing ever resolved this session: while the agent works,
         still embody it at the last known or a neutral spot */
      agent_busy ? last_avatar_pos^ |> Option.map(p => (p, "think")) : None
    };
  };
  /* streaming chain-of-thought tail for the avatar's bubble (rendered
     only while busy; pace toggle governs the whole watch experience) */
  let avatar_bubble =
    if (agent_busy
        && globals.settings.canvas_pace
        && String.length(reasoning_tail) > 0) {
      let n = String.length(reasoning_tail);
      let tail_len = min(52, n);
      Some(String.sub(reasoning_tail, n - tail_len, tail_len));
    } else {
      None;
    };
  let split_btn = {
    let split = globals.settings.canvas_split;
    div(
      ~attrs=[
        clss(["canvas-split-btn"]),
        Attr.on_click(_ => globals.inject_global(Set(ToggleCanvasSplit))),
        Attr.title(
          split
            ? "dock the canvas back into the sidebar"
            : "split view: canvas beside the editor, agent chat in the sidebar — watch the graph update live as the agent works",
        ),
      ],
      [text(split ? {js|⇱ dock|js} : {js|⇲ split|js})],
    );
  };
  let legend = {
    let item = (cls, glyph, label) =>
      div(
        ~attrs=[clss(["legend-item", cls])],
        [
          span(~attrs=[clss(["legend-glyph"])], [text(glyph)]),
          text(label),
        ],
      );
    div(
      ~attrs=[clss(["canvas-legend"])],
      [
        item("lg-fn", {js|─▶|js}, "function"),
        item("lg-form", {js|┈▶|js}, "made of"),
        item("lg-hole", {js|┈┈|js}, "unwritten (hole)"),
        item("lg-tests", {js|●|js}, "tests"),
        item("lg-agent", "@", "agent"),
      ],
    );
  };
  let focus_strip =
    switch (focused_ty, focused) {
    | (Some(key), _) =>
      CanvasFocus.type_view(
        ~globals,
        ~editor,
        ~inject_jump,
        ~on_close=
          globals.inject_global(Set(Sidebar(SetCanvasFocusTy(None)))),
        ~dynamics=editor.dynamics,
        ~info_map=editor.statics.info_map,
        ~graph,
        key,
      )
      |> Option.to_list
    | (None, Some(name)) =>
      CanvasFocus.view(
        ~globals,
        ~editor,
        ~inject_jump,
        ~on_close=set_focus(None),
        ~ask_agent,
        ~graph,
        name,
      )
      |> Option.to_list
    | (None, None) => []
    };
  let toolbar = {
    let btn = (~cls="", ~on_press: unit => unit=() => (), label, tooltip, eff) =>
      div(
        ~attrs=[
          clss(["canvas-tool-btn"] @ (cls == "" ? [] : [cls])),
          Attr.on_click(_ => {
            on_press();
            eff;
          }),
          Attr.title(tooltip),
        ],
        [text(label)],
      );
    let mode_btn = (kind, label, tooltip) => {
      let active =
        switch (place) {
        | Some((k, _)) => k == kind
        | None => false
        };
      btn(
        ~cls=active ? "tool-active" : "",
        label,
        tooltip,
        set_place(active ? None : Some((kind, []))),
      );
    };
    div(
      ~attrs=[clss(["canvas-toolbar"])],
      [
        mode_btn(
          "type",
          {js|τ|js},
          "stub type: click the canvas where it should go; creates type T = ? in",
        ),
        mode_btn(
          "tuple",
          "()",
          "tuple former: click component nodes in order, then the canvas to place; creates type T = (A, B) in",
        ),
        mode_btn(
          "list",
          "[]",
          "list former: click the element node, then the canvas to place; creates type T = [A] in",
        ),
        btn(
          ~cls=connect == None ? "" : "tool-active",
          {js|ƒ|js},
          "draw a function: click a source node then a target node (shift-click collects several sources into a tuple input); creates let f : A -> B = ? in",
          set_connect(connect == None ? Some([]) : None),
        ),
      ]
      @ [
        btn(
          ~cls=globals.settings.canvas_pace ? "tool-active" : "",
          "pace",
          "play bursts of agent edits as separate animated beats (max ~1.4/s) instead of one jump-cut",
          globals.inject_global(Set(ToggleCanvasPace)),
        ),
        btn(
          ~on_press=() => pending_anchor := true,
          "fit",
          "zoom so the whole graph fits the pane",
          {
            /* slack: equality lets sub-pixel rounding re-summon the
               scrollbar the fit was meant to remove */
            let zw =
              switch (avail_width) {
              | Some(w) => (w -. 24.) /. max(1., lay.width)
              | None => 1.
              };
            let zh =
              switch (avail_height) {
              | Some(h) => (h -. 24.) /. max(1., lay.height)
              | None => 1.
              };
            globals.inject_global(
              Set(SetCanvasZoom(max(0.4, min(2.5, min(zw, zh))))),
            );
          },
        ),
        btn(
          ~cls=offsets == [] && pins == [] ? "tool-disabled" : "",
          "reset",
          offsets == [] && pins == []
            ? "no manual node positions on this slide"
            : "clear manual node positions for this slide",
          offsets == [] && pins == []
            ? Effect.Ignore
            : globals.inject_global(Set(ClearCanvasNodeOffsets(slide))),
        ),
        div(~attrs=[clss(["toolbar-spacer"])], []),
        split_btn,
      ],
    );
  };
  let header =
    div(
      ~attrs=[clss(["canvas-header"])],
      [
        div(~attrs=[clss(["canvas-title"])], [text("Constellation")]),
        toolbar,
      ],
    );
  CanvasRipple.request_draw();
  ensure_scroll_anchor(slide);
  {
    let (prev_slide, prev_nodes) = last_node_snapshot^;
    let cur_keys =
      List.map((nl: CanvasLayout.node_layout) => nl.node.key, lay.nodes);
    let removed =
      List.filter(((k, _)) => !List.mem(k, cur_keys), prev_nodes);
    if (prev_slide == slide && removed != [] && List.length(removed) <= 4) {
      List.iter(
        ((_, (x, y))) => CanvasRipple.splash(~amp=-6.5, (x, y)),
        removed,
      );
    };
    /* agent rainfall: nodes that appear during a paced burst arrive with
       a small splash + grow-in (gesture placements splash bigger at the
       click site and are already stamped, so they're skipped here) */
    let added =
      List.filter(
        (nl: CanvasLayout.node_layout) =>
          !List.mem_assoc(nl.node.key, prev_nodes)
          && !List.mem_assoc(nl.node.key, last_placed^),
        lay.nodes,
      );
    if (prev_slide == slide
        && CanvasBuffer.in_burst()
        && added != []
        && List.length(added) <= 6) {
      List.iter(
        (nl: CanvasLayout.node_layout) => {
          CanvasRipple.splash(~amp=4., (nl.p.x, nl.p.y));
          note_placed(nl.node.key);
        },
        added,
      );
    };
    last_node_snapshot :=
      (
        slide,
        List.map(
          (nl: CanvasLayout.node_layout) => (nl.node.key, (nl.p.x, nl.p.y)),
          lay.nodes,
        ),
      );
  };
  /* telegraph state for CanvasView: rubber-band anchors + grow-in key */
  let connect_pts: list(CanvasLayout.pos) =
    switch (connect) {
    | Some(srcs) =>
      List.filter_map(
        sstr =>
          lay.nodes
          |> List.find_opt((nl: CanvasLayout.node_layout) =>
               ty_syntax(nl.node) == sstr
             )
          |> Option.map((nl: CanvasLayout.node_layout) => nl.p),
        srcs,
      )
    | None => []
    };
  let just_placed = {
    let now = CanvasBuffer.now();
    last_placed := List.filter(((_, t)) => now -. t < 2500., last_placed^);
    List.map(fst, last_placed^);
  };
  /* mode guidance floats OVER the canvas in a zero-height row: putting
     it in the toolbar re-wrapped the row mid-gesture, shifting the
     canvas under the cursor and misplacing the click */
  let hint_row = {
    /* base types join a gesture as chips — the "f takes an alias AND an
       Int" case shouldn't require aliasing Int first; the committed stub
       just names the base type and the graph shows it as a use-site
       satellite like any other */
    let base_chips = (on_pick: string => Effect.t(unit)): list(Node.t) =>
      List.map(
        b =>
          div(
            ~attrs=[
              clss(["hint-chip"]),
              Attr.title("add " ++ b ++ " here"),
              Attr.on_click(_ => on_pick(b)),
            ],
            [text(b)],
          ),
        ["Int", "Float", "Bool", "String"],
      );
    let content =
      switch (connect, place) {
      | (Some([]), _) => [
          [text({js|source: click a node, or |js})],
          base_chips(b => set_connect(Some([b]))),
          [text({js|…|js})],
        ]
      | (Some(srcs), _) => [
          [
            text(
              String.concat(", ", srcs) ++ {js| ⟶ click the target, or |js},
            ),
          ],
          base_chips(b => set_connect(Some(srcs @ [b]))),
          [text({js| (shift+click adds sources)|js})],
        ]
      | (None, Some(("type", _))) => [
          [text({js|click the canvas to place…|js})],
        ]
      | (None, Some(("list", comps))) => [
          [
            text(
              (comps == [] ? "" : String.concat(", ", comps) ++ {js| — |js})
              ++ {js|element: click a node or |js},
            ),
          ],
          base_chips(b => set_place(Some(("list", [b])))),
          [text({js|, then the canvas…|js})],
        ]
      | (None, Some((_, comps))) => [
          [
            text(
              (comps == [] ? "" : String.concat(", ", comps) ++ {js| — |js})
              ++ {js|components: click nodes or |js},
            ),
          ],
          base_chips(b => set_place(Some(("tuple", comps @ [b])))),
          [text({js|, then the canvas…|js})],
        ]
      | (None, None) => []
      };
    /* ALWAYS present (empty when idle): inserting it displaced the
       un-keyed scroll div in the children diff, recreating the whole
       canvas subtree — the fresh root transitioned zoom from 1 (the
       zoom-flicker-on-click bug) */
    div(
      ~attrs=[clss(["canvas-mode-hint-row"])],
      switch (content) {
      | [] => []
      | parts => [
          div(~attrs=[clss(["canvas-mode-hint"])], List.concat(parts)),
        ]
      },
    );
  };
  div(
    ~attrs=[Attr.id("canvas-sidebar")],
    [
      header,
      hint_row,
      div(
        ~attrs=[Attr.id("canvas-scroll"), clss(["canvas-scroll"])],
        [
          div(
            ~attrs=[clss(["canvas-pan-pad"])],
            [
              CanvasView.view(
                ~inject_jump,
                ~connect_pts,
                ~just_placed,
                ~on_canvas_dblclick,
                ~on_canvas_contextmenu,
                ~on_node_contextmenu,
                ~on_edge_click,
                ~on_node_mousedown,
                ~on_canvas_click,
                ~zoom,
                ~avatar_bubble,
                ~min_size=(
                  (Option.value(~default=0., avail_width) -. 2.) /. zoom,
                  (Option.value(~default=0., avail_height) -. 2.) /. zoom,
                ),
                ~focused,
                ~avatar,
                ~loose_tests=graph.loose_tests,
                lay,
              ),
            ],
          ),
        ],
      ),
    ]
    @ focus_strip
    @ [legend, menu_layer],
  );
};
