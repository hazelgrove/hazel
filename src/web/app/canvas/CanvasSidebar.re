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
let pan_slack = CanvasRipple.pan_slack;
/* set to force a re-anchor on the next render (fit button) */
let pending_anchor: ref(bool) = ref(false);
/* where anchoring scrolls to; recomputed each render so the content
   sits centered in the pane (infinite canvas: centering can borrow
   the slack when the graph is smaller than the pane) */
let anchor_target: ref((float, float)) = ref((pan_slack, pan_slack));

/* pinch-zoom plumbing: the wheel listener must be non-passive (to
   preventDefault the browser's page zoom on ctrl+wheel), so it is
   installed raw on the scroll element; these refs carry the current
   zoom and dispatcher across renders */
let zoom_now = CanvasCamera.zoom_now;
let zoom_send = CanvasCamera.zoom_send;
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
        /* panning and pane resizes must redraw the viewport-fixed dot
           field (a resize otherwise leaves a stale, mis-scaled frame
           until the next scroll) */
        ignore(
          Js.Unsafe.meth_call(
            el',
            "addEventListener",
            [|
              Js.Unsafe.inject(Js.string("scroll")),
              Js.Unsafe.inject(
                Js.Unsafe.callback(() => {
                  CanvasCamera.note_scroll();
                  CanvasRipple.request_draw();
                }),
              ),
              Js.Unsafe.inject(
                Js.Unsafe.obj([|
                  ("passive", Js.Unsafe.inject(Js.bool(true))),
                |]),
              ),
            |],
          ),
        );
        if (Js.Optdef.test(Js.Unsafe.get(Js.Unsafe.global, "ResizeObserver"))) {
          let obs =
            Js.Unsafe.new_obj(
              Js.Unsafe.get(Js.Unsafe.global, "ResizeObserver"),
              [|
                Js.Unsafe.inject(
                  Js.Unsafe.callback(() => CanvasRipple.request_draw()),
                ),
              |],
            );
          ignore(
            Js.Unsafe.meth_call(obs, "observe", [|Js.Unsafe.inject(el')|]),
          );
        };
        let last = ref(0.);
        let pending_dy = ref(0.);
        let anim_off_until = ref(0.);
        let cb =
          Js.Unsafe.callback((evt: Js.t(Js.Unsafe.any)) => {
            let ctrl: bool = Js.to_bool(Js.Unsafe.coerce(evt)##.ctrlKey);
            if (ctrl) {
              ignore(Js.Unsafe.meth_call(evt, "preventDefault", [||]));
              let dy: float = Js.Unsafe.coerce(evt)##.deltaY;
              let now: float =
                Js.Unsafe.coerce(Js.Unsafe.global)##._Date##now();
              /* accumulate between throttle windows: dropping deltas
                 made pinching feel sluggish */
              pending_dy := pending_dy^ +. dy;
              if (now -. last^ > 30.) {
                last := now;
                let z0 = zoom_now^;
                let z = z0 *. exp(-. pending_dy^ *. 0.018);
                pending_dy := 0.;
                /* detent at 1:1 so pinching back to normal lands exactly */
                let z = abs_float(z -. 1.) < 0.05 ? 1. : z;
                let z = max(0.4, min(2.5, z));
                switch (zoom_send^) {
                | Some(send) =>
                  /* apply the zoom IMPERATIVELY and correct the scroll
                     in the same frame: the old flow (state update, then
                     a delayed correction) let the anchor point jump and
                     snap back on every step — the pinch judder. The
                     auto-fit zoom transition is suppressed while a
                     pinch is live for the same reason. */
                  let root =
                    Js.Unsafe.meth_call(
                      Js.Unsafe.global##.document,
                      "querySelector",
                      [|Js.Unsafe.inject(Js.string(".canvas-root"))|],
                    );
                  switch (Js.Opt.to_option(root)) {
                  | None => send(z)
                  | Some(root) =>
                    let cl =
                      Js.Unsafe.get(Js.Unsafe.coerce(root), "classList");
                    if (now > anim_off_until^) {
                      ignore(
                        Js.Unsafe.meth_call(
                          cl,
                          "add",
                          [|Js.Unsafe.inject(Js.string("no-zoom-anim"))|],
                        ),
                      );
                    };
                    anim_off_until := now +. 250.;
                    ignore(
                      Js.Unsafe.global##setTimeout(
                        Js.Unsafe.callback(() =>
                          if (Js.Unsafe.coerce(Js.Unsafe.global)##._Date##now()
                              >= anim_off_until^) {
                            ignore(
                              Js.Unsafe.meth_call(
                                cl,
                                "remove",
                                [|
                                  Js.Unsafe.inject(
                                    Js.string("no-zoom-anim"),
                                  ),
                                |],
                              ),
                            );
                          }
                        ),
                        260,
                      ),
                    );
                    Js.Unsafe.coerce(root)##.style##.zoom :=
                      Js.string(Printf.sprintf("%.4f", z));
                    let rect =
                      Js.Unsafe.meth_call(el', "getBoundingClientRect", [||]);
                    /* measure from the scroll ORIGIN (inside the
                       border), not the border box */
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
                    let r = z /. z0;
                    /* the pan slack doesn't scale with zoom, so the
                       fixed-point math runs in board coordinates */
                    Js.Unsafe.coerce(el')##.scrollLeft :=
                      (sl +. mx -. pan_slack) *. r +. pan_slack -. mx;
                    Js.Unsafe.coerce(el')##.scrollTop :=
                      (st +. my -. pan_slack) *. r +. pan_slack -. my;
                    zoom_now := z;
                    CanvasRipple.zoom := z;
                    CanvasCamera.note_user_zoom();
                    CanvasRipple.request_draw();
                    send(z);
                  };
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

/* fit: one camera animation moving zoom AND scroll together so the
   graph center stays pinned at the pane center throughout */
let animate_fit =
    (~z_to: float, ~lw: float, ~lh: float, ~aw: float, ~ah: float): unit =>
  CanvasCamera.animate(
    ~aw,
    ~ah,
    ~zoom=Some(z_to),
    ~dur=320.,
    ~easing=CanvasCamera.EaseOut,
    (lw /. 2., lh /. 2.),
  );
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
              CanvasCamera.mark_driving();
              let (tx, ty) = anchor_target^;
              el'##.scrollLeft := tx;
              el'##.scrollTop := ty;
            };
          }
        }),
      ),
    )
  );

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
  fc_burst: int, /* the burst it was derived in (CanvasLog turn) */
  fc_nodes: int, /* nodes in the graph it was derived from */
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
/* previous render's function edges (by name), for the arrival
   choreography: new edges during an agent beat are drawn by the avatar */
let last_edge_snapshot: ref((string, list(string))) = ref(("", []));

/* collapsed module hulls (root module names); toggled from the hull
   label, repaints ride Set(CanvasTick) */
let collapsed_modules: ref(list(string)) = ref([]);
/* collapse/expand transitions must not read as node deletions: the
   removal differ skips its suctions inside this window */
let collapse_fx_until: ref(float) = ref(0.);

/* hide a collapsed module's internals pre-layout: members, their edges
   and orbit values, and the satellite terminals their functions grew.
   The former node and the module's own value stay. Returns the graph +
   (root, hidden-member count) pairs for the hull label. */
let collapse_filter =
    (g: CanvasGraph.t): (CanvasGraph.t, list((string, int))) =>
  if (collapsed_modules^ == []) {
    (g, []);
  } else {
    let hidden = r => List.mem(r, collapsed_modules^);
    let member_edges =
      List.filter(
        (e: CanvasGraph.edge) =>
          switch (e.m_path) {
          | [r, ..._] => hidden(r)
          | [] => false
          },
        g.edges,
      );
    let member_values =
      List.filter(
        (v: CanvasGraph.value) =>
          switch (v.m_path) {
          | [r, ..._] => hidden(r)
          | [] => false
          },
        g.values,
      );
    let member_qnames =
      List.map((e: CanvasGraph.edge) => e.e_name, member_edges)
      @ List.map((v: CanvasGraph.value) => v.v_name, member_values);
    let is_member_sat = (n: CanvasGraph.tynode): bool =>
      switch (n.sat) {
      | Some(_) =>
        List.exists(
          qn => {
            let suffix = "@" ++ qn;
            let ln = String.length(n.key)
            and ls = String.length(suffix);
            ln >= ls && String.sub(n.key, ln - ls, ls) == suffix;
          },
          member_qnames,
        )
      | None => false
      };
    let drop_node = (n: CanvasGraph.tynode): bool =>
      switch (n.m_path) {
      | [r, ..._] => hidden(r) && n.key != "{}@" ++ r
      | [] => is_member_sat(n)
      };
    let counts =
      List.map(
        r => {
          let c =
            List.length(
              List.filter(
                (n: CanvasGraph.tynode) =>
                  switch (n.m_path) {
                  | [r', ..._] => r' == r && n.key != "{}@" ++ r
                  | [] => false
                  },
                g.nodes,
              ),
            )
            + List.length(
                List.filter(
                  (e: CanvasGraph.edge) =>
                    switch (e.m_path) {
                    | [r', ..._] => r' == r
                    | [] => false
                    },
                  g.edges,
                ),
              )
            + List.length(
                List.filter(
                  (v: CanvasGraph.value) =>
                    switch (v.m_path) {
                    | [r', ..._] => r' == r
                    | [] => false
                    },
                  g.values,
                ),
              );
          (r, c);
        },
        collapsed_modules^,
      );
    (
      {
        ...g,
        nodes: List.filter(n => !drop_node(n), g.nodes),
        edges:
          List.filter(
            (e: CanvasGraph.edge) => !List.mem(e, member_edges),
            g.edges,
          ),
        values:
          List.filter(
            (v: CanvasGraph.value) => !List.mem(v, member_values),
            g.values,
          ),
      },
      counts,
    );
  };

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
/* camera follow: the avatar position last handed to the camera, and
   whether the agent was busy then (a resting avatar waking up = a hop
   of attention even though it didn't move) */
let last_followed: ref(option(CanvasLayout.pos)) =
  ref(None: option(CanvasLayout.pos));
let last_followed_busy: ref(bool) = ref(false);
/* slide the avatar/camera state above belongs to */
let last_seen_slide: ref(string) = ref("");
/* whether beats were playing at the previous render (frame freeze) */
let last_pacing_live: ref(bool) = ref(false);
/* previous (site, state, busy) for transition logging only */
let last_logged_avatar: ref((option(Id.t), string)) =
  ref((None: option(Id.t), "off"));
let last_logged_busy: ref(bool) = ref(false);
/* one pending repaint for the action toast's expiry */
let toast_tick_scheduled: ref(bool) = ref(false);
/* function pill under the pointer (dependency-fan disclosure) */
let hovered_edge: ref(option(string)) = ref(None: option(string));
/* orbiting constant whose info panel is open (transient, like hover) */
let focused_value: ref(option(string)) = ref(None: option(string));

/* Graph extraction walks the whole statics map, and view runs on EVERY
   render — including per-token renders while the model streams. Memoize
   on physical identity of the inputs (statics is the same content
   signal pacing trusts). */
let extract_cache:
  ref(
    option(
      (CachedStatics.t, option(Language.TestResults.t), CanvasGraph.t),
    ),
  ) =
  ref(
    None:
          option(
            (CachedStatics.t, option(Language.TestResults.t), CanvasGraph.t),
          ),
  );
let extract_cached =
    (~test_results: option(Language.TestResults.t), statics: CachedStatics.t)
    : CanvasGraph.t => {
  let hit =
    switch (extract_cache^) {
    | Some((st, tr, g))
        when
          st === statics
          && (
            switch (tr, test_results) {
            | (None, None) => true
            | (Some(a), Some(b)) => a === b
            | _ => false
            }
          ) =>
      Some(g)
    | _ => None
    };
  switch (hit) {
  | Some(g) => g
  | None =>
    let g = CanvasGraph.extract(~test_results?, statics);
    extract_cache := Some((statics, test_results, g));
    g;
  };
};
/* beat viability asks "is the graph blank?" — cache per statics too */
let viable_cache: ref(option((CachedStatics.t, bool))) =
  ref(None: option((CachedStatics.t, bool)));
let viable_cached = (statics: CachedStatics.t): bool =>
  switch (viable_cache^) {
  | Some((st, v)) when st === statics => v
  | _ =>
    let v = CanvasGraph.extract(statics).nodes != [];
    viable_cache := Some((statics, v));
    v;
  };
/* beat weight for pacing: graph elements (by stable name) that differ
   between two states — nodes by key, edges by qualified name */
let graph_delta =
    (
      ~test_results: option(Language.TestResults.t),
      a: CodeWithStatics.Model.t,
      b: CodeWithStatics.Model.t,
    )
    : int => {
  let keys = (g: CanvasGraph.t) =>
    List.map((n: CanvasGraph.tynode) => n.key, g.nodes)
    @ List.map((e: CanvasGraph.edge) => "e:" ++ e.e_name, g.edges);
  let ka = keys(extract_cached(~test_results, a.statics))
  and kb = keys(extract_cached(~test_results, b.statics));
  List.length(List.filter(k => !List.mem(k, kb), ka))
  + List.length(List.filter(k => !List.mem(k, ka), kb));
};
/* sample-volume telemetry: logged when the total moves meaningfully */
let last_logged_sample_total: ref(int) = ref(0);
/* per-edge output-sample counts, for firing data-flow pulses on growth
   (e_name -> (last count, last pulse time)) */
let edge_pulse_state: ref(list((string, (int, float)))) = ref([]);

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

let view_impl =
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
  let slide = current_slide(editors);
  {
    /* the layout frame is frozen while beats play and re-derives when
       they stop: that re-frame must glide, not snap */
    let pl = CanvasBuffer.pacing_live();
    if (last_pacing_live^ && !pl) {
      CanvasBuffer.stage_beat(~slow=true, ());
    };
    last_pacing_live := pl;
  };
  if (last_seen_slide^ != slide) {
    /* per-slide state: a site remembered from the previous slide put the
       avatar off the new board (the blank-canvas start in andrew's run) */
    last_seen_slide := slide;
    last_avatar_pos := None;
    last_avatar_id := None;
    last_followed := None;
    CanvasCamera.reset_exposure();
    CanvasBuffer.avatar_site := None;
    last_followed_busy := false;
    CanvasBuffer.beat_avatar := None;
    CanvasCamera.roi := [];
  };
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
      ~viable=(m: CodeWithStatics.Model.t) => viable_cached(m.statics),
      ~weight=graph_delta(~test_results),
      ~schedule_tick,
      editor,
    );
  };
  /* the graph MUST derive from the PACED editor: extracting from the
     live model rendered every intermediate state instantly (final-state
     jump cuts, blank statics flashes) and left the beats animating an
     already-settled graph */
  let (graph, collapsed_counts) =
    collapse_filter(extract_cached(~test_results, editor.statics));
  {
    /* on fresh statics, note the probe-sample volume when it moved
       meaningfully (floods here are a known freeze suspect) */

    let total =
      Language.Sample.Map.fold(
        (_, ss, (t, m)) => {
          let n = List.length(ss);
          (t + n, max(m, n));
        },
        editor.dynamics,
        (0, 0),
      );
    let (t, m) = total;
    let prev = last_logged_sample_total^;
    if (abs(t - prev) > max(200, prev / 3)) {
      last_logged_sample_total := t;
      CanvasLog.log(
        Printf.sprintf("samples: %d total (max %d on one probe)", t, m),
      );
    };
  };
  let zoom = globals.settings.canvas_zoom;
  zoom_now := zoom;
  CanvasBuffer.canvas_zoom := zoom;
  CanvasRipple.zoom := zoom;
  zoom_send :=
    Some(
      z =>
        globals.inject_global(Set(SetCanvasZoom(z)))
        |> Bonsai.Effect.Expert.handle,
    );
  install_zoom_listener();
  CanvasCamera.install_testers();
  CanvasEnact.install_testers();
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
      let h: int = Js_of_ocaml.Js.Unsafe.coerce(el)##.clientHeight;
      /* measure as if the focus strip were closed: the strip opening
         shrank the pane, re-derived the vertical scale, and made
         bystander nodes jump on click */
      let strip =
        Js_of_ocaml.(
          switch (
            Js.Opt.to_option(
              Js.Unsafe.meth_call(
                Js.Unsafe.global##.document,
                "querySelector",
                [|
                  Js.Unsafe.inject(
                    Js.string("#canvas-sidebar .canvas-focus"),
                  ),
                |],
              ),
            )
          ) {
          | Some(strip_el) =>
            int_of_float(
              Js.Unsafe.coerce(
                Js.Unsafe.meth_call(strip_el, "getBoundingClientRect", [||]),
              )##.height,
            )
          | None => 0
          }
        );
      let h = h + strip;
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
    /* pins/offsets for nodes that no longer exist (another program on this
       slide) must not keep a stale frame alive */
    let node_keys = List.map((n: CanvasGraph.tynode) => n.key, graph.nodes);
    let live = ((k, _)) => List.mem(k, node_keys);
    let manual = List.exists(live, offsets) || List.exists(live, pins);
    let aw = Option.value(~default=0., avail_width);
    /* while beats play, small programs keep re-framing so new nodes spread
       to fill the pane (movers glide); only a program that has outgrown
       the pane freezes its frame until the burst settles */
    /* the frame stays frozen for the whole burst (B2: a node keeps its
       place unless an act moves it); the burst-end re-frame is one tidy */
    let outgrown = CanvasBuffer.pacing_live() && !manual;
    /* a frozen frame that would push the new content past the pane is
       re-fit at this beat instead (the drift makes room first, so the
       whole graph glides to the new fit before anything blooms) */
    let fits_cached =
      switch (cached_frame^, avail_height) {
      | (Some(fc), Some(ah)) when !manual =>
        let l =
          CanvasLayout.layout(
            ~x_scale=fc.fc_x_scale,
            ~y_scale=fc.fc_y_scale,
            ~origin_override=Some(fc.fc_origin),
            graph,
          );
        l.height <= ah -. 8. && l.width <= aw +. 40.;
      | _ => true
      };
    let runtime_frame =
      switch (cached_frame^) {
      | Some(fc)
          when
            fits_cached
            && (
              manual
              || outgrown
              /* a new burst keeps the frozen frame until it adds nodes;
                 re-deriving on the bare tool beat that opens a burst moved
                 everything mid-score */
              && (
                fc.fc_burst == CanvasLog.turn_no()
                || List.length(graph.nodes) <= fc.fc_nodes
              )
            )
            /* a frame derived from an empty graph (the bare tool beat
               before the first insertion) would spread the real one */
            && fc.fc_nodes > 0
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
    let persisted_frame =
      switch (persisted_frame) {
      | Some((_, xs, _)) when runtime_frame == None =>
        /* laid in a wider pane: it would strew the program off-screen */
        let v = CanvasLayout.layout(graph);
        v.width *. xs > aw *. 1.25 ? Option.none : persisted_frame;
      | p => p
      };
    switch (persisted_frame) {
    | Some((origin, xs, ys)) =>
      /* sticky frame: manual positions never re-anchor (see cached_frame) */
      cached_frame :=
        Some({
          fc_slide: slide,
          fc_burst: CanvasLog.turn_no(),
          fc_nodes: List.length(graph.nodes),
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
          /* fill the pane, less a margin the camera can see as "all in" */
          let target = avail -. 56.;
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
          fc_burst: CanvasLog.turn_no(),
          fc_nodes: List.length(graph.nodes),
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
  /* zoom while the agent works is the camera's job now: CanvasCamera.follow
     frames the sites touched this burst (with hysteresis) on each hop */
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
    focused_value := None;
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
  let click_effect = (n: CanvasGraph.tynode) => {
    focused_value := None;
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
  };
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
    /* a beat may still be morphing or revealing geometry (dash arrays
       left on paths would truncate them as they stretch): the drag takes
       over cleanly */
    List.iter(
      id =>
        switch (Util.JsUtil.get_elem_by_id_opt(id)) {
        | Some(el) =>
          let anims = Js.Unsafe.meth_call(el, "getAnimations", [||]);
          let len: int = Js.Unsafe.get(anims, "length");
          for (i in 0 to len - 1) {
            ignore(
              Js.Unsafe.meth_call(Js.Unsafe.get(anims, i), "cancel", [||]),
            );
          };
          ignore(
            Js.Unsafe.meth_call(
              el,
              "removeAttribute",
              [|Js.Unsafe.inject(Js.string("stroke-dasharray"))|],
            ),
          );
          let st = Js.Unsafe.get(el, "style");
          Js.Unsafe.set(st, "strokeDasharray", Js.string(""));
          Js.Unsafe.set(st, "strokeDashoffset", Js.string(""));
        | None => ()
        },
      Util.JsUtil.ids_with_prefix("cpath-")
      @ Util.JsUtil.ids_with_prefix("cform-")
      @ Util.JsUtil.ids_with_prefix("corbit-"),
    );
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
    let set_attr = (el, name: string, v: string) =>
      ignore(
        Js.Unsafe.meth_call(
          el,
          "setAttribute",
          [|
            Js.Unsafe.inject(Js.string(name)),
            Js.Unsafe.inject(Js.string(v)),
          |],
        ),
      );
    let set_pos = (id: string, x: float, y: float) =>
      switch (Util.JsUtil.get_elem_by_id_opt(id)) {
      | Some(el) =>
        let st = Js.Unsafe.coerce(el)##.style;
        st##.left := Js.string(Printf.sprintf("%.1fpx", x));
        st##.top := Js.string(Printf.sprintf("%.1fpx", y));
      | None => ()
      };
    let fmt' = (v: float) => Printf.sprintf("%f", v);
    let pull_back = (p: CanvasLayout.pos, c: CanvasLayout.pos, d: float) => {
      let vx = p.x -. c.x
      and vy = p.y -. c.y;
      let len = max(1., Float.hypot(vx, vy));
      CanvasLayout.{
        x: p.x -. vx /. len *. d,
        y: p.y -. vy /. len *. d,
      };
    };
    /* EXACT drag follow: recompute the REAL layout with the pending
       delta (same frame + offsets the commit will use) and apply every
       piece of geometry imperatively — no vdom, no app render, and
       nothing left to jump on drop. Hull circles go through the jelly
       springs as targets. */
    let lay_with = ((dx, dy)): option(CanvasLayout.t) => {
      let offsets' =
        pin == None
          ? [
            (n.key, (fst(base) +. dx, snd(base) +. dy)),
            ...List.remove_assoc(n.key, offsets),
          ]
          : offsets;
      let pins' =
        switch (pin) {
        | Some((px, py)) => [
            (n.key, (px +. dx, py +. dy)),
            ...List.remove_assoc(n.key, pins),
          ]
        | None => pins
        };
      switch (cached_frame^) {
      | Some(fc) =>
        Some(
          CanvasLayout.layout(
            ~x_scale=fc.fc_x_scale,
            ~y_scale=fc.fc_y_scale,
            ~origin_override=Some(fc.fc_origin),
            ~offsets=offsets',
            ~pins=pins',
            graph,
          ),
        )
      | None => None
      };
    };
    let apply_layout = (l: CanvasLayout.t): unit => {
      List.iter(
        (nl: CanvasLayout.node_layout) =>
          set_pos(CanvasView.node_dom_id(nl.node.key), nl.p.x, nl.p.y),
        l.nodes,
      );
      List.iter(
        (el: CanvasLayout.edge_layout) => {
          let e = el.edge;
          if (el.endo) {
            switch (
              Util.JsUtil.get_elem_by_id_opt(
                "corbit-" ++ CanvasView.sanitize(e.e_name),
              )
            ) {
            | Some(c) =>
              set_attr(c, "cx", fmt'(el.dst_p.x));
              set_attr(c, "cy", fmt'(el.dst_p.y));
            | None => ()
            };
          } else {
            let dstp = pull_back(el.dst_p, el.c2, 7.);
            switch (
              Util.JsUtil.get_elem_by_id_opt(
                "cpath-" ++ CanvasView.sanitize(e.e_name),
              )
            ) {
            | Some(path_el) =>
              set_attr(
                path_el,
                "d",
                Printf.sprintf(
                  "M %f,%f C %f,%f %f,%f %f,%f",
                  el.src_p.x,
                  el.src_p.y,
                  el.c1.x,
                  el.c1.y,
                  el.c2.x,
                  el.c2.y,
                  dstp.x,
                  dstp.y,
                ),
              )
            | None => ()
            };
          };
          set_pos(
            CanvasView.edge_dom_id(e.e_name),
            el.label_p.x,
            el.label_p.y,
          );
          switch (
            Util.JsUtil.get_elem_by_id_opt(
              "clead-" ++ CanvasView.sanitize(e.e_name),
            )
          ) {
          | Some(lel) =>
            set_attr(lel, "x1", fmt'(el.label_p.x));
            set_attr(lel, "y1", fmt'(el.label_p.y -. 8.));
            set_attr(lel, "x2", fmt'(el.label_anchor.x));
            set_attr(lel, "y2", fmt'(el.label_anchor.y));
          | None => ()
          };
        },
        l.edges,
      );
      List.iteri(
        (
          _i,
          (a, b, cp, pp): (
            string,
            string,
            CanvasLayout.pos,
            CanvasLayout.pos,
          ),
        ) => {
          let mx = (cp.x +. pp.x) /. 2.;
          let pp' =
            pull_back(
              pp,
              CanvasLayout.{
                x: mx,
                y: pp.y,
              },
              5.,
            );
          switch (
            Util.JsUtil.get_elem_by_id_opt(CanvasView.formation_dom_id(a, b))
          ) {
          | Some(el) =>
            let (ox, oy) =
              CanvasLayout.link_offset(
                ~nodes=l.nodes,
                ~from_key=a,
                ~to_key=b,
                cp,
                pp',
              );
            set_attr(
              el,
              "d",
              CanvasLayout.link_d(
                cp,
                CanvasLayout.{
                  x: mx +. ox,
                  y: cp.y +. oy,
                },
                CanvasLayout.{
                  x: mx +. ox,
                  y: pp'.y +. oy,
                },
                pp',
              ),
            );
          | None => ()
          };
        },
        l.formations,
      );
      List.iteri(
        (
          _i,
          (a, b, dp, tp): (
            string,
            string,
            CanvasLayout.pos,
            CanvasLayout.pos,
          ),
        ) => {
          let tp' = pull_back(tp, dp, 5.);
          switch (
            Util.JsUtil.get_elem_by_id_opt(CanvasView.dep_dom_id(a, b))
          ) {
          | Some(el) =>
            let (c1, c2) =
              CanvasLayout.route_link(
                ~nodes=l.nodes,
                ~from_key=a,
                ~to_key=b,
                dp,
                tp',
              );
            set_attr(el, "d", CanvasLayout.link_d(dp, c1, c2, tp'));
          | None => ()
          };
        },
        l.dep_links,
      );
      List.iter(
        (vl: CanvasLayout.value_layout) =>
          set_pos(CanvasView.value_dom_id(vl.value.v_name), vl.p.x, vl.p.y),
        l.values,
      );
      /* hull sausages track exactly (no springs for paths) */
      List.iter(
        path =>
          List.iter(
            ((id, d, _)) =>
              switch (Util.JsUtil.get_elem_by_id_opt(id)) {
              | Some(el) => set_attr(el, "d", d)
              | None => ()
              },
            CanvasView.hull_sausages_at(l, path),
          ),
        CanvasView.hull_paths_of(l),
      );
      CanvasJelly.set_targets(CanvasView.hull_targets(l));
    };
    let raf_busy = ref(false);
    let pending: ref(option((float, float))) =
      ref(None: option((float, float)));
    let follow_edges = (dx: float, dy: float) => {
      pending := Some((dx, dy));
      if (! raf_busy^) {
        raf_busy := true;
        let _ =
          Js.Unsafe.meth_call(
            Js.Unsafe.global##.window,
            "requestAnimationFrame",
            [|
              Js.Unsafe.inject(
                Js.Unsafe.callback(() => {
                  raf_busy := false;
                  switch (pending^) {
                  | Some(d) =>
                    switch (lay_with(d)) {
                    | Some(l) => apply_layout(l)
                    | None => ()
                    }
                  | None => ()
                  };
                }),
              ),
            |],
          );
        ();
      };
    };
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
        /* incident edges, labels, orbits and orbit rings follow by
           DIRECT DOM mutation — the old throttled live commits caused
           a whole-app re-render (editor included) every 120ms, which
           on large programs made dragging visibly laggy */
        follow_edges(dx, dy);
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
      /* the former is folded into the alias node now (glyph badge), so
         there is no separate former node to pin */
      switch (comp_pts, kind) {
      | _ => []
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
  let agent_busy = agent_busy || CanvasBuffer.fake_busy();
  let avatar = {
    /* while beats are on screen, the avatar target rides them (captured
       per tool at exec time) instead of reading live agent state and
       arriving ahead of the paced graph */
    let target =
      switch (CanvasBuffer.pacing_live() ? CanvasBuffer.beat_avatar^ : None) {
      | Some(_) as beat_site => beat_site
      | None => avatar_target(~editor, editors)
      };
    let resolved_from_target =
      target
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
    let resolved =
      switch (CanvasBuffer.avatar_site^) {
      | Some((x, y)) =>
        let p =
          CanvasLayout.{
            x,
            y,
          };
        last_avatar_pos := Some(p);
        Some((p, agent_busy ? "edit" : ""));
      | None => resolved_from_target
      };
    switch (resolved) {
    | Some((p, state)) =>
      /* busy with no fresh edit landing = thinking */
      Some((p, agent_busy && state == "" ? "think" : state))
    | None when agent_busy =>
      /* nothing resolved yet: embody the agent from the first thinking
         token anyway — at its last known spot, else at the viewport
         center (where the user is looking); its first hop then travels
         from there. Without this the avatar AND its reasoning bubble
         were invisible until the first tool landed. */
      let on_board = (p: CanvasLayout.pos) =>
        p.x >= (-60.)
        && p.x <= lay.width
        +. 60.
        && p.y >= (-60.)
        && p.y <= lay.height
        +. 60.;
      let p =
        switch (last_avatar_pos^) {
        | Some(p) when on_board(p) => Some(p)
        | _ =>
          switch (avail_width, avail_height) {
          | (Some(aw), Some(ah)) =>
            CanvasCamera.center(~aw, ~ah)
            |> Option.map(((x, y)) =>
                 CanvasLayout.{
                   x,
                   y,
                 }
               )
          | _ =>
            Some(
              CanvasLayout.{
                x: lay.width /. 2.,
                y: lay.height /. 2.,
              },
            )
          }
        };
      switch (p) {
      | Some(p) =>
        last_avatar_pos := Some(p);
        Some((p, "think"));
      | None => None
      };
    | None => None
    };
  };
  /* the camera judges "fits"/"all visible" against the nodes themselves,
     not the pane-sized layout box (which never fits with a margin) */
  CanvasCamera.graph_bbox :=
    (
      switch (lay.nodes) {
      | [] => None
      | ns =>
        Some(
          List.fold_left(
            ((x0, y0, x1, y1), nl: CanvasLayout.node_layout) =>
              (
                min(x0, nl.p.x -. nl.r),
                min(y0, nl.p.y -. nl.r),
                max(x1, nl.p.x +. nl.r),
                max(y1, nl.p.y +. nl.r +. 18.),
              ),
            (infinity, infinity, neg_infinity, neg_infinity),
            ns,
          ),
        )
      }
    );
  switch (avail_width, avail_height) {
  | (Some(aw), Some(ah)) =>
    CanvasCamera.note_exposure(
      ~aw,
      ~ah,
      List.map(
        (nl: CanvasLayout.node_layout) => (nl.node.key, (nl.p.x, nl.p.y)),
        lay.nodes,
      ),
    )
  | _ => ()
  };
  /* camera follow: a hop (or a resting avatar waking) hands the site to
     the camera; the dead zone decides whether it actually moves */
  switch (avatar, avail_width, avail_height) {
  | (Some((p, _)), Some(aw), Some(ah))
      when
        globals.settings.canvas_follow
        /* while beats are paced the score owns the camera (its frames
           are planned per act); the generic follow is for the rest */
        && !CanvasBuffer.pacing_live()
        && CanvasBuffer.now() >= CanvasCamera.scored_until^ =>
    let moved =
      switch (last_followed^) {
      | Some(lp: CanvasLayout.pos) =>
        abs_float(lp.x -. p.x) > 1. || abs_float(lp.y -. p.y) > 1.
      | None => true
      };
    let woke = agent_busy && ! last_followed_busy^;
    if (moved || woke) {
      last_followed := Some(p);
      CanvasCamera.follow(~aw, ~ah, (p.x, p.y));
    };
    last_followed_busy := agent_busy;
  | _ => ()
  };
  if (agent_busy != last_logged_busy^) {
    last_logged_busy := agent_busy;
    CanvasBuffer.note_agent_busy(agent_busy);
    CanvasLog.log(
      agent_busy ? "agent: busy (awaiting reply)" : "agent: idle",
    );
  };
  {
    let (pid, pstate) = last_logged_avatar^;
    let cur_state =
      switch (avatar) {
      | Some((_, st)) => st == "" ? "at rest" : st
      | None => "off"
      };
    let cur_id = last_avatar_id^;
    /* name the node the avatar landed nearest, so the journal reads
       as a story ("hop -> Recipe") */
    let site = () =>
      switch (avatar) {
      | Some((p, _)) =>
        lay.nodes
        |> List.fold_left(
             (best, nl: CanvasLayout.node_layout) => {
               let d = abs_float(nl.p.x -. p.x) +. abs_float(nl.p.y -. p.y);
               switch (best) {
               | Some((bd, _)) when bd <= d => best
               | _ => Some((d, nl.node.key))
               };
             },
             None,
           )
        |> Option.map(((_, k)) => " @ " ++ k)
        |> Option.value(~default="")
      | None => ""
      };
    if (cur_state != pstate) {
      CanvasLog.log("avatar: " ++ cur_state ++ site());
      last_logged_avatar := (cur_id, cur_state);
    } else if (cur_id != pid) {
      CanvasLog.log("avatar: hop (" ++ cur_state ++ ")" ++ site());
      last_logged_avatar := (cur_id, cur_state);
    };
  };
  /* transient action toast: the tool name of the beat just shown,
     briefly displacing the thought bubble at the same anchor */
  let avatar_toast = CanvasBuffer.current_toast();
  switch (avatar_toast) {
  | Some(_) when ! toast_tick_scheduled^ =>
    toast_tick_scheduled := true;
    Js_of_ocaml.(
      ignore(
        Js.Unsafe.global##setTimeout(
          Js.Unsafe.callback(() => {
            toast_tick_scheduled := false;
            globals.inject_global(Set(CanvasTick))
            |> Bonsai.Effect.Expert.handle;
          }),
          1150,
        ),
      )
    );
  | _ => ()
  };
  /* streaming chain-of-thought tail for the avatar's bubble: a longer
     window than fits the cloud — the ticker clips it left, so newest
     text rides the right edge and streaming pushes older text leftward
     (marquee motion paced by the model's actual thinking) */
  let avatar_bubble =
    if (agent_busy
        && globals.settings.canvas_pace
        && String.length(reasoning_tail) > 0) {
      let n = String.length(reasoning_tail);
      let tail_len = min(240, n);
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
  let const_panel =
    switch (focused_value^) {
    | Some(vn) =>
      List.find_opt((v: CanvasGraph.value) => v.v_name == vn, graph.values)
      |> Option.map(v =>
           CanvasFocus.value_info(
             ~globals,
             ~editor,
             ~inject_jump,
             ~on_close=
               () => {
                 focused_value := None;
                 globals.inject_global(Set(CanvasTick));
               },
             v,
           )
         )
    | None => None
    };
  let focus_strip =
    switch (const_panel) {
    | Some(panel) => [panel]
    | None =>
      switch (focused_ty, focused) {
      | (Some(key), _) =>
        /* module nodes stand for the implicit module type: their panel
           is the module VALUE, like any other constant */
        let module_value =
          String.length(key) >= 3 && String.sub(key, 0, 3) == "{}@"
            ? List.find_opt(
                (v: CanvasGraph.value) => v.v_key == key,
                graph.values,
              )
            : None;
        switch (module_value) {
        | Some(v) => [
            CanvasFocus.value_info(
              ~globals,
              ~editor,
              ~inject_jump,
              ~on_close=
                () =>
                  globals.inject_global(
                    Set(Sidebar(SetCanvasFocusTy(None))),
                  ),
              v,
            ),
          ]
        | None =>
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
        };
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
      }
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
          "play bursts of agent edits as separate animated beats (travel, act, settle; bigger edits dwell longer) instead of one jump-cut",
          globals.inject_global(Set(ToggleCanvasPace)),
        ),
        btn(
          ~cls=globals.settings.canvas_follow ? "tool-active" : "",
          "follow",
          "keep the agent in view: the camera glides to its work site when it hops off-center and frames the sites touched this turn",
          globals.inject_global(Set(ToggleCanvasFollow)),
        ),
        btn(
          ~on_press=
            () => {
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
              animate_fit(
                ~z_to=max(0.4, min(2.5, min(zw, zh))),
                ~lw=lay.width,
                ~lh=lay.height,
                ~aw=Option.value(~default=lay.width, avail_width),
                ~ah=Option.value(~default=lay.height, avail_height),
              );
            },
          "fit",
          "zoom so the whole graph fits the pane",
          Effect.Ignore,
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
        div(~attrs=[Attr.id("canvas-clock"), clss(["canvas-clock"])], []),
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
  switch (avail_width, avail_height) {
  | (Some(aw), Some(ah)) =>
    anchor_target :=
      (
        max(0., pan_slack +. (lay.width *. zoom -. aw) /. 2.),
        max(0., pan_slack +. (lay.height *. zoom -. ah) /. 2.),
      )
  | _ => ()
  };
  ensure_scroll_anchor(slide);
  /* hull circles chase these targets with springs (jelly): layout
     changes, drags, and collapses all wobble through CanvasJelly */
  CanvasJelly.set_targets(CanvasView.hull_targets(lay));
  {
    let (prev_slide, prev_nodes) = last_node_snapshot^;
    if (prev_slide != slide) {
      CanvasLog.log("slide: " ++ slide);
    };
    let cur_keys =
      List.map((nl: CanvasLayout.node_layout) => nl.node.key, lay.nodes);
    let removed =
      List.filter(((k, _)) => !List.mem(k, cur_keys), prev_nodes);
    if (prev_slide == slide
        && removed != []
        && CanvasBuffer.now() > collapse_fx_until^) {
      if (List.length(removed) <= 4) {
        List.iter(((_, (x, y))) => CanvasRipple.suction((x, y)), removed);
        CanvasLog.log(
          Printf.sprintf(
            "-%d node(s): %s (suction ripple)",
            List.length(removed),
            String.concat(", ", List.map(fst, removed)),
          ),
        );
      } else {
        CanvasLog.log(
          Printf.sprintf(
            "-%d nodes at once (removal effects skipped)",
            List.length(removed),
          ),
        );
      };
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
    /* ---- the beat's score (A1): ONE plan from this render's diff. The
       actor visits each new definition, pulls each new arrow, and the
       rest of the graph drifts last. Arrival times feed Animation before
       the render lands; the player runs one frame after it. ---- */
    let (_, prev_edges) = last_edge_snapshot^;
    let new_edges =
      lay.edges
      |> List.filter_map((el: CanvasLayout.edge_layout) =>
           if (List.mem(el.edge.e_name, prev_edges)) {
             None;
           } else {
             let product =
               lay.nodes
               |> List.find_opt((nl: CanvasLayout.node_layout) =>
                    nl.node.key == el.edge.e_src
                  )
               |> Util.OptUtil.and_then((nl: CanvasLayout.node_layout) =>
                    nl.node.kind == CanvasGraph.Product
                    && nl.node.parts != []
                    && !List.mem_assoc(nl.node.key, prev_nodes)
                      ? Some((nl.node.key, nl.node.parts)) : None
                  );
             Some(
               CanvasScore.{
                 name: el.edge.e_name,
                 src: el.edge.e_src,
                 dst: el.edge.dst,
                 product,
               },
             );
           }
         );
    let moved =
      lay.nodes
      |> List.filter_map((nl: CanvasLayout.node_layout) =>
           switch (List.assoc_opt(nl.node.key, prev_nodes)) {
           | Some((px, py))
               when
                 abs_float(px -. nl.p.x) > 1. || abs_float(py -. nl.p.y) > 1. =>
             Some((
               nl.node.key,
               CanvasLayout.{
                 x: px,
                 y: py,
               },
               nl.p,
             ))
           | _ => None
           }
         );
    let big_move =
      List.length(
        List.filter(
          ((_, f: CanvasLayout.pos, t: CanvasLayout.pos)) =>
            abs_float(f.x -. t.x) > 40. || abs_float(f.y -. t.y) > 40.,
          moved,
        ),
      )
      >= 3;
    let scored =
      prev_slide == slide
      && globals.settings.canvas_pace
      && (
        CanvasBuffer.pacing_live()
        && (added != [] || new_edges != [] || removed != [])
        || big_move
      );
    if (scored) {
      /* a render nobody staged (the burst-end re-frame) still needs its
         movers recorded before the patch, or the drift is a jump */
      if (!Animation.staged()) {
        CanvasBuffer.stage_beat(~slow=true, ());
      };
      List.iter(
        (nl: CanvasLayout.node_layout) => note_placed(nl.node.key),
        added,
      );
      let pos_of = k =>
        lay.nodes
        |> List.find_opt((nl: CanvasLayout.node_layout) => nl.node.key == k)
        |> Option.map((nl: CanvasLayout.node_layout) => nl.p);
      let diff =
        CanvasScore.{
          d_cause: Option.value(~default="edit", CanvasBuffer.shown_label^),
          added:
            List.map(
              (nl: CanvasLayout.node_layout) =>
                CanvasScore.{
                  key: nl.node.key,
                  anchor: Option.map(fst, nl.node.sat),
                  p: nl.p,
                },
              added,
            ),
          edges: new_edges,
          removed:
            List.map(
              ((k, (x, y))) =>
                (
                  k,
                  CanvasLayout.{
                    x,
                    y,
                  },
                ),
              removed,
            ),
          moved,
          actor: last_avatar_pos^,
        };
      let score = CanvasScore.plan(~pos_of, diff);
      let score =
        switch (avail_width, avail_height) {
        | (Some(aw), Some(ah)) when globals.settings.canvas_follow =>
          switch (CanvasCamera.center(~aw, ~ah)) {
          | Some((cx, cy)) =>
            CanvasScore.with_frames(
              ~pane=(aw, ah),
              ~cur=
                CanvasScore.{
                  center:
                    CanvasLayout.{
                      x: cx,
                      y: cy,
                    },
                  zoom: CanvasCamera.zoom_now^,
                },
              ~pos_of,
              ~all_keys=cur_keys,
              ~exposed=CanvasCamera.exposed_keys(),
              score,
            )
          | None => score
          }
        | _ => score
        };
      /* Appear/Pill times drive Animation's arrivals; movers wait for
         the drift act */
      let pills =
        CanvasScore.effects_abs(score)
        |> List.filter_map(((t, _, e: CanvasScore.timed_effect)) =>
             switch (e.effect) {
             | Pill(name) => Some((CanvasView.edge_dom_id(name), t))
             | _ => None
             }
           );
      Animation.set_arrival_schedule(
        List.map(
          ((k, t)) => (CanvasView.node_dom_id(k), t),
          CanvasScore.appear_times(score),
        )
        @ pills,
      );
      Animation.set_movers_at(CanvasScore.drift_at(score));
      {
        /* new geometry follows its act: formation/dep lines draw on right
           after the node they attach to blooms; new function paths stay
           hidden until the actor rides them; label leaders come with pills */

        let appear = CanvasScore.appear_times(score);
        let at_key = k => List.assoc_opt(k, appear);
        let pair = (a, b, id) =>
          switch (at_key(a), at_key(b)) {
          | (Some(ta), Some(tb)) => [(id, Some(max(ta, tb) + 80))]
          | (Some(t), None)
          | (None, Some(t)) => [(id, Some(t + 80))]
          | (None, None) => []
          };
        /* lines into a product the edge act FORMS are drawn by that act */
        let formed =
          List.filter_map(
            (e: CanvasScore.new_edge) => Option.map(fst, e.product),
            new_edges,
          );
        let forms =
          List.concat_map(
            ((a, b, _, _)) =>
              List.mem(b, formed)
                ? [(CanvasView.formation_dom_id(a, b), Option.none)]
                : pair(a, b, CanvasView.formation_dom_id(a, b)),
            lay.formations,
          );
        let deps =
          List.concat_map(
            ((a, b, _, _)) => pair(a, b, CanvasView.dep_dom_id(a, b)),
            lay.dep_links,
          );
        let (draws, leaders) =
          CanvasScore.effects_abs(score)
          |> List.fold_left(
               ((ds, ls), (t, _, e: CanvasScore.timed_effect)) =>
                 switch (e.effect) {
                 | Draw(name) => (
                     [(CanvasView.path_dom_id(name), Option.none), ...ds],
                     ls,
                   )
                 | Pill(name) => (
                     ds,
                     [
                       ("clead-" ++ CanvasView.sanitize(name), Some(t)),
                       ...ls,
                     ],
                   )
                 | _ => (ds, ls)
                 },
               ([], []),
             );
        Animation.set_geom_schedule(forms @ deps @ draws @ leaders);
      };
      CanvasBuffer.extend_dwell(float_of_int(score.total_ms) +. 600.);
      CanvasLog.log(CanvasScore.to_string(score));
      List.iter(
        v => CanvasLog.log("SCORE: " ++ v),
        CanvasScore.validate(score),
      );
      let zoom = CanvasCamera.zoom_now^;
      CanvasEnact.jump_snapshot(~zoom);
      CanvasEnact.after_render(() => {
        CanvasEnact.check_jumps(~zoom);
        CanvasEnact.play(~zoom, score);
      });
    } else if (prev_slide == slide && CanvasBuffer.in_burst() && added != []) {
      CanvasLog.log(
        Printf.sprintf("+%d node(s) (unscored render)", List.length(added)),
      );
    };
    {
      /* data-flow pulses: when a function's output samples GROW, send a
         pulse along its edge (throttled per edge; first sighting seeds
         the count silently so slide loads don't storm) */

      let nowt = CanvasBuffer.now();
      let next_state =
        List.filter_map(
          (el: CanvasLayout.edge_layout) =>
            switch (el.edge.e_out_id) {
            | None => None
            | Some(out_id) =>
              let n =
                switch (Language.Sample.Map.lookup(out_id, editor.dynamics)) {
                | Some(ss) => List.length(ss)
                | None => 0
                };
              let (prev_n, last_t) =
                switch (List.assoc_opt(el.edge.e_name, edge_pulse_state^)) {
                | Some(st) => st
                | None => ((-1), 0.)
                };
              let fire = prev_n >= 0 && n > prev_n && nowt -. last_t > 1000.;
              if (fire) {
                CanvasRipple.pulse_edge(
                  (el.src_p.x, el.src_p.y),
                  (el.c1.x, el.c1.y),
                  (el.c2.x, el.c2.y),
                  (el.dst_p.x, el.dst_p.y),
                );
              };
              Some((el.edge.e_name, (n, fire ? nowt : last_t)));
            },
          lay.edges,
        );
      edge_pulse_state := next_state;
    };
    /* layout churn: how much did EXISTING nodes move this render?
       (the metric behind "the layout suddenly reshuffled") */
    let moved =
      List.filter_map(
        (nl: CanvasLayout.node_layout) =>
          switch (List.assoc_opt(nl.node.key, prev_nodes)) {
          | Some((px, py)) =>
            let d = max(abs_float(nl.p.x -. px), abs_float(nl.p.y -. py));
            d > 21. ? Some(d) : None;
          | None => None
          },
        lay.nodes,
      );
    if (prev_slide == slide && moved != [] && ! drag_active^) {
      CanvasLog.log(
        Printf.sprintf(
          "layout: %d/%d nodes moved (max %.0fpx)",
          List.length(moved),
          List.length(lay.nodes),
          List.fold_left(max, 0., moved),
        ),
      );
    };
    /* record positions (grid cells) whenever the layout meaningfully
       changed, for post-hoc before/after diffing */
    if (! drag_active^
        && (prev_slide != slide || moved != [] || added != [] || removed != [])) {
      let pos_line =
        lay.nodes
        |> List.map((nl: CanvasLayout.node_layout) =>
             Printf.sprintf(
               "%s(%.0f,%.0f)",
               nl.node.key,
               nl.p.x /. 14.,
               nl.p.y /. 14.,
             )
           )
        |> String.concat(" ");
      CanvasLog.record_layout(
        CanvasLog.stamp() ++ " " ++ slide ++ " | " ++ pos_line,
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

    /* the edge set for the next render's diff (edges are enacted by the
       score planned above) */
    let cur_edges =
      List.map((el: CanvasLayout.edge_layout) => el.edge.e_name, lay.edges);
    last_edge_snapshot := (slide, cur_edges);
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
  /* dependency fan: subdued under-layer curves from the hovered or
     focused function's pill to what it references */
  let dep_fan = {
    let fan_for = (name: string) =>
      switch (
        List.find_opt(
          (el: CanvasLayout.edge_layout) => el.edge.e_name == name,
          lay.edges,
        )
      ) {
      | None => []
      | Some(el) =>
        let uses =
          el.edge.e_deps
          |> List.filter_map(dep => {
               let tgt =
                 switch (
                   List.find_opt(
                     (dl: CanvasLayout.edge_layout) => dl.edge.e_name == dep,
                     lay.edges,
                   )
                 ) {
                 | Some(dl) => Some(dl.label_p)
                 | None =>
                   List.find_opt(
                     (vl: CanvasLayout.value_layout) =>
                       vl.value.v_name == dep,
                     lay.values,
                   )
                   |> Option.map((vl: CanvasLayout.value_layout) => vl.p)
                 };
               tgt |> Option.map(t => (el.label_p, t));
             });
        /* and the reverse: who uses THIS function */
        let used_by =
          lay.edges
          |> List.filter_map((dl: CanvasLayout.edge_layout) =>
               dl.edge.e_name != name && List.mem(name, dl.edge.e_deps)
                 ? Some((dl.label_p, el.label_p)) : None
             );
        uses @ used_by;
      };
    switch (hovered_edge^, focused) {
    | (Some(n), _)
    | (None, Some(n)) => fan_for(n)
    | (None, None) => []
    };
  };
  let on_hull_toggle = (root: string) => {
    let collapsing = !List.mem(root, collapsed_modules^);
    collapsed_modules :=
      collapsing
        ? [root, ...collapsed_modules^]
        : List.filter(r => r != root, collapsed_modules^);
    collapse_fx_until := CanvasBuffer.now() +. 900.;
    /* the effect emanates from the module node itself */
    switch (
      Util.JsUtil.get_elem_by_id_opt(CanvasView.node_dom_id("{}@" ++ root))
    ) {
    | Some(el) =>
      open Js_of_ocaml;
      let st = Js.Unsafe.coerce(el)##.style;
      let px = (v: Js.t(Js.js_string)): float => {
        let str = Js.to_string(v);
        switch (String.index_opt(str, 'p')) {
        | Some(i) =>
          Option.value(
            ~default=0.,
            float_of_string_opt(String.sub(str, 0, i)),
          )
        | None => 0.
        };
      };
      let l = px(st##.left)
      and tp = px(st##.top);
      let r = float_of_int(Js.Unsafe.coerce(el)##.offsetWidth) /. 2.;
      collapsing
        ? CanvasRipple.suction((l +. r, tp +. r))
        : CanvasRipple.splash(~amp=8., (l +. r, tp +. r));
    | None => ()
    };
    globals.inject_global(Set(CanvasTick));
  };
  let on_edge_hover = (h: option(string)) => {
    hovered_edge := h;
    globals.inject_global(Set(CanvasTick));
  };
  /* clicking empty canvas dismisses whichever info panel is open */
  let on_canvas_plain_click = () =>
    if (focused != None || focused_ty != None || focused_value^ != None) {
      focused_value := None;
      Effect.Many([
        globals.inject_global(Set(Sidebar(SetCanvasFocus(None)))),
        globals.inject_global(Set(Sidebar(SetCanvasFocusTy(None)))),
        globals.inject_global(Set(CanvasTick)),
      ]);
    } else {
      Effect.Ignore;
    };
  /* clicking a value: focus it AND select its definition, as functions do */
  let on_value_click = (v: CanvasGraph.value) => {
    focused_value := Some(v.v_name);
    Effect.Many([
      globals.inject_global(Set(Sidebar(SetCanvasFocusTy(None)))),
      globals.inject_global(Set(Sidebar(SetCanvasFocus(None)))),
      globals.inject_global(Set(CanvasTick)),
      globals.inject_global(SelectTile(v.v_id)),
      Effect.Stop_propagation,
    ]);
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
          /* the dot field: a viewport-fixed canvas UNDER the board
             (sticky 0x0 holder), redrawn on scroll/zoom with the
             lattice window + edge fade (CanvasRipple) */
          div(
            ~attrs=[clss(["canvas-dots-holder"])],
            [
              Node.create(
                "canvas",
                ~attrs=[Attr.id("canvas-dots"), clss(["canvas-dots"])],
                [],
              ),
            ],
          ),
          /* CanvasView.view now provides its own .canvas-pan-pad
             wrapper (gesture layer spans the slack ring) */
          CanvasView.view(
            ~inject_jump,
            ~collapsed_counts,
            ~on_hull_toggle,
            ~dep_fan,
            ~on_edge_hover,
            ~on_value_click=Some(on_value_click),
            ~on_canvas_plain_click,
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
            ~avatar_toast,
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
    ]
    @ focus_strip
    @ [legend, menu_layer],
  );
};

/* ---- instrumentation (E1): a slow canvas render says where the time went ---- */
let view =
    (
      ~globals: Globals.t,
      ~editors: Editors.Model.t,
      ~editors_inject: Editors.Update.t => Effect.t(unit),
      ~editor: CodeWithStatics.Model.t,
      ~use_sidebar_width=true,
      (),
    )
    : Node.t => {
  let t0 = CanvasBuffer.now();
  CanvasLayout.layout_calls := 0;
  CanvasLayout.layout_ms := 0.;
  let r =
    view_impl(
      ~globals,
      ~editors,
      ~editors_inject,
      ~editor,
      ~use_sidebar_width,
      (),
    );
  let ms = CanvasBuffer.now() -. t0;
  if (ms > 120.) {
    CanvasLog.log(
      Printf.sprintf(
        "slow: canvas view %.0fms (layout %d call(s) %.0fms)",
        ms,
        CanvasLayout.layout_calls^,
        CanvasLayout.layout_ms^,
      ),
    );
  };
  r;
};
