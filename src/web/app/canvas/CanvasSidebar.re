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
                  let mx: float =
                    Js.Unsafe.coerce(evt)##.clientX
                    -.
                    Js.Unsafe.coerce(rect)##.left;
                  let my: float =
                    Js.Unsafe.coerce(evt)##.clientY
                    -.
                    Js.Unsafe.coerce(rect)##.top;
                  let sl: float = Js.Unsafe.coerce(el')##.scrollLeft
                  and st: float = Js.Unsafe.coerce(el')##.scrollTop;
                  ignore(
                    Js.Unsafe.global##setTimeout(
                      Js.Unsafe.callback(() => {
                        let r = z /. z0;
                        Js.Unsafe.coerce(el')##.scrollLeft :=
                          (sl +. mx) *. r -. mx;
                        Js.Unsafe.coerce(el')##.scrollTop :=
                          (st +. my) *. r -. my;
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
  let lay = {
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
  /* clicking a function: focus it in the detail strip AND select its def */
  let on_edge_click = (e: CanvasGraph.edge) =>
    Effect.Many([
      set_focus(Some(e.e_name)),
      globals.inject_global(SelectTile(e.e_id)),
      Effect.Stop_propagation,
    ]);
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
      };
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
        Effect.Many([
          insert_stub(stub),
          set_connect(None),
          Effect.Stop_propagation,
          Effect.Prevent_default,
        ]);
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
  /* a canvas-background click in place mode inserts the stub and pins the
     new node where you clicked (pin recorded in the pre-normalization
     frame via lay.origin) */
  let on_canvas_click: option(((float, float)) => Effect.t(unit)) =
    switch (place) {
    | None => None
    | Some((kind, comps)) =>
      Some(
        ((x, y)) => {
          let name = fresh_name("T");
          let body =
            switch (kind, comps) {
            | ("tuple", []) => "(?, ?)"
            | ("tuple", [a]) => "(" ++ a ++ ", ?)"
            | ("tuple", cs) => "(" ++ String.concat(", ", cs) ++ ")"
            | ("list", [a]) => "[" ++ a ++ "]"
            | ("list", _) => "[?]"
            | _ => "?"
            };
          Effect.Many([
            insert_stub(Printf.sprintf("type %s = %s in", name, body)),
            globals.inject_global(
              Set(
                SetCanvasNodePin(
                  slide,
                  name,
                  x -. lay.origin.x,
                  y -. lay.origin.y,
                ),
              ),
            ),
            set_place(None),
            Effect.Stop_propagation,
          ]);
        },
      )
    };
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
  let header =
    div(
      ~attrs=[clss(["canvas-header"])],
      [div(~attrs=[clss(["canvas-title"])], [text("Constellation")])],
    );
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
        item("lg-form", {js|┈▶|js}, "forms tuple"),
        item("lg-dep", {js|─▶|js}, "made of"),
        item("lg-hole", {js|╌╌|js}, "unwritten (hole)"),
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
    let btn = (~cls="", label, tooltip, eff) =>
      div(
        ~attrs=[
          clss(["canvas-tool-btn"] @ (cls == "" ? [] : [cls])),
          Attr.on_click(_ => eff),
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
          "+ type",
          "stub type: click the canvas where it should go; creates type T = ? in",
        ),
        mode_btn(
          "tuple",
          "+ tuple",
          "tuple former: click component nodes in order, then the canvas to place; creates type T = (A, B) in",
        ),
        mode_btn(
          "list",
          "+ list",
          "list former: click the element node, then the canvas to place; creates type T = [A] in",
        ),
        btn(
          ~cls=connect == None ? "" : "tool-active",
          "+ fn",
          "draw a function: click a source node then a target node (shift-click collects several sources into a tuple input); creates let f : A -> B = ? in",
          set_connect(connect == None ? Some([]) : None),
        ),
      ]
      @ (
        switch (connect, place) {
        | (Some([]), _) => [
            span(
              ~attrs=[clss(["tool-hint"])],
              [text({js|pick the source node…|js})],
            ),
          ]
        | (Some(srcs), _) => [
            span(
              ~attrs=[clss(["tool-hint"])],
              [
                text(
                  String.concat(", ", srcs)
                  ++ {js| ⟶ click the target (shift-click adds a source)…|js},
                ),
              ],
            ),
          ]
        | (None, Some(("type", _))) => [
            span(
              ~attrs=[clss(["tool-hint"])],
              [text({js|click the canvas to place…|js})],
            ),
          ]
        | (None, Some((_, comps))) => [
            span(
              ~attrs=[clss(["tool-hint"])],
              [
                text(
                  (comps == [] ? "" : String.concat(", ", comps) ++ " — ")
                  ++ {js|click nodes to add, the canvas to place…|js},
                ),
              ],
            ),
          ]
        | (None, None) => []
        }
      )
      @ [
        btn(
          ~cls=globals.settings.canvas_pace ? "tool-active" : "",
          "pace",
          "play bursts of agent edits as separate animated beats (max ~1.4/s) instead of one jump-cut",
          globals.inject_global(Set(ToggleCanvasPace)),
        ),
        btn(
          "fit",
          "zoom so the whole graph fits the pane",
          {
            let zw =
              switch (avail_width) {
              | Some(w) => (w -. 10.) /. max(1., lay.width)
              | None => 1.
              };
            let zh =
              switch (avail_height) {
              | Some(h) => (h -. 10.) /. max(1., lay.height)
              | None => 1.
              };
            globals.inject_global(
              Set(SetCanvasZoom(max(0.4, min(2.5, min(zw, zh))))),
            );
          },
        ),
        btn(
          ~cls=offsets == [] && pins == [] ? "tool-disabled" : "",
          "reset layout",
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
  div(
    ~attrs=[Attr.id("canvas-sidebar")],
    [
      header,
      toolbar,
      div(
        ~attrs=[Attr.id("canvas-scroll"), clss(["canvas-scroll"])],
        [
          CanvasView.view(
            ~inject_jump,
            ~on_edge_click,
            ~on_node_mousedown,
            ~on_canvas_click,
            ~zoom,
            ~avatar_bubble,
            ~min_size=(
              Option.value(~default=0., avail_width) /. zoom,
              Option.value(~default=0., avail_height) /. zoom,
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
    @ [legend],
  );
};
