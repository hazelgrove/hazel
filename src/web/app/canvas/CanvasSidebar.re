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
  let offsets =
    globals.settings.canvas_node_offsets
    |> List.filter_map((((s, k), d)) => s == slide ? Some((k, d)) : None);
  /* stretch columns to fill the panel when the graph is narrower than it;
     panel width is read from the (pre-patch) DOM, so the first render
     after a panel switch or drag-resize uses the previous width */
  let avail_width =
    switch (use_sidebar_width ? globals.settings.sidebar.width : None) {
    | Some(w) => Some(float_of_int(w) -. 6.)
    | None =>
      switch (Util.JsUtil.get_elem_by_id_opt("canvas-sidebar")) {
      | Some(el) =>
        let w = Js_of_ocaml.Js.Unsafe.coerce(el)##.offsetWidth;
        w > 50 ? Some(float_of_int(w)) : None;
      | None => None
      }
    };
  let lay = {
    let base = CanvasLayout.layout(~offsets, graph);
    switch (avail_width) {
    | Some(avail) when base.width < avail -. 24. =>
      /* stretching scales grid columns only (satellite/label extents are
         fixed), so a first fit undershoots; one secant step closes most
         of the gap */
      let target = avail -. 24.;
      let s1 = min(1.8, target /. base.width);
      if (s1 <= 1.02) {
        base;
      } else {
        let l1 = CanvasLayout.layout(~x_scale=s1, ~offsets, graph);
        if (l1.width >= target -. 30. || s1 >= 1.8) {
          l1;
        } else {
          let s2 = min(1.8, s1 *. target /. l1.width);
          CanvasLayout.layout(~x_scale=s2, ~offsets, graph);
        };
      };
    | _ => base
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
  /* ---- canvas authoring: paste real code stubs into the program ---- */
  let perform = (a: Haz3lcore.Action.t) =>
    editors_inject(
      Editors.Update.Scratch(
        ScratchMode.Update.CellAction(
          CellEditor.Update.MainEditor(CodeEditable.Update.Perform(a)),
        ),
      ),
    );
  /* paste trims trailing whitespace, so the break after a stub is an
     explicit linebreak insert (what Enter dispatches) */
  let insert_at_start = (text: string) =>
    Effect.Many([
      perform(Move(Start)),
      perform(Paste(text)),
      perform(Insert(Haz3lcore.Token.linebreak)),
    ]);
  /* function stubs go after all definitions: just before the first test
     or the result expression */
  let insert_before_anchor = (text: string) =>
    switch (graph.insert_anchor) {
    | Some(id) =>
      /* Goal(TileId) can land on the far side of the anchor's first token;
         snapping to line start puts the paste before the whole anchor */
      Effect.Many([
        perform(Move(Goal(TileId(id)))),
        perform(Move(Line(Left))),
        perform(Paste(text)),
        perform(Insert(Haz3lcore.Token.linebreak)),
      ])
    | None => insert_at_start(text)
    };
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
    let base =
      Option.value(~default=(0., 0.), List.assoc_opt(n.key, offsets));
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
      let dx = float_of_int(x - sx)
      and dy = float_of_int(y - sy);
      if (abs_float(dx) +. abs_float(dy) > 4.) {
        moved := true;
      };
      if (moved^) {
        delta := (dx, dy);
        switch (orig) {
        | Some((el, l, t)) =>
          let st = Js.Unsafe.coerce(el)##.style;
          st##.left := Js.string(Printf.sprintf("%.1fpx", l +. dx));
          st##.top := Js.string(Printf.sprintf("%.1fpx", t +. dy));
        | None => ()
        };
      };
      ();
    }
    and on_up = _ => {
      let doc = Js.Unsafe.coerce(Dom_html.document);
      let _ = doc##removeEventListener("mousemove", on_move);
      let _ = doc##removeEventListener("mouseup", on_up);
      let (dx, dy) = delta^;
      Effect.Expert.handle_non_dom_event_exn(
        moved^
          ? globals.inject_global(
              Set(
                SetCanvasNodeOffset(
                  slide,
                  n.key,
                  fst(base) +. dx,
                  snd(base) +. dy,
                ),
              ),
            )
          : click_effect(n),
      );
      ();
    };
    let doc = Js.Unsafe.coerce(Dom_html.document);
    let _ = doc##addEventListener("mousemove", on_move);
    let _ = doc##addEventListener("mouseup", on_up);
    Effect.Prevent_default;
  };
  let on_node_mousedown = (n: CanvasGraph.tynode, evt) =>
    switch (connect) {
    | Some(None) =>
      Effect.Many([
        set_connect(Some(Some(n.key))),
        Effect.Stop_propagation,
        Effect.Prevent_default,
      ])
    | Some(Some(src_key)) =>
      let src =
        List.find_opt(
          (m: CanvasGraph.tynode) => m.key == src_key,
          graph.nodes,
        )
        |> Option.map(ty_syntax)
        |> Option.value(~default="?");
      let stub =
        Printf.sprintf(
          "let %s : %s -> %s = ? in",
          fresh_name("f"),
          src,
          ty_syntax(n),
        );
      Effect.Many([
        insert_before_anchor(stub),
        set_connect(None),
        Effect.Stop_propagation,
        Effect.Prevent_default,
      ]);
    | None => start_node_drag(n, evt)
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
  let avatar =
    avatar_target(~editor, editors)
    |> Util.OptUtil.and_then(((id, state)) =>
         locate(~info_map=editor.statics.info_map, lay, id)
         |> Option.map(p => (p, state))
       );
  let n_tests =
    List.length(graph.loose_tests)
    + List.fold_left(
        (acc, e: CanvasGraph.edge) => acc + List.length(e.tests),
        0,
        graph.edges,
      );
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
      [
        div(~attrs=[clss(["canvas-title"])], [text("Constellation")]),
        split_btn,
        div(
          ~attrs=[clss(["canvas-stats"])],
          [
            text(
              Printf.sprintf(
                "%d types · %d functions · %d tests",
                List.length(graph.nodes),
                List.length(graph.edges),
                n_tests,
              ),
            ),
          ],
        ),
      ],
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
        ~inject_jump,
        ~on_close=set_focus(None),
        ~dynamics=editor.dynamics,
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
    div(
      ~attrs=[clss(["canvas-toolbar"])],
      [
        btn(
          "+ type",
          "add a stub type alias: type T = ? in (rename it in the code)",
          insert_at_start(
            Printf.sprintf("type %s = ? in", fresh_name("T")),
          ),
        ),
        btn(
          "+ tuple",
          "add a tuple former: type T = (?, ?) in — fill the holes with types",
          insert_at_start(
            Printf.sprintf("type %s = (?, ?) in", fresh_name("T")),
          ),
        ),
        btn(
          "+ list",
          "add a list former: type T = [?] in",
          insert_at_start(
            Printf.sprintf("type %s = [?] in", fresh_name("T")),
          ),
        ),
        btn(
          ~cls=connect == None ? "" : "tool-active",
          "+ fn",
          "draw a function: click a source node, then a target node; creates let f : A -> B = ? in",
          set_connect(connect == None ? Some(None) : None),
        ),
      ]
      @ (
        switch (connect) {
        | Some(None) => [
            span(
              ~attrs=[clss(["tool-hint"])],
              [text({js|pick the source node…|js})],
            ),
          ]
        | Some(Some(src)) => [
            span(
              ~attrs=[clss(["tool-hint"])],
              [text(src ++ {js| ⟶ pick the target node…|js})],
            ),
          ]
        | None => []
        }
      )
      @ (
        offsets == []
          ? []
          : [
            btn(
              "reset layout",
              "clear manual node positions for this slide",
              globals.inject_global(Set(ClearCanvasNodeOffsets(slide))),
            ),
          ]
      ),
    );
  };
  div(
    ~attrs=[Attr.id("canvas-sidebar")],
    [
      header,
      toolbar,
      div(
        ~attrs=[clss(["canvas-scroll"])],
        [
          CanvasView.view(
            ~inject_jump,
            ~on_edge_click,
            ~on_node_mousedown,
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
