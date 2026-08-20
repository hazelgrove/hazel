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
      ~editor: CodeWithStatics.Model.t,
      /* false when hosted in the main-area split, whose width is the
         pane's own, not the sidebar setting's */
      ~use_sidebar_width=true,
      (),
    )
    : Node.t => {
  let test_results = test_results_of(editors);
  let graph = CanvasGraph.extract(~test_results?, editor.statics);
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
    let base = CanvasLayout.layout(graph);
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
        let l1 = CanvasLayout.layout(~x_scale=s1, graph);
        if (l1.width >= target -. 30. || s1 >= 1.8) {
          l1;
        } else {
          let s2 = min(1.8, s1 *. target /. l1.width);
          CanvasLayout.layout(~x_scale=s2, graph);
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
  /* clicking a type: focus its observed values; aliases also select */
  let on_node_click = (n: CanvasGraph.tynode) =>
    Effect.Many(
      [
        globals.inject_global(Set(Sidebar(SetCanvasFocusTy(Some(n.key))))),
      ]
      @ (
        switch (n.n_id) {
        | Some(id) => [globals.inject_global(SelectTile(id))]
        | None => []
        }
      )
      @ [Effect.Stop_propagation],
    );
  let focused = globals.settings.sidebar.canvas_focus;
  let focused_ty = globals.settings.sidebar.canvas_focus_ty;
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
        ~graph,
        name,
      )
      |> Option.to_list
    | (None, None) => []
    };
  div(
    ~attrs=[Attr.id("canvas-sidebar")],
    [
      header,
      div(
        ~attrs=[clss(["canvas-scroll"])],
        [
          CanvasView.view(
            ~inject_jump,
            ~on_edge_click,
            ~on_node_click,
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
