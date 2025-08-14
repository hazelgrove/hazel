open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

[@deriving (show({with_path: false}), sexp, yojson)]
type position = {
  x: float,
  y: float,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type node = {
  id: string,
  label: string,
  position,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type edge = {
  from_id: string,
  to_id: string,
  label: string,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type graph = {
  nodes: list(node),
  edges: list(edge),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type model = {
  selected_node: option(string),
  dragging: option(string),
  drag_offset: position,
  creating_edge_from: option(string), // First node selected for edge creation
  editing_node: option(string) // Node currently being edited
};

[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | SelectNode(string)
  | StartDrag(string, position)
  | UpdateDrag(position)
  | EndDrag
  | MoveNode(string, position)
  | AddNode(position)
  | RemoveNode(string)
  | AddEdge(string, string, string) // from_id, to_id, label
  | RemoveEdge(string, string)
  | UpdateNodeLabel(string, string)
  | UpdateEdgeLabel(string, string, string)
  | StartEdgeCreation(string) // Start creating edge from this node
  | CancelEdgeCreation
  | CreateNodeAt(position) // Create new node at position
  | StartEditingNode(string) // Start editing node label
  | FinishEditingNode(string, string); // node_id, new_label

let model_of_sexp = (sexp: Sexplib.Sexp.t): model =>
  switch (model_of_sexp(sexp)) {
  | exception _ => {
      selected_node: None,
      dragging: None,
      drag_offset: {
        x: 0.,
        y: 0.,
      },
      creating_edge_from: None,
      editing_node: None,
    }
  | m => m
  };

module SyntaxTerm = {
  open IdTagged.FreshGrammar;
  open OptUtil.Syntax;

  let position_to_exp = ({x, y}: position): Term.Exp.t =>
    Exp.tuple([Exp.float(x), Exp.float(y)]);

  let node_to_exp = ({id, label, position}: node): Term.Exp.t =>
    Exp.tuple([
      Exp.string(id),
      Exp.string(label),
      position_to_exp(position),
    ]);

  let edge_to_exp = ({from_id, to_id, label}: edge): Term.Exp.t =>
    Exp.tuple([Exp.string(from_id), Exp.string(to_id), Exp.string(label)]);

  let graph_to_exp = ({nodes, edges}: graph): Term.Exp.t =>
    Exp.parens(
      Exp.tuple([
        Exp.list_lit(List.map(node_to_exp, nodes)),
        Exp.list_lit(List.map(edge_to_exp, edges)),
      ]),
    );

  let syntax_to_any = (graph: graph): Term.Any.t =>
    Exp(graph_to_exp(graph));

  let exp_to_position = (term: Term.Exp.t): option(position) =>
    switch (term.term) {
    | Parens({term: Tuple([x_term, y_term]), _})
    | Tuple([x_term, y_term]) =>
      switch (x_term.term, y_term.term) {
      | (Atom(Float(x)), Atom(Float(y))) =>
        Some({
          x,
          y,
        })
      | _ => None
      }
    | _ => None
    };

  let exp_to_node = (term: Term.Exp.t): option(node) =>
    switch (term.term) {
    | Parens({term: Tuple([id_term, label_term, pos_term]), _})
    | Tuple([id_term, label_term, pos_term]) =>
      // print_endline("id_term:" ++ Term.Exp.show(id_term));
      // print_endline("label_term:" ++ Term.Exp.show(label_term));
      // print_endline("pos_term:" ++ Term.Exp.show(pos_term));
      switch (id_term.term, label_term.term) {
      | (Atom(String(id)), Atom(String(label))) =>
        Printf.eprintf("GraphProj: Found node: %s, %s\n%!", id, label);
        let+ position = exp_to_position(pos_term);
        {
          id,
          label,
          position,
        };
      | _ =>
        // print_endline("exp_to_node: not a string tuple");
        None
      }
    | _ =>
      // print_endline("exp_to_node: not a tuple");
      None
    };

  let exp_to_edge = (term: Term.Exp.t): option(edge) =>
    switch (term.term) {
    | Parens({term: Tuple([from_term, to_term, label_term]), _})
    | Tuple([from_term, to_term, label_term]) =>
      switch (from_term.term, to_term.term, label_term.term) {
      | (
          Atom(String(from_id)),
          Atom(String(to_id)),
          Atom(String(label)),
        ) =>
        // Printf.eprintf(
        //   "GraphProj: Found edge: %s -> %s (%s)\n%!",
        //   from_id,
        //   to_id,
        //   label,
        // );
        Some({
          from_id,
          to_id,
          label,
        })
      | _ =>
        // print_endline("exp_to_edge: not a string triple");
        None
      }
    | _ => None
    };

  let rec exp_to_graph = (term: Term.Exp.t): option(graph) =>
    switch (term.term) {
    | Parens(inner) =>
      // Printf.eprintf("GraphProj: Found Parens, unwrapping\n%!");
      exp_to_graph(inner)
    | Tuple(elements) =>
      // Printf.eprintf(
      //   "GraphProj: Found tuple with %d elements\n%!",
      //   List.length(elements),
      // );
      switch (elements) {
      | [nodes_term, edges_term] =>
        Printf.eprintf("GraphProj: Tuple has exactly 2 elements\n%!");
        switch (nodes_term.term, edges_term.term) {
        | (ListLit(node_terms), ListLit(edge_terms)) =>
          // Printf.eprintf(
          //   "GraphProj: Found lists - nodes:%d, edges:%d\n%!",
          //   List.length(node_terms),
          //   List.length(edge_terms),
          // );
          let* nodes =
            node_terms |> List.map(exp_to_node) |> OptUtil.sequence;
          let+ edges =
            edge_terms |> List.map(exp_to_edge) |> OptUtil.sequence;
          Printf.eprintf("GraphProj: Successfully parsed graph\n%!");
          {
            nodes,
            edges,
          };
        | (ListLit(_), _) =>
          // Printf.eprintf("GraphProj: First is ListLit, second is not\n%!");
          None
        | (_, ListLit(_)) =>
          // Printf.eprintf("GraphProj: Second is ListLit, first is not\n%!");
          None
        | _ =>
          // Printf.eprintf("GraphProj: Neither element is ListLit\n%!");
          None
        };
      | _ =>
        // Printf.eprintf(
        //   "GraphProj: Tuple doesn't have exactly 2 elements\n%!",
        // );
        None
      }
    | ListLit(_) =>
      // Printf.eprintf("GraphProj: Found ListLit instead of Tuple\n%!");
      None
    | Atom(_) =>
      // Printf.eprintf("GraphProj: Found Atom instead of Tuple\n%!");
      None
    | _ =>
      // Printf.eprintf("GraphProj: Found some other term type\n%!");
      None
    };

  let any_to_graph = (term: Term.Any.t): option(graph) =>
    switch (term) {
    | Exp(term) =>
      // Printf.eprintf("GraphProj: Trying to parse Exp term\n%!");
      exp_to_graph(term)
    | _ =>
      // Printf.eprintf("GraphProj: Not an Exp term\n%!");
      None
    };

  let put = (info, graph): option(Base.segment) =>
    info.utility.lift_syntax(_ => syntax_to_any(graph), info.syntax);

  let get_opt = (any: Any.t): option(graph) => any_to_graph(any);

  let get = (info: info): graph =>
    switch (info.syntax |> info.utility.seg_to_term) {
    | Some(syntax) =>
      switch (get_opt(syntax)) {
      | Some(graph) => graph
      | None => failwith("Graph: Get: not a graph")
      }
    | None => failwith("Graph: Get: seg_to_term failed")
    };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type m = model;
[@deriving (show({with_path: false}), sexp, yojson)]
type a = action;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = m;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = a;

  let focus_keyboard = (id: Id.t, d: Direction.t) => {
    JsUtil.get_elem_by_id(Id.cls(id))##focus;
    switch (d) {
    | Left => ()
    | Right => ()
    };
  };

  let focus_pointer = (id: Id.t) => {
    JsUtil.get_elem_by_id(Id.cls(id))##focus;
  };

  let focusable =
    Focusable.{
      pointer: Some(focus_pointer),
      keyboard: Some(focus_keyboard),
    };
  let dynamics = false;

  let init = (any: Language.Any.t): option(model) =>
    switch (SyntaxTerm.get_opt(any)) {
    | Some(_) =>
      Some({
        selected_node: None,
        dragging: None,
        drag_offset: {
          x: 0.,
          y: 0.,
        },
        creating_edge_from: None,
        editing_node: None,
      })
    | None => None
    };

  let placeholder = (_, _): ProjectorCore.Shape.t => {
    horizontal: 40,
    vertical: Block(12),
  };

  let update = (model, _info, action) =>
    switch (action) {
    | SelectNode(id) => {
        ...model,
        selected_node: Some(id),
        creating_edge_from: None, // Cancel edge creation when selecting normally
        editing_node: None // Cancel editing when selecting another node
      }
    | StartDrag(id, offset) => {
        ...model,
        dragging: Some(id),
        drag_offset: offset,
      }
    | UpdateDrag(pos) => model // Will be handled by parent via SetSyntax
    | EndDrag => {
        ...model,
        dragging: None,
      }
    | StartEdgeCreation(node_id) => {
        ...model,
        creating_edge_from: Some(node_id),
        selected_node: Some(node_id),
        editing_node: None // Cancel editing when starting edge creation
      }
    | CancelEdgeCreation => {
        ...model,
        creating_edge_from: None,
      }
    | StartEditingNode(node_id) => {
        ...model,
        editing_node: Some(node_id),
        creating_edge_from: None // Cancel edge creation when starting edit
      }
    | FinishEditingNode(_, _) => {
        ...model,
        editing_node: None,
      }
    | MoveNode(_, _) => model // Will be handled by parent via SetSyntax
    | AddNode(_) => model // Will be handled by parent via SetSyntax
    | RemoveNode(_) => model // Will be handled by parent via SetSyntax
    | CreateNodeAt(_) => model // Will be handled by parent via SetSyntax
    | AddEdge(_, _, _) => model // Will be handled by parent via SetSyntax
    | RemoveEdge(_, _) => model // Will be handled by parent via SetSyntax
    | UpdateNodeLabel(_, _) => model // Will be handled by parent via SetSyntax
    | UpdateEdgeLabel(_, _, _) => model // Will be handled by parent via SetSyntax
    };

  let view =
      (
        model,
        info,
        ~local,
        ~parent: external_action => Ui_effect.t(unit),
        ~view_seg as _,
      ) => {
    let graph = SyntaxTerm.get(info);

    let canvas_width = 400;
    let canvas_height = 300;
    let node_radius = 20;

    // Helper function to clamp node positions within canvas bounds
    let clamp_position = ({x, y}: position): position => {
      let clamped_x =
        max(
          float_of_int(node_radius),
          min(float_of_int(canvas_width - node_radius), x),
        );
      let clamped_y =
        max(
          float_of_int(node_radius),
          min(float_of_int(canvas_height - node_radius), y),
        );
      {
        x: clamped_x,
        y: clamped_y,
      };
    };

    // Helper functions for graph manipulation
    let create_unique_id = (graph: graph): string => {
      let rec try_id = (n: int): string => {
        let id = "node" ++ string_of_int(n);
        if (List.exists(node => node.id == id, graph.nodes)) {
          try_id(n + 1);
        } else {
          id;
        };
      };
      try_id(1);
    };

    let add_node = (graph: graph, position: position): graph => {
      let new_node = {
        id: create_unique_id(graph),
        label: "Node",
        position: clamp_position(position),
      };
      {
        ...graph,
        nodes: [new_node, ...graph.nodes],
      };
    };

    let update_node_label =
        (graph: graph, node_id: string, new_label: string): graph => {
      let updated_nodes =
        List.map(
          node =>
            node.id == node_id
              ? {
                ...node,
                label: new_label,
              }
              : node,
          graph.nodes,
        );
      {
        ...graph,
        nodes: updated_nodes,
      };
    };

    let add_edge = (graph: graph, from_id: string, to_id: string): graph =>
      if (from_id == to_id
          || List.exists(
               edge => edge.from_id == from_id && edge.to_id == to_id,
               graph.edges,
             )) {
        graph; // Don't add self-loops or duplicate edges
      } else {
        let new_edge = {
          from_id,
          to_id,
          label: "edge",
        };
        {
          ...graph,
          edges: [new_edge, ...graph.edges],
        };
      };

    // Normalize all nodes to be within canvas bounds on render
    let normalized_graph = {
      ...graph,
      nodes:
        List.map(
          node =>
            {
              ...node,
              position: clamp_position(node.position),
            },
          graph.nodes,
        ),
    };

    let render_edge = ({from_id, to_id, label}: edge): Node.t => {
      switch (
        List.find_opt(n => n.id == from_id, normalized_graph.nodes),
        List.find_opt(n => n.id == to_id, normalized_graph.nodes),
      ) {
      | (Some(from_node), Some(to_node)) =>
        let mid_x = (from_node.position.x +. to_node.position.x) /. 2.0;
        let mid_y = (from_node.position.y +. to_node.position.y) /. 2.0;

        Node.create_svg(
          "g",
          ~attrs=[],
          [
            Node.create_svg(
              "line",
              ~attrs=[
                Attr.create("x1", Float.to_string(from_node.position.x)),
                Attr.create("y1", Float.to_string(from_node.position.y)),
                Attr.create("x2", Float.to_string(to_node.position.x)),
                Attr.create("y2", Float.to_string(to_node.position.y)),
                Attr.create("stroke", "#666"),
                Attr.create("stroke-width", "2"),
              ],
              [],
            ),
            Node.create_svg(
              "text",
              ~attrs=[
                Attr.create("x", Float.to_string(mid_x)),
                Attr.create("y", Float.to_string(mid_y -. 5.0)),
                Attr.create("text-anchor", "middle"),
                Attr.create("font-size", "10"),
                Attr.create("fill", "#444"),
                Attr.create("pointer-events", "none"),
                Attr.create("stroke", "white"),
                Attr.create("stroke-width", "3"),
                Attr.create("paint-order", "stroke fill"),
              ],
              [Node.text(label)],
            ),
          ],
        );
      | _ => Node.text("") // Invalid edge, skip
      };
    };

    let render_node = (node: node): Node.t => {
      let is_selected = model.selected_node == Some(node.id);
      let is_dragging = model.dragging == Some(node.id);
      let is_edge_source = model.creating_edge_from == Some(node.id);
      let is_editing = model.editing_node == Some(node.id);

      let on_mousedown = evt => {
        let coerced_evt = Js_of_ocaml.Js.Unsafe.coerce(evt);
        // For mousedown, we need to find the SVG by traversing up from the clicked element
        let rec find_svg = element =>
          if (element##.tagName == "svg") {
            element;
          } else {
            find_svg(element##.parentElement);
          };
        let svg_element = find_svg(coerced_evt##.target);
        let svg_rect = svg_element##getBoundingClientRect();

        let mouse_x = float_of_int(coerced_evt##.clientX) -. svg_rect##.left;
        let mouse_y = float_of_int(coerced_evt##.clientY) -. svg_rect##.top;

        // Store the offset between mouse and node center
        let offset_x = mouse_x -. node.position.x;
        let offset_y = mouse_y -. node.position.y;

        // Check for double-click to start editing
        if (coerced_evt##.detail == 2) {
          local(StartEditingNode(node.id));
        } else if (Js_of_ocaml.Js.to_bool(coerced_evt##.shiftKey)) {
          // Check for shift key - if pressed, handle edge creation
          switch (model.creating_edge_from) {
          | Some(from_id) when from_id != node.id =>
            // Complete edge creation
            let updated_graph = add_edge(graph, from_id, node.id);
            switch (SyntaxTerm.put(info, updated_graph)) {
            | Some(new_syntax) => parent(SetSyntax(new_syntax))
            | None => Effect.Ignore
            };
          | _ =>
            // Start edge creation
            local(StartEdgeCreation(node.id))
          };
        } else {
          // Normal drag behavior
          local(
            StartDrag(
              node.id,
              {
                x: offset_x,
                y: offset_y,
              },
            ),
          );
        };
      };

      Node.create_svg(
        "g",
        ~attrs=[],
        [
          Node.create_svg(
            "circle",
            ~attrs=[
              Attr.create("cx", Float.to_string(node.position.x)),
              Attr.create("cy", Float.to_string(node.position.y)),
              Attr.create("r", Int.to_string(node_radius)),
              Attr.create(
                "fill",
                is_editing
                  ? "#FFD700"
                  : is_edge_source
                      ? "#FFA500" : is_selected ? "#4A90E2" : "#E8E8E8",
              ),
              Attr.create(
                "stroke",
                is_dragging ? "#FF6B6B" : is_edge_source ? "#FF8C00" : "#666",
              ),
              Attr.create(
                "stroke-width",
                is_dragging ? "3" : is_edge_source ? "3" : "2",
              ),
              Attr.create("cursor", "pointer"),
              Attr.on_mousedown(on_mousedown),
            ],
            [],
          ),
          is_editing
            ? Node.create_svg(
                "foreignObject",
                ~attrs=[
                  Attr.create("x", Float.to_string(node.position.x -. 40.0)),
                  Attr.create("y", Float.to_string(node.position.y -. 10.0)),
                  Attr.create("width", "80"),
                  Attr.create("height", "20"),
                ],
                [
                  Node.input(
                    ~attrs=[
                      Attr.create("type", "text"),
                      Attr.create("value", node.label),
                      Attr.create(
                        "style",
                        "width: 100%; text-align: center; font-size: 12px; border: 1px solid #666; border-radius: 3px;",
                      ),
                      Attr.on_blur(_ =>
                        local(FinishEditingNode(node.id, node.label))
                      ),
                      Attr.on_keydown(evt => {
                        let key_evt = Js_of_ocaml.Js.Unsafe.coerce(evt);
                        if (key_evt##.key == "Enter") {
                          let target = key_evt##.target;
                          let new_label =
                            Js_of_ocaml.Js.to_string(target##.value);
                          let updated_graph =
                            update_node_label(graph, node.id, new_label);
                          switch (SyntaxTerm.put(info, updated_graph)) {
                          | Some(new_syntax) =>
                            let _ = parent(SetSyntax(new_syntax));
                            local(FinishEditingNode(node.id, new_label));
                          | None => Effect.Ignore
                          };
                        } else if (key_evt##.key == "Escape") {
                          local(FinishEditingNode(node.id, node.label));
                        } else {
                          Effect.Ignore;
                        };
                      }),
                    ],
                    (),
                  ),
                ],
              )
            : Node.create_svg(
                "text",
                ~attrs=[
                  Attr.create("x", Float.to_string(node.position.x)),
                  Attr.create("y", Float.to_string(node.position.y +. 5.0)),
                  Attr.create("text-anchor", "middle"),
                  Attr.create("font-size", "12"),
                  Attr.create("fill", "#333"),
                  Attr.create("pointer-events", "none") // Don't interfere with circle mouse events
                ],
                [Node.text(node.label)],
              ),
        ],
      );
    };

    let on_mousemove = evt => {
      switch (model.dragging) {
      | Some(node_id) =>
        let coerced_evt = Js_of_ocaml.Js.Unsafe.coerce(evt);
        // Use currentTarget which should be the SVG element where we registered the event
        let svg_element = coerced_evt##.currentTarget;
        let svg_rect = svg_element##getBoundingClientRect();

        let mouse_x = float_of_int(coerced_evt##.clientX) -. svg_rect##.left;
        let mouse_y = float_of_int(coerced_evt##.clientY) -. svg_rect##.top;

        // Calculate new position and apply bounds checking
        let raw_position = {
          x: mouse_x -. model.drag_offset.x,
          y: mouse_y -. model.drag_offset.y,
        };
        let new_position = clamp_position(raw_position);

        // Update the graph with new node position
        let updated_nodes =
          List.map(
            node =>
              node.id == node_id
                ? {
                  ...node,
                  position: new_position,
                }
                : node,
            graph.nodes,
          );
        let updated_graph = {
          ...graph,
          nodes: updated_nodes,
        };

        switch (SyntaxTerm.put(info, updated_graph)) {
        | Some(new_syntax) => parent(SetSyntax(new_syntax))
        | None => Effect.Ignore
        };
      | None => Effect.Ignore
      };
    };

    let on_mouseup = _evt => {
      switch (model.dragging) {
      | Some(_) => local(EndDrag)
      | None => Effect.Ignore
      };
    };

    let on_canvas_click = evt => {
      let coerced_evt = Js_of_ocaml.Js.Unsafe.coerce(evt);
      // Check if it's a double-click and if clicking the canvas (not on existing nodes)
      if (coerced_evt##.detail == 2 && coerced_evt##.target##.tagName == "svg") {
        let svg_element = coerced_evt##.target;
        let svg_rect = svg_element##getBoundingClientRect();

        let mouse_x = float_of_int(coerced_evt##.clientX) -. svg_rect##.left;
        let mouse_y = float_of_int(coerced_evt##.clientY) -. svg_rect##.top;

        let new_position =
          clamp_position({
            x: mouse_x,
            y: mouse_y,
          });
        let updated_graph = add_node(graph, new_position);

        switch (SyntaxTerm.put(info, updated_graph)) {
        | Some(new_syntax) => parent(SetSyntax(new_syntax))
        | None => Effect.Ignore
        };
      } else if
        // Cancel edge creation on single click of canvas
        (coerced_evt##.target##.tagName == "svg") {
        local(CancelEdgeCreation);
      } else {
        Effect.Ignore;
      };
    };

    let edges = List.map(render_edge, normalized_graph.edges);
    let nodes = List.map(render_node, normalized_graph.nodes);

    // Auto-focus the input when editing starts
    let () =
      switch (model.editing_node) {
      | Some(_) =>
        // Schedule focus for next frame using setTimeout
        ignore(
          Js_of_ocaml.Dom_html.window##setTimeout(
            Js_of_ocaml.Js.wrap_callback(() => {
              let input_selector = "input[type='text']";
              let document = Js_of_ocaml.Dom_html.document;
              let input_opt =
                document##querySelector(
                  Js_of_ocaml.Js.string(input_selector),
                );
              Js_of_ocaml.Js.Opt.iter(
                input_opt,
                input => {
                  let _ = Js_of_ocaml.Js.Unsafe.coerce(input)##focus();
                  Js_of_ocaml.Js.Unsafe.coerce(input)##select();
                },
              );
            }),
            0.0,
          ),
        )
      | None => ()
      };

    View.mk(
      Node.div(
        ~attrs=[
          Attr.id(Id.cls(info.id)),
          Attr.classes(["graph-projector"]),
          Attr.create("style", "user-select: none;"),
        ],
        [
          Node.create_svg(
            "svg",
            ~attrs=[
              Attr.create("width", Int.to_string(canvas_width)),
              Attr.create("height", Int.to_string(canvas_height)),
              Attr.create(
                "style",
                {|border: 1px solid #ccc;
    background: #f9f9f9;
    border-radius: 0.5em;
    left: 10px;
    top: 10px;|},
              ),
              Attr.on_mousemove(on_mousemove),
              Attr.on_mouseup(on_mouseup),
              Attr.on_click(on_canvas_click),
            ],
            edges @ nodes,
          ),
        ],
      ),
    );
  };
};
