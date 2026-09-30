/* The outline of an instance's runs, drawn as HTML: a port of `drawOutline`
   from Fumola's web player (pages/web-play/index.html), which is itself a
   port of the replayground's `renderOutline`. It reads the Outline value
   tree as it crosses the wasm boundary -- `#internal` and `#leaf` nodes,
   records of edges -- and draws the same nesting, the same arrows and the
   same `(begin, end)` pairs as Adapton.IntoText's text format.

   One departure: a `get` leads to a `#leaf`, which the web player draws as
   an empty nested node with a result row beneath it. Here a leaf's value
   sits on the edge's own line, as the text format has it. */
open Virtual_dom.Vdom;

let field = (name: string, obj) => List.assoc_opt(name, obj);

/* The {tag, value} a runtime value crosses as. */
let tagged = (v: Yojson.Safe.t): (string, Yojson.Safe.t) =>
  switch (v) {
  | `Assoc(obj) => (
      switch (field("tag", obj)) {
      | Some(`String(t)) => t
      | _ => ""
      },
      Option.value(field("value", obj), ~default=`Null),
    )
  | _ => ("", `Null)
  };

let record = (v: Yojson.Safe.t) =>
  switch (tagged(v)) {
  | ("Record", `Assoc(fields)) => fields
  | _ => []
  };

let items = (v: Yojson.Safe.t) =>
  switch (tagged(v)) {
  | ("List" | "Tuple", `List(xs)) => xs
  | _ => []
  };

/* A variant's name and payload. */
let variant = (v: Yojson.Safe.t): option((string, Yojson.Safe.t)) =>
  switch (tagged(v)) {
  | ("Variant", `Assoc(obj)) =>
    switch (field("name", obj)) {
    | Some(`String(name)) =>
      Some((name, Option.value(field("value", obj), ~default=`Null)))
    | _ => None
    }
  | _ => None
  };

/* A value on one line, in Fumola's spelling: `?(19, 21)`, `#tag`, a symbol
   as its text. The text format prints with debug_show, and this follows it
   closely enough to read the same. */
let rec show = (v: Yojson.Safe.t): string =>
  switch (tagged(v)) {
  | ("Int" | "Nat" | "Float", `String(n)) => n
  | ("Bool", `Bool(b)) => b ? "true" : "false"
  | ("Text" | "String", `String(s)) => "\"" ++ s ++ "\""
  | ("Unit", _) => "()"
  | ("Null", _) => "null"
  | ("Option", payload) => "?" ++ show(payload)
  | ("Tuple", `List(xs)) =>
    "(" ++ String.concat(", ", List.map(show, xs)) ++ ")"
  | ("List", `List(xs)) =>
    "[" ++ String.concat(", ", List.map(show, xs)) ++ "]"
  | ("Record", `Assoc(fields)) =>
    "{"
    ++ String.concat(
         "; ",
         List.map(((k, x)) => k ++ " = " ++ show(x), fields),
       )
    ++ "}"
  | ("Symbol", symbol) =>
    switch (FumolaValue.symbol_text(symbol)) {
    | Ok(text) => text
    | Error(_) => "<symbol>"
    }
  | ("AdaptonPointer", `Assoc(fields)) =>
    switch (field("symbol", fields)) {
    | Some(symbol) =>
      switch (FumolaValue.symbol_text(symbol)) {
      | Ok(text) => text
      | Error(_) => "<pointer>"
      }
    | None => "<pointer>"
    }
  | ("Variant", _) =>
    switch (variant(v)) {
    /* A space's #Symbol wrapper says nothing a reader needs. */
    | Some(("Symbol", payload)) => show(payload)
    | Some((name, `Null)) => "#" ++ name
    | Some((name, payload)) => "#" ++ name ++ "(" ++ show(payload) ++ ")"
    | None => "#?"
    }
  | ("Opaque", `String(shows)) => shows
  | (tag, _) => "<" ++ tag ++ ">"
  };

/* A node id is (name, time, serial): the name is what to show, and the rest
   goes in the title, for a reader who wants to tell revisions apart. */
let node_name = (id: Yojson.Safe.t): (string, string) =>
  switch (items(id)) {
  | [name, ...rest] => (
      show(name),
      String.concat(", ", List.map(show, [name, ...rest])),
    )
  | [] => (show(id), show(id))
  };

let arrow =
  fun
  | "put" => " \u{2500}\u{2500}\u{2500}\u{bb} "
  | "get" => " \u{2500}\u{2500}\u{2500}> "
  | "putForce" => " \u{2550}\u{2550}\u{25b7}\u{bb} "
  | "force_" => " \u{2550}\u{2550}\u{2550}\u{25b7} "
  | other => " " ++ other ++ " ";

let span = (cls, text) =>
  Node.span(~attrs=[Attr.classes(cls)], [Node.text(text)]);

/* `(t)` when an edge happened at one moment, `(t0, t1)` when a nested one
   spans two, as IntoText.metaTimesIntoText writes it. */
let meta_times = (v: Yojson.Safe.t): string =>
  switch (items(v)) {
  | [a, b] when show(a) == show(b) => "(" ++ show(a) ++ ")"
  | [a, b] => "(" ++ show(a) ++ ", " ++ show(b) ++ ")"
  | _ => "(" ++ show(v) ++ ")"
  };

let result_row = (value: Yojson.Safe.t): Node.t =>
  Node.div(
    ~attrs=[Attr.class_("outline-internal-result")],
    [
      span(
        ["outline-arrow", "outline-result-arrow"],
        "\u{2570}\u{301c}\u{301c}> ",
      ),
      span(["outline-value-text"], show(value)),
    ],
  );

let rec node = (~root: bool, outline: Yojson.Safe.t): Node.t =>
  switch (variant(outline)) {
  | Some(("internal", fields)) =>
    let fields = record(fields);
    let head =
      root
        ? switch (field("__nodeId", fields)) {
          | Some(id) =>
            let (name, full) = node_name(id);
            [
              Node.span(
                ~attrs=[Attr.class_("outline-node-id"), Attr.title(full)],
                [Node.text(name)],
              ),
            ];
          | None => []
          }
        : [];
    let edges =
      List.concat_map(
        edge_rows,
        Option.fold(~none=[], ~some=items, field("edges", fields)),
      );
    /* The root's own result, which the text format ends every tree with;
       a nested node's result is drawn by the edge that forced it. */
    let result =
      root
        ? [
          result_row(Option.value(field("value", fields), ~default=`Null)),
        ]
        : [];
    Node.div(~attrs=[Attr.class_("outline-node")], head @ edges @ result);
  | Some(("leaf", fields)) =>
    let fields = record(fields);
    Node.div(
      ~attrs=[Attr.class_("outline-node")],
      [
        span(
          ["outline-value-text"],
          Option.fold(~none="", ~some=show, field("value", fields)),
        ),
      ],
    );
  | _ => Node.none
  }

and edge_rows = (edge: Yojson.Safe.t): list(Node.t) => {
  let e = record(edge);
  let get = name => Option.value(field(name, e), ~default=`Null);
  let action =
    switch (variant(get("action"))) {
    | Some((name, _)) => name
    | None => ""
    };
  let (target, target_full) =
    switch (tagged(get("actionTarget"))) {
    | ("Tuple", _) => node_name(get("actionTarget"))
    | _ => (show(get("actionTarget")), show(get("actionTarget")))
    };
  let row = rest =>
    Node.div(
      ~attrs=[Attr.class_("outline-edge")],
      [
        span(["outline-meta-time"], meta_times(get("metaTimes"))),
        span(["outline-arrow", "outline-action-" ++ action], arrow(action)),
        Node.span(
          ~attrs=[Attr.class_("outline-node-id"), Attr.title(target_full)],
          [Node.text(target)],
        ),
        ...rest,
      ],
    );
  /* A nested force draws its own tree, then the value it came back with on
     a line of its own; anything else carries its value on its line. */
  let nested =
    switch (tagged(get("outline"))) {
    | ("Option", inner) =>
      switch (variant(inner)) {
      | Some(("internal", _)) => Some(inner)
      | _ => None
      }
    | _ => None
    };
  switch (nested) {
  | Some(inner) => [
      row([]),
      node(~root=false, inner),
      result_row(get("value")),
    ]
  | None => [
      row([span(["outline-value-text"], " " ++ show(get("value")))]),
    ]
  };
};

/* A forest: one tree per top-level force, with a rule between them so that
   several read as several, as the web player marks them. */
let forest = (outlines: list(Yojson.Safe.t)): Node.t => {
  let n = List.length(outlines);
  Node.div(
    ~attrs=[Attr.class_("fumola-outline")],
    List.concat(
      List.mapi(
        (i, o) =>
          (
            n > 1
              ? [
                span(
                  ["outline-forest-rule"],
                  Printf.sprintf("tree %d of %d", i + 1, n),
                ),
              ]
              : []
          )
          @ [node(~root=true, o)],
        outlines,
      ),
    ),
  );
};
