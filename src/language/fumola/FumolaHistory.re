/* The whole of what an instance remembers: its events, its nodes and its
   edges, in one fetch.

   `prim "adaptonPeekHistory" ()` answers all three; FumolaEvents has been
   reading the first and discarding the other two. The two it discarded are
   what the Nodes and Edges views of the panel show.

   Rows are translated rather than parsed. A node row and an edge row are
   ordinary Fumola values, and FumolaValue already turns those into Hazel
   values given the type to expect -- which FumolaAdapton supplies. So what
   the panel renders is a Hazel value of a declared Hazel type, the same thing
   the `Node info` slide gets from `peekInfo`, and it can be shown with the
   same code view rather than with strings assembled here.

   What IS read out of the json, and only that, is the handful of keys the
   panel navigates by: which moment a row belongs to, and which space or edge
   it is about. Those are keys, not content. */

/* A row: the keys it is filed under, and the value to show. */
type node_row = {
  /* The metaTime this revision was born at. */
  meta_time: string,
  /* The node's space, as the symbol that names it -- the identity a node
     keeps across revisions, and what an event's pointer is matched against. */
  space: string,
  /* The edges this revision's last run left behind, if it is a thunk. They
     are inside the rendered value too, but code is not clickable, so they
     are read out here to be offered as ways on. */
  trace: list(string),
  /* Whether this node is the editor's own: its space is Here. The editor
     used to be a single node at a single time, which no list of nodes would
     bother showing; with a time of its own per moment it has revisions like
     any other node, and a reader wants the same Show / Dim / Hide over them
     as over its edges. */
  editor: bool,
  value: TermBase.Exp.t,
};

type edge_row = {
  edge_id: string,
  source: string,
  target: string,
  /* Whether the source is the editor's: a node whose space is Here. The
     editor acts at more than one time, so the time is not part of the
     test -- (Here, Now, _) and (Here, t, _) are both the editor, and a
     panel that read the whole triple would call the second one program
     traffic. Kept as a fact about the row rather than filtered away,
     because dimming needs the row to still be here. */
  editor: bool,
  /* The pair an edge spans; both are moments a reader may want to open. */
  meta_times: (string, string),
  value: TermBase.Exp.t,
};

type t = {
  events: list(FumolaEvents.event),
  nodes: list(node_row),
  edges: list(edge_row),
};

let empty: t = {
  events: [],
  nodes: [],
  edges: [],
};

/* Whether a node id is the editor's: its space is Here.

   Space only. A node id is a triple of space, time and a counter, and the
   editor is spread across times rather than confined to Now, so the time
   says which of the editor's moments this was and not whether it was the
   editor at all.

   The variant's name, not the rendered key: FumolaValue renders a Name as
   the bare string it carries, and the runtime's own symbol for the editor is
   spelled `@here`, which nothing reserves. A program free to name a cell
   `@here` would otherwise have its edges read as the editor's. */
let is_editor = (node_id: Yojson.Safe.t): bool =>
  switch (FumolaEvents.tagged(node_id)) {
  | Some(("Tuple", `List([space, ..._]))) =>
    switch (FumolaEvents.tagged(space)) {
    | Some(("Variant", v)) =>
      FumolaEvents.field("name", v) == Some(`String("Here"))
    | _ => false
    }
  | _ => false
  };

let meta_times_of = (edge: Yojson.Safe.t): (string, string) =>
  switch (FumolaEvents.field("metaTimes", edge)) {
  | Some(pair) =>
    switch (FumolaEvents.tagged(pair)) {
    | Some(("Tuple", `List([a, b]))) => (
        FumolaEvents.summarize(a),
        FumolaEvents.summarize(b),
      )
    | _ => ("", "")
    }
  | None => ("", "")
  };

/* Each row is translated on its own, so one unshowable row costs its own row
   and not the list. */
let rows =
    (
      ~instance_id: int,
      ~ana: TermBase.Typ.t,
      ~key: (Yojson.Safe.t, TermBase.Exp.t) => option('a),
      json: Yojson.Safe.t,
    )
    : list('a) =>
  switch (FumolaEvents.tagged(json)) {
  | Some(("List", `List(items))) =>
    List.filter_map(
      item =>
        switch (
          FumolaValue.exp_of_json(
            ~instance_id,
            ~eval=FumolaRun.eval_in(instance_id),
            ~ana,
            ~tools=FumolaAdapton.tools,
            item,
          )
        ) {
        | Ok(value) => key(item, value)
        | Error(_) => None
        },
      items,
    )
  | _ => []
  };

/* A thunk node's trace: the edges its last run recorded. A non-thunk has
   none, and neither does a thunk that has not run. */
let trace_of = (node: option(Yojson.Safe.t)): list(string) =>
  switch (node) {
  | Some(node) =>
    switch (FumolaEvents.tagged(node)) {
    | Some(("Variant", v)) =>
      switch (FumolaEvents.field("value", v)) {
      | Some(payload) =>
        switch (FumolaEvents.tagged(payload)) {
        | Some(("Record", fields)) =>
          switch (FumolaEvents.field("trace", fields)) {
          | Some(trace) =>
            switch (FumolaEvents.tagged(trace)) {
            | Some(("List", `List(items))) =>
              List.map(FumolaEvents.summarize, items)
            | _ => []
            }
          | None => []
          }
        | _ => []
        }
      | None => []
      }
    | _ => []
    }
  | None => []
  };

let node_rows = (~instance_id: int, json: Yojson.Safe.t): list(node_row) =>
  rows(
    ~instance_id,
    ~ana=FumolaAdapton.node_row(),
    ~key=
      (item, value) =>
        switch (FumolaEvents.tagged(item)) {
        | Some(("Record", fields)) =>
          let meta_time =
            switch (FumolaEvents.field("metaTime", fields)) {
            | Some(t) => FumolaEvents.summarize(t)
            | None => ""
            };
          let space =
            switch (FumolaEvents.field("nodeId", fields)) {
            | Some(id) => FumolaEvents.space_key(id)
            | None => ""
            };
          Some({
            meta_time,
            space,
            trace: trace_of(FumolaEvents.field("node", fields)),
            editor:
              switch (FumolaEvents.field("nodeId", fields)) {
              | Some(id) => is_editor(id)
              | None => false
              },
            value,
          });
        | _ => None
        },
    json,
  );

let edge_rows = (~instance_id: int, json: Yojson.Safe.t): list(edge_row) =>
  rows(
    ~instance_id,
    ~ana=FumolaAdapton.edge_row(),
    ~key=
      (item, value) =>
        switch (FumolaEvents.tagged(item)) {
        | Some(("Record", fields)) =>
          let edge_id =
            switch (FumolaEvents.field("edgeId", fields)) {
            | Some(id) => FumolaEvents.summarize(id)
            | None => ""
            };
          let edge = FumolaEvents.field("edge", fields);
          let inner =
            switch (edge) {
            | Some(e) =>
              switch (FumolaEvents.tagged(e)) {
              | Some(("Record", f)) => Some(f)
              | _ => None
              }
            | None => None
            };
          let at = name =>
            switch (inner) {
            | Some(f) =>
              switch (FumolaEvents.field(name, f)) {
              | Some(id) => FumolaEvents.space_key(id)
              | None => ""
              }
            | None => ""
            };
          Some({
            edge_id,
            source: at("source"),
            target: at("target"),
            editor:
              switch (inner) {
              | Some(f) =>
                switch (FumolaEvents.field("source", f)) {
                | Some(id) => is_editor(id)
                | None => false
                }
              | None => false
              },
            meta_times:
              switch (inner) {
              | Some(f) => meta_times_of(f)
              | None => ("", "")
              },
            value,
          });
        | _ => None
        },
    json,
  );

/* One fetch, three lists. The error cases are FumolaEvents.of_instance's, for
   the same reasons: a missing runtime is an error because a panel showing
   nothing would otherwise read as "this program did nothing", and an instance
   that has never run is not. */
let of_instance = (name: string): result(t, string) =>
  switch (FumolaRun.instance_of_name(name)) {
  | exception FumolaRun.No_runtime =>
    Error("the Fumola runtime is not loaded")
  | instance_id =>
    switch (FumolaRun.eval_in(instance_id, "prim \"adaptonPeekHistory\" ()")) {
    | `Null => Error("the Fumola runtime is not loaded")
    | `Assoc(obj) as json =>
      switch (List.assoc_opt("ok", obj)) {
      | Some(`Bool(true)) =>
        switch (FumolaEvents.field("value", json)) {
        | Some(history) =>
          let list_at = name =>
            switch (FumolaEvents.field(name, history)) {
            | Some(list) => Some(list)
            /* A simple instance keeps no graph, so its history has no such
               field rather than an empty one. */
            | None => None
            };
          Ok({
            events:
              switch (list_at("events")) {
              | Some(events) => FumolaEvents.of_json(events)
              | None => []
              },
            nodes:
              switch (list_at("nodes")) {
              | Some(nodes) => node_rows(~instance_id, nodes)
              | None => []
              },
            edges:
              switch (list_at("edges")) {
              | Some(edges) => edge_rows(~instance_id, edges)
              | None => []
              },
          });
        | None => Ok(empty)
        }
      | _ =>
        Error(
          switch (List.assoc_opt("error", obj)) {
          | Some(`String(message)) => message
          | _ => "the instance would not report its history"
          },
        )
      }
    | _ => Error("could not read the Fumola runtime's response")
    }
  };
