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
  /* Where one of Hazel's passes began, and which one: the metaTime of each
     put into the marker cell, paired with the name it wrote. Ascending, so a
     row's pass is the last boundary at or before its own metaTime.

     Positional rather than carried in the ids, because it cannot be carried:
     `root_node()` answers a constant, and a node's time is part of its
     identity, so a pass with its own time would be a pass with its own copy
     of the store. See the note in FumolaRun. */
  passes: list((int, string)),
};

let empty: t = {
  events: [],
  nodes: [],
  edges: [],
  passes: [],
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
            /* Sourced at the editor, or aimed at the marker cell. The
               first catches every edge the editor causes; the second is
               belt and braces for the marker, whose whole point is to be
               the editor's. */
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

/* Where each of Hazel's moments begins, as (metaTime, label), ascending.

   Read off the TIME of a node id rather than from anything written into the
   store. Every run happens at `hazel(n), so a node's time IS the moment that
   made it, and the pass that moment belonged to is looked up in FumolaRun.
   Nothing is put anywhere to record this -- there is no marker, and no cell
   that exists only to be read by the panel. */
let moment_of = (node_id: Yojson.Safe.t): option(int) => {
  let at = (name, json) =>
    switch (json) {
    | `Assoc(obj) => List.assoc_opt(name, obj)
    | _ => None
    };
  let text = json =>
    switch (at("value", json)) {
    | Some(`String(s)) => Some(s)
    | _ => None
    };
  switch (FumolaEvents.tagged(node_id)) {
  | Some(("Tuple", `List([_space, time, ..._]))) =>
    switch (FumolaEvents.tagged(time)) {
    | Some(("Variant", v)) =>
      switch (at("value", v)) {
      | Some(sym) =>
        switch (FumolaEvents.tagged(sym)) {
        | Some(("Symbol", call)) =>
          switch (at("tag", call), at("fun", call), at("arg", call)) {
          | (Some(`String("Call")), Some(f), Some(arg)) =>
            switch (text(f), text(arg)) {
            | (Some("hazel"), Some(n)) => int_of_string_opt(n)
            | _ => None
            }
          | _ => None
          }
        | _ => None
        }
      | None => None
      }
    | _ => None
    }
  | _ => None
  };
};

let label_of_moment = (n: int): string =>
  switch (FumolaRun.pass_of_moment(n)) {
  | Some(pass) => pass ++ " · moment " ++ string_of_int(n)
  | None => "moment " ++ string_of_int(n)
  };

let pass_boundaries = (edges: Yojson.Safe.t): list((int, string)) => {
  let inner_of = fields =>
    switch (FumolaEvents.field("edge", fields)) {
    | Some(e) =>
      switch (FumolaEvents.tagged(e)) {
      | Some(("Record", f)) => Some(f)
      | _ => None
      }
    | None => None
    };
  let began_at = f =>
    switch (FumolaEvents.field("metaTimes", f)) {
    | Some(pair) =>
      switch (FumolaEvents.tagged(pair)) {
      | Some(("Tuple", `List([a, ..._]))) =>
        int_of_string_opt(FumolaEvents.summarize(a))
      | _ => None
      }
    | None => None
    };
  switch (FumolaEvents.tagged(edges)) {
  | Some(("List", `List(items))) =>
    items
    |> List.filter_map(item =>
         switch (FumolaEvents.tagged(item)) {
         | Some(("Record", fields)) =>
           switch (inner_of(fields)) {
           | Some(f) =>
             switch (
               began_at(f),
               FumolaEvents.field("target", f) |> Option.map(moment_of),
             ) {
             | (Some(at), Some(Some(n))) => Some((at, label_of_moment(n)))
             | _ => None
             }
           | None => None
           }
         | _ => None
         }
       )
    |> List.sort(((a, _), (b, _)) => compare(a, b))
  | _ => []
  };
};

/* The pass a row belongs to: the last boundary at or before its metaTime.

   None for a row older than the first marker, which is every row an instance
   recorded before this Hazel build, and every row a program put there
   itself. */
let pass_at =
    (passes: list((int, string)), meta_time: string): option(string) =>
  switch (int_of_string_opt(meta_time)) {
  | None => None
  | Some(m) =>
    List.fold_left(
      (acc, (at, name)) => at <= m ? Some(name) : acc,
      None,
      passes,
    )
  };

/* One fetch, three lists. The error cases are FumolaEvents.of_instance's, for
   the same reasons: a missing runtime is an error because a panel showing
   nothing would otherwise read as "this program did nothing", and an instance
   that has never run is not. */
/* An edge's status field was spelled `align` until Adapton/fumola#133 and is
   spelled `status` after it, and Hazel pins no runtime version -- it reads
   whatever fumola.org is serving. So both spellings arrive in practice, and
   for as long as they do the reader accepts either by renaming the older key
   on the way in.

   A blanket rename over the history is safe because only an Edge carries this
   field: a node row has body / result / space / trace, and an event carries
   its own shape. Nothing else in this JSON is called `align`.

   Removable once the published runtime has moved and stayed moved. */
let rec accept_either_status = (json: Yojson.Safe.t): Yojson.Safe.t =>
  switch (json) {
  | `Assoc(fields) =>
    `Assoc(
      List.map(
        ((key, v)) =>
          (key == "align" ? "status" : key, accept_either_status(v)),
        fields,
      ),
    )
  | `List(items) => `List(List.map(accept_either_status, items))
  | other => other
  };

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
          let history = accept_either_status(history);
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
            passes:
              switch (list_at("edges")) {
              | Some(edges) => pass_boundaries(edges)
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
