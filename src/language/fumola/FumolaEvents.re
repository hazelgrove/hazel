/* The adapton event list of a Fumola instance.

   Fetched by running `prim "adaptonPeekHistory" ()` in the instance, through
   the same shim everything else here uses. `peek` rather than `get`: reading
   the history to show it must not itself be recorded as a read.

   No wasm export was added for this. The history is already reachable as a
   prim, so the boundary stays source text and a change to what the panel
   shows costs a Hazel rebuild rather than a Rust one. */

/* A rendered event is mostly boilerplate -- "node added", "edge added" --
   with one part that actually distinguishes it: the symbol the node or cell
   was named with. The panel bolds that part, so the rendering keeps it apart
   from the rest rather than flattening everything into one string. */
type span =
  | Plain(string)
  | Sym(string);

/* The events, as the panel wants them: when, what, and to what. */
type event = (string, string, list(span));

/* Fumola renders a value as tagged JSON; these read the bits this needs and
   say nothing about the rest. */
let field = (name: string, json: Yojson.Safe.t): option(Yojson.Safe.t) =>
  switch (json) {
  | `Assoc(obj) => List.assoc_opt(name, obj)
  | _ => None
  };

let tagged = (json: Yojson.Safe.t): option((string, Yojson.Safe.t)) =>
  switch (field("tag", json), field("value", json)) {
  | (Some(`String(tag)), Some(value)) => Some((tag, value))
  | _ => None
  };

/* The text of a symbol, which is the name the store itself knows the cell by.

   Deferred to FumolaValue.symbol_text rather than spelled again here: that
   function is what turns a symbol into a NAME everywhere else, and its
   spelling is deliberately injective (see the note on BinOp there), so two
   distinct cells cannot come out reading alike in this list.

   QuotedAst is the one form it has no written form for. It carries its own
   source text, and showing that beats showing nothing. */
let symbol_text = (json: Yojson.Safe.t): option(string) =>
  switch (FumolaValue.symbol_text(json)) {
  | Ok(text) => Some(text)
  | Error(_) =>
    switch (field("tag", json), field("source", json)) {
    | (Some(`String("QuotedAst")), Some(`String(source))) => Some(source)
    | _ => None
    }
  };

/* A short, readable rendering of whatever an event points at: a node id, an
   edge id, a symbol. Enough to tell two events apart, which is what a list
   of them is for; the full structure belongs in a graph view, not here.

   A node id is (space, time, serial), and the space is where the symbol
   lives. Before this returned spans, a symbol whose form had no case here
   rendered as the empty string, so `Symbol(Num 1)` came out as the bare
   word "Symbol" and every node in the list looked the same -- which is the
   opposite of what the column is for. */
let rec spans = (json: Yojson.Safe.t): list(span) =>
  switch (tagged(json)) {
  | Some(("Int", `String(n))) => [Plain(n)]
  | Some(("Int", `Int(n))) => [Plain(string_of_int(n))]
  | Some(("Symbol", v)) =>
    switch (symbol_text(v)) {
    | Some(text) => [Sym(text)]
    | None => []
    }
  | Some(("Name", `String(x))) => [Sym("`" ++ x)]
  | Some(("Variant", v)) =>
    switch (field("name", v), field("value", v)) {
    | (Some(`String(name)), Some(`Null)) => [Plain(name)]
    | (Some(`String(name)), Some(payload)) =>
      switch (spans(payload)) {
      | [] => [Plain(name)]
      | inner => [Plain(name ++ " "), ...inner]
      }
    | _ => []
    }
  | Some(("Tuple", `List(parts))) =>
    parts |> List.map(spans) |> List.filter(p => p != []) |> join
  | Some(("List", `List(parts))) =>
    [Plain("[")]
    @ (parts |> List.map(spans) |> List.filter(p => p != []) |> comma)
    @ [Plain("]")]
  | Some(("Record", _)) => [Plain("\xE2\x80\xA6")]
  | Some(("Opaque", `String(s))) => [Plain(s)]
  | _ => []
  }
/* Both of these put a separator between groups and nothing at the ends,
   which List.concat alone will not do. */
and join = (groups: list(list(span))): list(span) => separate(" ", groups)
and comma = (groups: list(list(span))): list(span) =>
  separate(", ", groups)
and separate = (sep: string, groups: list(list(span))): list(span) =>
  switch (groups) {
  | [] => []
  | [first, ...rest] =>
    List.fold_left(
      (acc, group) => acc @ [Plain(sep), ...group],
      first,
      rest,
    )
  };

/* The same rendering with the distinction thrown away, for the places that
   want one string -- the meta-time column, which is never a symbol. */
let summarize = (json: Yojson.Safe.t): string =>
  spans(json)
  |> List.map(
       fun
       | Plain(s) => s
       | Sym(s) => s,
     )
  |> String.concat("");

let of_json = (json: Yojson.Safe.t): list(event) =>
  switch (tagged(json)) {
  | Some(("List", `List(items))) =>
    items
    |> List.filter_map(item =>
         switch (tagged(item)) {
         | Some(("Record", fields)) =>
           switch (field("event", fields), field("metaTime", fields)) {
           | (Some(event), Some(meta_time)) =>
             switch (tagged(event)) {
             | Some(("Variant", v)) =>
               switch (field("name", v)) {
               | Some(`String(name)) =>
                 Some((
                   summarize(meta_time),
                   name,
                   switch (field("value", v)) {
                   | Some(payload) => spans(payload)
                   | None => []
                   },
                 ))
               | _ => None
               }
             | _ => None
             }
           | _ => None
           }
         | _ => None
         }
       )
  | _ => []
  };

/* The events of the instance this name owns.

   An instance that has never run is not an error: claiming a name creates the
   runtime, and an empty list is the honest answer. A missing runtime is, and
   says so, because the panel showing nothing would otherwise read as "this
   program did nothing". */
let of_instance = (name: string): result(list(event), string) =>
  switch (FumolaRun.instance_of_name(name)) {
  | exception FumolaRun.No_runtime =>
    Error("the Fumola runtime is not loaded")
  | instance_id =>
    switch (FumolaRun.eval_in(instance_id, "prim \"adaptonPeekHistory\" ()")) {
    | `Null => Error("the Fumola runtime is not loaded")
    | `Assoc(obj) as json =>
      switch (List.assoc_opt("ok", obj)) {
      | Some(`Bool(true)) =>
        switch (field("value", json)) {
        | Some(history) =>
          switch (field("events", history)) {
          | Some(events) => Ok(of_json(events))
          /* A simple instance keeps no graph, so its history has no events
             field rather than an empty one. */
          | None => Ok([])
          }
        | None => Ok([])
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
