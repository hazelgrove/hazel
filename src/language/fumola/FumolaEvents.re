/* The adapton event list of a Fumola instance.

   Fetched by running `prim "adaptonPeekHistory" ()` in the instance, through
   the same shim everything else here uses. `peek` rather than `get`: reading
   the history to show it must not itself be recorded as a read.

   No wasm export was added for this. The history is already reachable as a
   prim, so the boundary stays source text and a change to what the panel
   shows costs a Hazel rebuild rather than a Rust one. */

/* The events, as the panel wants them: when, what, and to what. */
type event = (string, string, string);

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

/* A short, readable rendering of whatever an event points at: a node id, an
   edge id, a symbol. Enough to tell two events apart, which is what a list
   of them is for; the full structure belongs in a graph view, not here. */
let rec summarize = (json: Yojson.Safe.t): string =>
  switch (tagged(json)) {
  | Some(("Int", `String(n))) => n
  | Some(("Int", `Int(n))) => string_of_int(n)
  | Some(("Symbol", v)) => summarize(v)
  | Some(("Name", `String(x))) => "`" ++ x
  | Some(("Variant", v)) =>
    switch (field("name", v), field("value", v)) {
    | (Some(`String(name)), Some(`Null)) => name
    | (Some(`String(name)), Some(payload)) =>
      let inner = summarize(payload);
      inner == "" ? name : name ++ " " ++ inner;
    | _ => ""
    }
  | Some(("Tuple", `List(parts))) =>
    parts
    |> List.map(summarize)
    |> List.filter(s => s != "")
    |> String.concat(" ")
  | Some(("List", `List(parts))) =>
    "[" ++ (parts |> List.map(summarize) |> String.concat(", ")) ++ "]"
  | Some(("Record", _)) => "…"
  | Some(("Opaque", `String(s))) => s
  | _ => ""
  };

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
                   | Some(payload) => summarize(payload)
                   | None => ""
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
