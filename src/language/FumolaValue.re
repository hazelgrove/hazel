open Grammar;

/* Translating a Fumola result into a Hazel value.
 *
 * The Fumola runtime answers with JSON that preserves structure:
 *
 *   {"tag": "Int",    "value": "3"}
 *   {"tag": "Tuple",  "value": [<node>, ...]}
 *   {"tag": "Record", "value": {"<field>": <node>, ...}}
 *   {"tag": "Variant","value": {"name": "<name>", "value": <node> | null}}
 *
 * so that a Fumola tuple can be rebuilt here as a Hazel tuple, rather than as
 * a wrapper a Hazel program would have to take apart. This lives outside
 * Livelit.Fumola so that it can be tested directly: the livelit itself can
 * only be exercised in a browser with the wasm runtime present.
 *
 * A Fumola result has no single static type -- 1 + 2 is an Int, while
 * (get(1), get(2)) is a pair -- so the livelit's expansion type is Unknown and
 * the shape is whatever the program produced.
 */

/* Build the Hazel value denoted by one {tag, value} node. */
let rec exp_of_json = (json: Yojson.Safe.t): result(TermBase.Exp.t, string) => {
  let field = (name, obj) =>
    switch (List.assoc_opt(name, obj)) {
    | Some(v) => Ok(v)
    | None => Error("Fumola result is missing field `" ++ name ++ "`")
    };
  switch (json) {
  | `Assoc(obj) =>
    switch (field("tag", obj)) {
    | Error(e) => Error(e)
    | Ok(`String(tag)) =>
      switch (field("value", obj)) {
      | Error(e) => Error(e)
      | Ok(value) => exp_of_tagged(tag, value)
      }
    | Ok(_) => Error("Fumola result has a non-string tag")
    }
  | _ => Error("Fumola result is not an object")
  };
}

and exp_of_tagged =
    (tag: string, value: Yojson.Safe.t): result(TermBase.Exp.t, string) =>
  switch (tag, value) {
  | ("Int", `String(n)) =>
    switch (Bigint.of_string_opt(n)) {
    | Some(n) => Ok(DHExp.fresh(Atom(Int(n))))
    | None => Error("Fumola returned an unreadable integer: " ++ n)
    }
  | ("Bool", `Bool(b)) => Ok(DHExp.fresh(Atom(Bool(b))))
  | ("String", `String(str)) => Ok(DHExp.fresh(Atom(String(str))))
  /* Fumola's unit is Hazel's empty tuple. */
  | ("Unit", _) => Ok(DHExp.fresh(Tuple([])))
  | ("Tuple", `List(items)) =>
    switch (all(List.map(exp_of_json, items))) {
    | Error(e) => Error(e)
    | Ok(items) => Ok(DHExp.fresh(Tuple(items)))
    }
  /* A Hazel record is a tuple of labelled elements. Field order is fixed on
     the Fumola side, so it does not shift between evaluations. */
  | ("Record", `Assoc(fields)) =>
    let element = ((name, value)) =>
      switch (exp_of_json(value)) {
      | Error(e) => Error(e)
      | Ok(value) =>
        Ok(DHExp.fresh(TupLabel(DHExp.fresh(Label(name)), value)))
      };
    switch (all(List.map(element, fields))) {
    | Error(e) => Error(e)
    | Ok(elements) => Ok(DHExp.fresh(Tuple(elements)))
    };
  | ("Variant", `Assoc(fields)) =>
    let name =
      switch (List.assoc_opt("name", fields)) {
      | Some(`String(name)) => Ok(name)
      | _ => Error("Fumola variant has no name")
      };
    switch (name) {
    | Error(e) => Error(e)
    | Ok(name) =>
      let ctr = DHExp.fresh(Constructor(name, None));
      switch (List.assoc_opt("value", fields)) {
      | None
      | Some(`Null) => Ok(ctr)
      | Some(payload) =>
        switch (exp_of_json(payload)) {
        | Error(e) => Error(e)
        | Ok(payload) => Ok(DHExp.fresh(Ap(Forward, ctr, payload)))
        }
      };
    };
  | (tag, _) =>
    Error(
      "Fumola returned a " ++ tag ++ ", which has no Hazel translation yet",
    )
  }

and all = (results: list(result('a, string))): result(list('a), string) =>
  List.fold_right(
    (r, acc) =>
      switch (r, acc) {
      | (Error(e), _) => Error(e)
      | (_, Error(e)) => Error(e)
      | (Ok(x), Ok(xs)) => Ok([x, ...xs])
      },
    results,
    Ok([]),
  );

/* A short rendering of a result, for the widget itself. Derived from the
   same JSON as the expansion so the two cannot disagree. */
let rec describe = (json: Yojson.Safe.t): string =>
  switch (json) {
  | `Assoc(obj) =>
    let tag =
      switch (List.assoc_opt("tag", obj)) {
      | Some(`String(tag)) => tag
      | _ => ""
      };
    let value =
      switch (List.assoc_opt("value", obj)) {
      | Some(v) => v
      | None => `Null
      };
    switch (tag, value) {
    | ("Int", `String(n)) => n
    | ("Bool", `Bool(b)) => b ? "true" : "false"
    | ("String", `String(str)) => "\"" ++ str ++ "\""
    | ("Unit", _) => "()"
    | ("Tuple", `List(items)) =>
      "(" ++ String.concat(", ", List.map(describe, items)) ++ ")"
    | ("Record", `Assoc(fields)) =>
      "{"
      ++ String.concat(
           ", ",
           List.map(((k, v)) => k ++ " = " ++ describe(v), fields),
         )
      ++ "}"
    | ("Variant", `Assoc(fields)) =>
      let name =
        switch (List.assoc_opt("name", fields)) {
        | Some(`String(name)) => name
        | _ => "?"
        };
      switch (List.assoc_opt("value", fields)) {
      | None
      | Some(`Null) => "#" ++ name
      | Some(payload) => "#" ++ name ++ "(" ++ describe(payload) ++ ")"
      };
    | (tag, _) => "<" ++ tag ++ ">"
    };
  | _ => "?"
  };
