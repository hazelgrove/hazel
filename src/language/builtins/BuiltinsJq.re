module Fresh = IdTagged.FreshGrammar;
open Fresh.Typ;
open BuiltinsUtil;
open BuiltinsADT;

// NOTE[Matt]: All Hazel builtin functions MUST BE FIXPOINTS even if they are not recursive
// to ensure that the environments are handled correctly.

// jq-inspired JSON combinators.
// Every combinator has type JSON -> [JSON] (a "filter"), and pipe composes via flat_map.

let builtins: list(hazel_fn) = [
  {
    // ---- Tier 1: Basic Filters (JSON -> [JSON]) ----

    // identity: . — returns [json]

    name: "identity",
    str: {|fix identity -> fun json -> [json]|},
    arg: Typ.term_of(JSON.t),
    ret: List(JSON.t),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("identity"),
            fn(
              Pat.var("json"),
              list_lit([var("json")]),
              None,
              Some("identity+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // iterate: .[] — List -> elements; Assoc -> values; else []

    name: "iterate",
    str: {|fix iterate -> fun json -> case json
             | List(xs) => xs
             | Assoc(pairs) => list_map(pairs, fun pair -> case pair | (_, v) => v end)
             | _ => []
           end|},
    arg: Typ.term_of(JSON.t),
    ret: List(JSON.t),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("iterate"),
            fn(
              Pat.var("json"),
              match(
                var("json"),
                [
                  (Pat.ap(JSON.pat_json_list, Pat.var("xs")), var("xs")),
                  (
                    Pat.ap(JSON.pat_json_assoc, Pat.var("pairs")),
                    ap(
                      Forward,
                      var("list_map"),
                      tuple([
                        var("pairs"),
                        fn(
                          Pat.tuple([Pat.wild(), Pat.var("v")]),
                          var("v"),
                          None,
                          None,
                        ),
                      ]),
                    ),
                  ),
                  (Pat.wild(), list_lit([])),
                ],
              ),
              None,
              Some("iterate+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // keys: keys — Assoc -> [List([String(k1), ...])]; List -> [List([Int(0), ...])]; else [Null]

    name: "keys",
    str: {|fix keys -> fun json -> case json
             | Assoc(pairs) => [List(list_map(pairs, fun pair -> case pair | (k, _) => String(k) end))]
             | List(xs) => [List(mapi(xs, fun (i, _) -> Int(i)))]
             | _ => [Null]
           end|},
    arg: Typ.term_of(JSON.t),
    ret: List(JSON.t),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("keys"),
            fn(
              Pat.var("json"),
              match(
                var("json"),
                [
                  (
                    Pat.ap(JSON.pat_json_assoc, Pat.var("pairs")),
                    list_lit([
                      ap(
                        Forward,
                        JSON.json_list,
                        ap(
                          Forward,
                          var("list_map"),
                          tuple([
                            var("pairs"),
                            fn(
                              Pat.tuple([Pat.var("k"), Pat.wild()]),
                              ap(Forward, JSON.json_string, var("k")),
                              None,
                              None,
                            ),
                          ]),
                        ),
                      ),
                    ]),
                  ),
                  (
                    Pat.ap(JSON.pat_json_list, Pat.var("xs")),
                    list_lit([
                      ap(
                        Forward,
                        JSON.json_list,
                        ap(
                          Forward,
                          var("mapi"),
                          tuple([
                            var("xs"),
                            fn(
                              Pat.tuple([Pat.var("i"), Pat.wild()]),
                              ap(Forward, JSON.json_int, var("i")),
                              None,
                              None,
                            ),
                          ]),
                        ),
                      ),
                    ]),
                  ),
                  (Pat.wild(), list_lit([JSON.json_null])),
                ],
              ),
              None,
              Some("keys+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // values: values — Assoc -> [List(values)]; List -> [List(elements)]; else [Null]

    name: "values",
    str: {|fix values -> fun json -> case json
             | Assoc(pairs) => [List(list_map(pairs, fun pair -> case pair | (_, v) => v end))]
             | List(xs) => [List(xs)]
             | _ => [Null]
           end|},
    arg: Typ.term_of(JSON.t),
    ret: List(JSON.t),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("values"),
            fn(
              Pat.var("json"),
              match(
                var("json"),
                [
                  (
                    Pat.ap(JSON.pat_json_assoc, Pat.var("pairs")),
                    list_lit([
                      ap(
                        Forward,
                        JSON.json_list,
                        ap(
                          Forward,
                          var("list_map"),
                          tuple([
                            var("pairs"),
                            fn(
                              Pat.tuple([Pat.wild(), Pat.var("v")]),
                              var("v"),
                              None,
                              None,
                            ),
                          ]),
                        ),
                      ),
                    ]),
                  ),
                  (
                    Pat.ap(JSON.pat_json_list, Pat.var("xs")),
                    list_lit([ap(Forward, JSON.json_list, var("xs"))]),
                  ),
                  (Pat.wild(), list_lit([JSON.json_null])),
                ],
              ),
              None,
              Some("values+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // length: length — Assoc/List -> [Int(len)]; String -> [Int(len)]; Null -> [Int(0)]; else [Null]

    name: "length",
    str: {|fix length -> fun json -> case json
             | Assoc(pairs) => [Int(list_length(pairs))]
             | List(xs) => [Int(list_length(xs))]
             | String(s) => [Int(string_length(s))]
             | Null => [Int(0)]
             | _ => [Null]
           end|},
    arg: Typ.term_of(JSON.t),
    ret: List(JSON.t),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("length"),
            fn(
              Pat.var("json"),
              match(
                var("json"),
                [
                  (
                    Pat.ap(JSON.pat_json_assoc, Pat.var("pairs")),
                    list_lit([
                      ap(
                        Forward,
                        JSON.json_int,
                        ap(Forward, var("list_length"), var("pairs")),
                      ),
                    ]),
                  ),
                  (
                    Pat.ap(JSON.pat_json_list, Pat.var("xs")),
                    list_lit([
                      ap(
                        Forward,
                        JSON.json_int,
                        ap(Forward, var("list_length"), var("xs")),
                      ),
                    ]),
                  ),
                  (
                    Pat.ap(JSON.pat_json_string, Pat.var("s")),
                    list_lit([
                      ap(
                        Forward,
                        JSON.json_int,
                        ap(Forward, var("string_length"), var("s")),
                      ),
                    ]),
                  ),
                  (
                    JSON.pat_json_null,
                    list_lit([ap(Forward, JSON.json_int, int(0))]),
                  ),
                  (Pat.wild(), list_lit([JSON.json_null])),
                ],
              ),
              None,
              Some("length+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // type_: type — returns [String("null")], [String("boolean")], etc.

    name: "type_",
    str: {|fix type_ -> fun json -> case json
             | Null => [String("null")]
             | Bool(_) => [String("boolean")]
             | Int(_) => [String("number")]
             | Float(_) => [String("number")]
             | String(_) => [String("string")]
             | List(_) => [String("array")]
             | Assoc(_) => [String("object")]
           end|},
    arg: Typ.term_of(JSON.t),
    ret: List(JSON.t),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("type_"),
            fn(
              Pat.var("json"),
              match(
                var("json"),
                [
                  (
                    JSON.pat_json_null,
                    list_lit([
                      ap(Forward, JSON.json_string, string("null")),
                    ]),
                  ),
                  (
                    Pat.ap(JSON.pat_json_bool, Pat.wild()),
                    list_lit([
                      ap(Forward, JSON.json_string, string("boolean")),
                    ]),
                  ),
                  (
                    Pat.ap(JSON.pat_json_int, Pat.wild()),
                    list_lit([
                      ap(Forward, JSON.json_string, string("number")),
                    ]),
                  ),
                  (
                    Pat.ap(JSON.pat_json_float, Pat.wild()),
                    list_lit([
                      ap(Forward, JSON.json_string, string("number")),
                    ]),
                  ),
                  (
                    Pat.ap(JSON.pat_json_string, Pat.wild()),
                    list_lit([
                      ap(Forward, JSON.json_string, string("string")),
                    ]),
                  ),
                  (
                    Pat.ap(JSON.pat_json_list, Pat.wild()),
                    list_lit([
                      ap(Forward, JSON.json_string, string("array")),
                    ]),
                  ),
                  (
                    Pat.ap(JSON.pat_json_assoc, Pat.wild()),
                    list_lit([
                      ap(Forward, JSON.json_string, string("object")),
                    ]),
                  ),
                ],
              ),
              None,
              Some("type_+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // ---- Tier 2: Parameterized Filters ----

    // field: String -> JSON -> [JSON] — .foo
    // Assoc -> look up key, return [value] or [Null]; else [Null]

    name: "field",
    str: {|fix field -> fun key -> fun json -> case json
             | Assoc(pairs) => case assoc_opt(pairs, key)
                                 | None => [Null]
                                 | Some(v) => [v]
                               end
             | _ => [Null]
           end|},
    arg: Atom(String),
    ret: Arrow(JSON.t, list(JSON.t)),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("field"),
            fn(
              Pat.var("key"),
              fn(
                Pat.var("json"),
                match(
                  var("json"),
                  [
                    (
                      Pat.ap(JSON.pat_json_assoc, Pat.var("pairs")),
                      match(
                        ap(
                          Forward,
                          var("assoc_opt"),
                          tuple([var("pairs"), var("key")]),
                        ),
                        [
                          (Option.pat_none, list_lit([JSON.json_null])),
                          (
                            Pat.ap(Option.pat_some, Pat.var("v")),
                            list_lit([var("v")]),
                          ),
                        ],
                      ),
                    ),
                    (Pat.wild(), list_lit([JSON.json_null])),
                  ],
                ),
                None,
                None,
              ),
              None,
              Some("field+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // index: Int -> JSON -> [JSON] — .[n]
    // List -> nth element or [Null]; else [Null]

    name: "index",
    str: {|fix index -> fun n -> fun json -> case json
             | List(xs) => case nth_opt(xs, n)
                             | None => [Null]
                             | Some(v) => [v]
                           end
             | _ => [Null]
           end|},
    arg: Atom(Int),
    ret: Arrow(JSON.t, list(JSON.t)),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("index"),
            fn(
              Pat.var("n"),
              fn(
                Pat.var("json"),
                match(
                  var("json"),
                  [
                    (
                      Pat.ap(JSON.pat_json_list, Pat.var("xs")),
                      match(
                        ap(
                          Forward,
                          var("nth_opt"),
                          tuple([var("xs"), var("n")]),
                        ),
                        [
                          (Option.pat_none, list_lit([JSON.json_null])),
                          (
                            Pat.ap(Option.pat_some, Pat.var("v")),
                            list_lit([var("v")]),
                          ),
                        ],
                      ),
                    ),
                    (Pat.wild(), list_lit([JSON.json_null])),
                  ],
                ),
                None,
                None,
              ),
              None,
              Some("index+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // has: String -> JSON -> [JSON] — has("k")
    // Assoc -> [Bool(true/false)]; else [Bool(false)]

    name: "has",
    str: {|fix has -> fun key -> fun json -> case json
             | Assoc(pairs) => [Bool(mem_assoc(pairs, key))]
             | _ => [Bool(false)]
           end|},
    arg: Atom(String),
    ret: Arrow(JSON.t, list(JSON.t)),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("has"),
            fn(
              Pat.var("key"),
              fn(
                Pat.var("json"),
                match(
                  var("json"),
                  [
                    (
                      Pat.ap(JSON.pat_json_assoc, Pat.var("pairs")),
                      list_lit([
                        ap(
                          Forward,
                          JSON.json_bool,
                          ap(
                            Forward,
                            var("mem_assoc"),
                            tuple([var("pairs"), var("key")]),
                          ),
                        ),
                      ]),
                    ),
                    (
                      Pat.wild(),
                      list_lit([ap(Forward, JSON.json_bool, bool(false))]),
                    ),
                  ],
                ),
                None,
                None,
              ),
              None,
              Some("has+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // ---- Tier 3: Higher-Order Combinators ----

    // pipe: (JSON -> [JSON], JSON -> [JSON]) -> JSON -> [JSON]
    // pipe(f, g)(x) = flat_map(f(x), g)

    name: "pipe",
    str: {|fix pipe -> fun (f, g) -> fun json -> flat_map(f(json), g)|},
    arg: Prod([arrow(JSON.t, list(JSON.t)), arrow(JSON.t, list(JSON.t))]),
    ret: Arrow(JSON.t, list(JSON.t)),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("pipe"),
            fn(
              Pat.tuple([Pat.var("f"), Pat.var("g")]),
              fn(
                Pat.var("json"),
                ap(
                  Forward,
                  var("flat_map"),
                  tuple([ap(Forward, var("f"), var("json")), var("g")]),
                ),
                None,
                None,
              ),
              None,
              Some("pipe+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // select: (JSON -> [JSON]) -> JSON -> [JSON]
    // Keep input if f(input) produces any truthy value (not Null/Bool(false)), else []

    name: "select",
    str: {|fix select -> fun pred -> fun json ->
             let results = pred(json) in
             if any(results, fun r -> case r
                  | Null => false
                  | Bool(b) => b
                  | _ => true
                end)
             then [json]
             else []|},
    arg: Arrow(JSON.t, list(JSON.t)),
    ret: Arrow(JSON.t, list(JSON.t)),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("select"),
            fn(
              Pat.var("pred"),
              fn(
                Pat.var("json"),
                let_(
                  Pat.var("results"),
                  ap(Forward, var("pred"), var("json")),
                  if_(
                    ap(
                      Forward,
                      var("any"),
                      tuple([
                        var("results"),
                        fn(
                          Pat.var("r"),
                          match(
                            var("r"),
                            [
                              (JSON.pat_json_null, bool(false)),
                              (
                                Pat.ap(JSON.pat_json_bool, Pat.var("b")),
                                var("b"),
                              ),
                              (Pat.wild(), bool(true)),
                            ],
                          ),
                          None,
                          None,
                        ),
                      ]),
                    ),
                    list_lit([var("json")]),
                    list_lit([]),
                  ),
                ),
                None,
                None,
              ),
              None,
              Some("select+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // map: (JSON -> [JSON]) -> JSON -> [JSON]
    // Apply filter to each element, collect into [List(results)]
    // For List: apply f to each element, flat_map results, wrap in List
    // For Assoc: apply f to each value, flat_map results, wrap in List

    name: "map",
    str: {|fix map -> fun f -> fun json -> case json
             | List(xs) => [List(flat_map(xs, f))]
             | Assoc(pairs) => [List(flat_map(list_map(pairs, fun p -> case p | (_, v) => v end), f))]
             | _ => [Null]
           end|},
    arg: Arrow(JSON.t, list(JSON.t)),
    ret: Arrow(JSON.t, list(JSON.t)),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("map"),
            fn(
              Pat.var("f"),
              fn(
                Pat.var("json"),
                match(
                  var("json"),
                  [
                    (
                      Pat.ap(JSON.pat_json_list, Pat.var("xs")),
                      list_lit([
                        ap(
                          Forward,
                          JSON.json_list,
                          ap(
                            Forward,
                            var("flat_map"),
                            tuple([var("xs"), var("f")]),
                          ),
                        ),
                      ]),
                    ),
                    (
                      Pat.ap(JSON.pat_json_assoc, Pat.var("pairs")),
                      list_lit([
                        ap(
                          Forward,
                          JSON.json_list,
                          ap(
                            Forward,
                            var("flat_map"),
                            tuple([
                              ap(
                                Forward,
                                var("list_map"),
                                tuple([
                                  var("pairs"),
                                  fn(
                                    Pat.tuple([Pat.wild(), Pat.var("v")]),
                                    var("v"),
                                    None,
                                    None,
                                  ),
                                ]),
                              ),
                              var("f"),
                            ]),
                          ),
                        ),
                      ]),
                    ),
                    (Pat.wild(), list_lit([JSON.json_null])),
                  ],
                ),
                None,
                None,
              ),
              None,
              Some("map+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // run: [JSON -> [JSON]] -> JSON -> [JSON]
    // Compose a list of filters left-to-right via fold_left + flat_map.
    // run([f1, f2, f3])(json) = fold_left([f1, f2, f3], (acc, f) => flat_map(acc, f), [json])

    name: "run",
    str: {|fix run -> fun filters -> fun json -> fold_left(filters, fun (acc, f) -> flat_map(acc, f), [json])|},
    arg: List(arrow(JSON.t, list(JSON.t))),
    ret: Arrow(JSON.t, list(JSON.t)),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("run"),
            fn(
              Pat.var("filters"),
              fn(
                Pat.var("json"),
                ap(
                  Forward,
                  var("fold_left"),
                  tuple([
                    var("filters"),
                    fn(
                      Pat.tuple([Pat.var("acc"), Pat.var("f")]),
                      ap(
                        Forward,
                        var("flat_map"),
                        tuple([var("acc"), var("f")]),
                      ),
                      None,
                      None,
                    ),
                    list_lit([var("json")]),
                  ]),
                ),
                None,
                None,
              ),
              None,
              Some("run+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // ---- Tier 4: Mutation Combinators ----

    // set: (String, JSON) -> JSON -> [JSON]
    // Set or add a field in an object.
    // set("name", String("Alice"))(obj) => [Assoc(("name", String("Alice")) :: remove_assoc(pairs, "name"))]

    name: "set",
    str: {|fix set -> fun (key, val) -> fun json -> case json
             | Assoc(pairs) => [Assoc((key, val) :: remove_assoc(pairs, key))]
             | _ => [Null]
           end|},
    arg: Prod([string(), JSON.t]),
    ret: Arrow(JSON.t, list(JSON.t)),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("set"),
            fn(
              Pat.tuple([Pat.var("key"), Pat.var("val")]),
              fn(
                Pat.var("json"),
                match(
                  var("json"),
                  [
                    (
                      Pat.ap(JSON.pat_json_assoc, Pat.var("pairs")),
                      list_lit([
                        ap(
                          Forward,
                          JSON.json_assoc,
                          cons(
                            tuple([var("key"), var("val")]),
                            ap(
                              Forward,
                              var("remove_assoc"),
                              tuple([var("pairs"), var("key")]),
                            ),
                          ),
                        ),
                      ]),
                    ),
                    (Pat.wild(), list_lit([JSON.json_null])),
                  ],
                ),
                None,
                None,
              ),
              None,
              Some("set+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // update: (String, JSON -> [JSON]) -> JSON -> [JSON]
    // Update a field by applying a filter to its current value (like jq's |=).
    // Takes first result of filter. If field missing, returns object unchanged.

    name: "update",
    str: {|fix update -> fun (key, f) -> fun json -> case json
             | Assoc(pairs) => case assoc_opt(pairs, key)
               | None => [json]
               | Some(v) => case f(v)
                 | new_v :: _ => [Assoc((key, new_v) :: remove_assoc(pairs, key))]
                 | [] => [json]
               end
             end
             | _ => [Null]
           end|},
    arg: Prod([string(), arrow(JSON.t, list(JSON.t))]),
    ret: Arrow(JSON.t, list(JSON.t)),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("update"),
            fn(
              Pat.tuple([Pat.var("key"), Pat.var("f")]),
              fn(
                Pat.var("json"),
                match(
                  var("json"),
                  [
                    (
                      Pat.ap(JSON.pat_json_assoc, Pat.var("pairs")),
                      match(
                        ap(
                          Forward,
                          var("assoc_opt"),
                          tuple([var("pairs"), var("key")]),
                        ),
                        [
                          (Option.pat_none, list_lit([var("json")])),
                          (
                            Pat.ap(Option.pat_some, Pat.var("v")),
                            match(
                              ap(Forward, var("f"), var("v")),
                              [
                                (
                                  Pat.cons(Pat.var("new_v"), Pat.wild()),
                                  list_lit([
                                    ap(
                                      Forward,
                                      JSON.json_assoc,
                                      cons(
                                        tuple([var("key"), var("new_v")]),
                                        ap(
                                          Forward,
                                          var("remove_assoc"),
                                          tuple([var("pairs"), var("key")]),
                                        ),
                                      ),
                                    ),
                                  ]),
                                ),
                                (
                                  Pat.list_lit([]),
                                  list_lit([var("json")]),
                                ),
                              ],
                            ),
                          ),
                        ],
                      ),
                    ),
                    (Pat.wild(), list_lit([JSON.json_null])),
                  ],
                ),
                None,
                None,
              ),
              None,
              Some("update+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // del: String -> JSON -> [JSON]
    // Delete a field from an object.
    // del("name")(obj) => [Assoc(remove_assoc(pairs, "name"))]

    name: "del",
    str: {|fix del -> fun key -> fun json -> case json
             | Assoc(pairs) => [Assoc(remove_assoc(pairs, key))]
             | _ => [Null]
           end|},
    arg: Atom(String),
    ret: Arrow(JSON.t, list(JSON.t)),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("del"),
            fn(
              Pat.var("key"),
              fn(
                Pat.var("json"),
                match(
                  var("json"),
                  [
                    (
                      Pat.ap(JSON.pat_json_assoc, Pat.var("pairs")),
                      list_lit([
                        ap(
                          Forward,
                          JSON.json_assoc,
                          ap(
                            Forward,
                            var("remove_assoc"),
                            tuple([var("pairs"), var("key")]),
                          ),
                        ),
                      ]),
                    ),
                    (Pat.wild(), list_lit([JSON.json_null])),
                  ],
                ),
                None,
                None,
              ),
              None,
              Some("del+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // ---- Tier 5: Structural Combinators ----

    // to_entries: JSON -> [JSON]
    // Assoc([("k", v), ...]) => [List([Assoc([("key", String("k")), ("value", v)]), ...])]
    // Like jq's to_entries

    name: "to_entries",
    str: {|fix to_entries -> fun json -> case json
             | Assoc(pairs) => [List(list_map(pairs, fun pair -> case pair | (k, v) => Assoc([("key", String(k)), ("value", v)]) end))]
             | _ => [Null]
           end|},
    arg: Typ.term_of(JSON.t),
    ret: List(JSON.t),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("to_entries"),
            fn(
              Pat.var("json"),
              match(
                var("json"),
                [
                  (
                    Pat.ap(JSON.pat_json_assoc, Pat.var("pairs")),
                    list_lit([
                      ap(
                        Forward,
                        JSON.json_list,
                        ap(
                          Forward,
                          var("list_map"),
                          tuple([
                            var("pairs"),
                            fn(
                              Pat.tuple([Pat.var("k"), Pat.var("v")]),
                              ap(
                                Forward,
                                JSON.json_assoc,
                                list_lit([
                                  tuple([
                                    string("key"),
                                    ap(Forward, JSON.json_string, var("k")),
                                  ]),
                                  tuple([string("value"), var("v")]),
                                ]),
                              ),
                              None,
                              None,
                            ),
                          ]),
                        ),
                      ),
                    ]),
                  ),
                  (Pat.wild(), list_lit([JSON.json_null])),
                ],
              ),
              None,
              Some("to_entries+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // from_entries: JSON -> [JSON]
    // List([Assoc([("key", String("k")), ("value", v)]), ...]) => [Assoc([("k", v), ...])]
    // Like jq's from_entries

    name: "from_entries",
    str: {|fix from_entries -> fun json -> case json
             | List(entries) => [Assoc(list_map(entries, fun entry -> case entry
               | Assoc(pairs) => case (assoc_opt(pairs, "key"), assoc_opt(pairs, "value"))
                 | (Some(String(k)), Some(v)) => (k, v)
                 | _ => ("", Null)
               end
               | _ => ("", Null)
             end))]
             | _ => [Null]
           end|},
    arg: Typ.term_of(JSON.t),
    ret: List(JSON.t),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("from_entries"),
            fn(
              Pat.var("json"),
              match(
                var("json"),
                [
                  (
                    Pat.ap(JSON.pat_json_list, Pat.var("entries")),
                    list_lit([
                      ap(
                        Forward,
                        JSON.json_assoc,
                        ap(
                          Forward,
                          var("list_map"),
                          tuple([
                            var("entries"),
                            fn(
                              Pat.var("entry"),
                              match(
                                var("entry"),
                                [
                                  (
                                    Pat.ap(
                                      JSON.pat_json_assoc,
                                      Pat.var("pairs"),
                                    ),
                                    match(
                                      tuple([
                                        ap(
                                          Forward,
                                          var("assoc_opt"),
                                          tuple([
                                            var("pairs"),
                                            string("key"),
                                          ]),
                                        ),
                                        ap(
                                          Forward,
                                          var("assoc_opt"),
                                          tuple([
                                            var("pairs"),
                                            string("value"),
                                          ]),
                                        ),
                                      ]),
                                      [
                                        (
                                          Pat.tuple([
                                            Pat.ap(
                                              Option.pat_some,
                                              Pat.ap(
                                                JSON.pat_json_string,
                                                Pat.var("k"),
                                              ),
                                            ),
                                            Pat.ap(
                                              Option.pat_some,
                                              Pat.var("v"),
                                            ),
                                          ]),
                                          tuple([var("k"), var("v")]),
                                        ),
                                        (
                                          Pat.wild(),
                                          tuple([
                                            string(""),
                                            JSON.json_null,
                                          ]),
                                        ),
                                      ],
                                    ),
                                  ),
                                  (
                                    Pat.wild(),
                                    tuple([string(""), JSON.json_null]),
                                  ),
                                ],
                              ),
                              None,
                              None,
                            ),
                          ]),
                        ),
                      ),
                    ]),
                  ),
                  (Pat.wild(), list_lit([JSON.json_null])),
                ],
              ),
              None,
              Some("from_entries+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // startswith: String -> JSON -> [JSON]
    // String(s) where s starts with prefix => [Bool(true)]; else [Bool(false)]
    // Like jq's startswith("prefix")

    name: "startswith",
    str: {|fix startswith -> fun prefix -> fun json -> case json
             | String(s) =>
               let plen = string_length(prefix) in
               let slen = string_length(s) in
               if slen >= plen
               then [Bool(str_sub(s, 0, plen) $== prefix)]
               else [Bool(false)]
             | _ => [Bool(false)]
           end|},
    arg: Atom(String),
    ret: Arrow(JSON.t, list(JSON.t)),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("startswith"),
            fn(
              Pat.var("prefix"),
              fn(
                Pat.var("json"),
                match(
                  var("json"),
                  [
                    (
                      Pat.ap(JSON.pat_json_string, Pat.var("s")),
                      let_(
                        Pat.var("plen"),
                        ap(Forward, var("string_length"), var("prefix")),
                        let_(
                          Pat.var("slen"),
                          ap(Forward, var("string_length"), var("s")),
                          if_(
                            bin_op(
                              Int(GreaterThanOrEqual),
                              var("slen"),
                              var("plen"),
                            ),
                            list_lit([
                              ap(
                                Forward,
                                JSON.json_bool,
                                bin_op(
                                  Poly(Equals),
                                  ap(
                                    Forward,
                                    var("str_sub"),
                                    tuple([var("s"), int(0), var("plen")]),
                                  ),
                                  var("prefix"),
                                ),
                              ),
                            ]),
                            list_lit([
                              ap(Forward, JSON.json_bool, bool(false)),
                            ]),
                          ),
                        ),
                      ),
                    ),
                    (
                      Pat.wild(),
                      list_lit([ap(Forward, JSON.json_bool, bool(false))]),
                    ),
                  ],
                ),
                None,
                None,
              ),
              None,
              Some("startswith+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // endswith: String -> JSON -> [JSON]
    // String(s) where s ends with suffix => [Bool(true)]; else [Bool(false)]
    // Like jq's endswith("suffix")

    name: "endswith",
    str: {|fix endswith -> fun suffix -> fun json -> case json
             | String(s) =>
               let sfxlen = string_length(suffix) in
               let slen = string_length(s) in
               if slen >= sfxlen
               then [Bool(str_sub(s, slen - sfxlen, sfxlen) $== suffix)]
               else [Bool(false)]
             | _ => [Bool(false)]
           end|},
    arg: Atom(String),
    ret: Arrow(JSON.t, list(JSON.t)),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("endswith"),
            fn(
              Pat.var("suffix"),
              fn(
                Pat.var("json"),
                match(
                  var("json"),
                  [
                    (
                      Pat.ap(JSON.pat_json_string, Pat.var("s")),
                      let_(
                        Pat.var("sfxlen"),
                        ap(Forward, var("string_length"), var("suffix")),
                        let_(
                          Pat.var("slen"),
                          ap(Forward, var("string_length"), var("s")),
                          if_(
                            bin_op(
                              Int(GreaterThanOrEqual),
                              var("slen"),
                              var("sfxlen"),
                            ),
                            list_lit([
                              ap(
                                Forward,
                                JSON.json_bool,
                                bin_op(
                                  Poly(Equals),
                                  ap(
                                    Forward,
                                    var("str_sub"),
                                    tuple([
                                      var("s"),
                                      bin_op(
                                        Int(Minus),
                                        var("slen"),
                                        var("sfxlen"),
                                      ),
                                      var("sfxlen"),
                                    ]),
                                  ),
                                  var("suffix"),
                                ),
                              ),
                            ]),
                            list_lit([
                              ap(Forward, JSON.json_bool, bool(false)),
                            ]),
                          ),
                        ),
                      ),
                    ),
                    (
                      Pat.wild(),
                      list_lit([ap(Forward, JSON.json_bool, bool(false))]),
                    ),
                  ],
                ),
                None,
                None,
              ),
              None,
              Some("endswith+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // ---- Tier 6: Helper Combinators ----

    // run1: [JSON -> [JSON]] -> JSON -> JSON
    // Like run but returns the first result directly (not wrapped in a list).
    // Returns Null if the pipeline produces no results.

    name: "run1",
    str: {|fix run1 -> fun filters -> fun json -> case run(filters)(json)
             | x :: _ => x
             | [] => Null
           end|},
    arg: List(arrow(JSON.t, list(JSON.t))),
    ret: Arrow(JSON.t, JSON.t),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("run1"),
            fn(
              Pat.var("filters"),
              fn(
                Pat.var("json"),
                match(
                  ap(
                    Forward,
                    ap(Forward, var("run"), var("filters")),
                    var("json"),
                  ),
                  [
                    (Pat.cons(Pat.var("x"), Pat.wild()), var("x")),
                    (Pat.list_lit([]), JSON.json_null),
                  ],
                ),
                None,
                None,
              ),
              None,
              Some("run1+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // add: Int -> JSON -> [JSON]
    // Add an integer to a JSON Int value.
    // Int(x) => [Int(x + n)]; else [Null]

    name: "add",
    str: {|fix add -> fun n -> fun json -> case json
             | Int(x) => [Int(x + n)]
             | _ => [Null]
           end|},
    arg: Atom(Int),
    ret: Arrow(JSON.t, list(JSON.t)),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("add"),
            fn(
              Pat.var("n"),
              fn(
                Pat.var("json"),
                match(
                  var("json"),
                  [
                    (
                      Pat.ap(JSON.pat_json_int, Pat.var("x")),
                      list_lit([
                        ap(
                          Forward,
                          JSON.json_int,
                          bin_op(Int(Plus), var("x"), var("n")),
                        ),
                      ]),
                    ),
                    (Pat.wild(), list_lit([JSON.json_null])),
                  ],
                ),
                None,
                None,
              ),
              None,
              Some("add+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // not: JSON -> [JSON]
    // Negate truthiness: Bool(false) and Null => [Bool(true)]; anything else => [Bool(false)]
    // Like jq's `not` filter.

    name: "not",
    str: {|fix not -> fun json -> case json
             | Bool(false) => [Bool(true)]
             | Null => [Bool(true)]
             | _ => [Bool(false)]
           end|},
    arg: Typ.term_of(JSON.t),
    ret: List(JSON.t),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("not"),
            fn(
              Pat.var("json"),
              match(
                var("json"),
                [
                  (
                    Pat.ap(JSON.pat_json_bool, Pat.var("b")),
                    list_lit([
                      ap(
                        Forward,
                        JSON.json_bool,
                        if_(var("b"), bool(false), bool(true)),
                      ),
                    ]),
                  ),
                  (
                    JSON.pat_json_null,
                    list_lit([ap(Forward, JSON.json_bool, bool(true))]),
                  ),
                  (
                    Pat.wild(),
                    list_lit([ap(Forward, JSON.json_bool, bool(false))]),
                  ),
                ],
              ),
              None,
              Some("not+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // entry: (JSON -> [JSON], JSON -> [JSON]) -> JSON -> [JSON]
    // Construct an entry object {key: kf(json), value: vf(json)} from two filters.
    // Takes the first result of each filter.

    name: "entry",
    str: {|fix entry -> fun (kf, vf) -> fun json ->
             case (kf(json), vf(json))
             | (k :: _, v :: _) => [Assoc([("key", k), ("value", v)])]
             | _ => [Null]
           end|},
    arg: Prod([arrow(JSON.t, list(JSON.t)), arrow(JSON.t, list(JSON.t))]),
    ret: Arrow(JSON.t, list(JSON.t)),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("entry"),
            fn(
              Pat.tuple([Pat.var("kf"), Pat.var("vf")]),
              fn(
                Pat.var("json"),
                match(
                  tuple([
                    ap(Forward, var("kf"), var("json")),
                    ap(Forward, var("vf"), var("json")),
                  ]),
                  [
                    (
                      Pat.tuple([
                        Pat.cons(Pat.var("k"), Pat.wild()),
                        Pat.cons(Pat.var("v"), Pat.wild()),
                      ]),
                      list_lit([
                        ap(
                          Forward,
                          JSON.json_assoc,
                          list_lit([
                            tuple([string("key"), var("k")]),
                            tuple([string("value"), var("v")]),
                          ]),
                        ),
                      ]),
                    ),
                    (Pat.wild(), list_lit([JSON.json_null])),
                  ],
                ),
                None,
                None,
              ),
              None,
              Some("entry+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // with_entries: (JSON -> [JSON]) -> JSON -> [JSON]
    // Equivalent to: to_entries | list_map(f) | from_entries
    // Like jq's with_entries(f)

    name: "with_entries",
    str: {|fix with_entries -> fun f -> fun json ->
             jq([to_entries, list_map(f), from_entries])(json)|},
    arg: Arrow(JSON.t, list(JSON.t)),
    ret: Arrow(JSON.t, list(JSON.t)),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("with_entries"),
            fn(
              Pat.var("f"),
              fn(
                Pat.var("json"),
                ap(
                  Forward,
                  ap(
                    Forward,
                    var("run"),
                    list_lit([
                      var("to_entries"),
                      ap(Forward, var("map"), var("f")),
                      var("from_entries"),
                    ]),
                  ),
                  var("json"),
                ),
                None,
                None,
              ),
              None,
              Some("with_entries+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // merge: (JSON, JSON) -> [JSON]
    // Merge two Assoc objects. Second object's keys win on conflict.
    // Like jq's `*` for objects.

    name: "merge",
    str: {|fix merge -> fun (a, b) -> case (a, b)
             | (Assoc(pa), Assoc(pb)) => [Assoc(fold_left(pb, fun (acc, (k, v)) -> (k, v) :: remove_assoc(acc, k), pa))]
             | _ => [Null]
           end|},
    arg: Prod([JSON.t, JSON.t]),
    ret: List(JSON.t),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("merge"),
            fn(
              Pat.tuple([Pat.var("a"), Pat.var("b")]),
              match(
                tuple([var("a"), var("b")]),
                [
                  (
                    Pat.tuple([
                      Pat.ap(JSON.pat_json_assoc, Pat.var("pa")),
                      Pat.ap(JSON.pat_json_assoc, Pat.var("pb")),
                    ]),
                    list_lit([
                      ap(
                        Forward,
                        JSON.json_assoc,
                        ap(
                          Forward,
                          var("fold_left"),
                          tuple([
                            var("pb"),
                            fn(
                              Pat.tuple([
                                Pat.var("acc"),
                                Pat.tuple([Pat.var("k"), Pat.var("v")]),
                              ]),
                              cons(
                                tuple([var("k"), var("v")]),
                                ap(
                                  Forward,
                                  var("remove_assoc"),
                                  tuple([var("acc"), var("k")]),
                                ),
                              ),
                              None,
                              None,
                            ),
                            var("pa"),
                          ]),
                        ),
                      ),
                    ]),
                  ),
                  (Pat.wild(), list_lit([JSON.json_null])),
                ],
              ),
              None,
              Some("merge+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // string_sub: (String, String) -> JSON -> [JSON]
    // Replace occurrences of a pattern in a JSON String.
    // String(s) => [String(string_replace(from, s, to))]; else [Null]
    // Like jq's gsub/sub

    name: "string_sub",
    str: {|fix string_sub -> fun (from, to_str) -> fun json -> case json
             | String(s) => [String(string_replace(from, s, to_str))]
             | _ => [Null]
           end|},
    arg: Prod([string(), string()]),
    ret: Arrow(JSON.t, list(JSON.t)),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("string_sub"),
            fn(
              Pat.tuple([Pat.var("from"), Pat.var("to_str")]),
              fn(
                Pat.var("json"),
                match(
                  var("json"),
                  [
                    (
                      Pat.ap(JSON.pat_json_string, Pat.var("s")),
                      list_lit([
                        ap(
                          Forward,
                          JSON.json_string,
                          ap(
                            Forward,
                            var("string_replace"),
                            tuple([var("from"), var("s"), var("to_str")]),
                          ),
                        ),
                      ]),
                    ),
                    (Pat.wild(), list_lit([JSON.json_null])),
                  ],
                ),
                None,
                None,
              ),
              None,
              Some("string_sub+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // ltrimstr: String -> JSON -> [JSON]
    // Trim a prefix from a JSON String. If the string doesn't start with the prefix,
    // returns the string unchanged. Like jq's ltrimstr.

    name: "ltrimstr",
    str: {|fix ltrimstr -> fun prefix -> fun json -> case json
             | String(s) =>
               let plen = string_length(prefix) in
               let slen = string_length(s) in
               if slen >= plen && str_sub(s, 0, plen) $== prefix
               then [String(str_sub(s, plen, slen - plen))]
               else [json]
             | _ => [json]
           end|},
    arg: Atom(String),
    ret: Arrow(JSON.t, list(JSON.t)),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("ltrimstr"),
            fn(
              Pat.var("prefix"),
              fn(
                Pat.var("json"),
                match(
                  var("json"),
                  [
                    (
                      Pat.ap(JSON.pat_json_string, Pat.var("s")),
                      let_(
                        Pat.var("plen"),
                        ap(Forward, var("string_length"), var("prefix")),
                        let_(
                          Pat.var("slen"),
                          ap(Forward, var("string_length"), var("s")),
                          if_(
                            bin_op(
                              Int(GreaterThanOrEqual),
                              var("slen"),
                              var("plen"),
                            ),
                            if_(
                              bin_op(
                                Poly(Equals),
                                ap(
                                  Forward,
                                  var("str_sub"),
                                  tuple([var("s"), int(0), var("plen")]),
                                ),
                                var("prefix"),
                              ),
                              list_lit([
                                ap(
                                  Forward,
                                  JSON.json_string,
                                  ap(
                                    Forward,
                                    var("str_sub"),
                                    tuple([
                                      var("s"),
                                      var("plen"),
                                      bin_op(
                                        Int(Minus),
                                        var("slen"),
                                        var("plen"),
                                      ),
                                    ]),
                                  ),
                                ),
                              ]),
                              list_lit([var("json")]),
                            ),
                            list_lit([var("json")]),
                          ),
                        ),
                      ),
                    ),
                    (Pat.wild(), list_lit([var("json")])),
                  ],
                ),
                None,
                None,
              ),
              None,
              Some("ltrimstr+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // string_prepend: String -> JSON -> [JSON]
    // Prepend a string to a JSON String value.
    // String(s) => [String(prefix ++ s)]; else [Null]

    name: "string_prepend",
    str: {|fix string_prepend -> fun prefix -> fun json -> case json
             | String(s) => [String(prefix ++ s)]
             | _ => [Null]
           end|},
    arg: Atom(String),
    ret: Arrow(JSON.t, list(JSON.t)),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("string_prepend"),
            fn(
              Pat.var("prefix"),
              fn(
                Pat.var("json"),
                match(
                  var("json"),
                  [
                    (
                      Pat.ap(JSON.pat_json_string, Pat.var("s")),
                      list_lit([
                        ap(
                          Forward,
                          JSON.json_string,
                          bin_op(String(Concat), var("prefix"), var("s")),
                        ),
                      ]),
                    ),
                    (Pat.wild(), list_lit([JSON.json_null])),
                  ],
                ),
                None,
                None,
              ),
              None,
              Some("string_prepend+"),
            ),
            None,
          )
        )
      );
    },
  },
];

/* Aliases for global builtins whose names are shadowed by members of the
 * Jq module (the member let-chain binds map, length, and string_sub).
 * These are let-bound at the top of the module chain for internal use by
 * member implementations, but not exported. */
let internals: list((string, Exp.t)) = [
  ("list_map", Fresh.Exp.var("map")),
  ("list_length", Fresh.Exp.var("length")),
  ("str_sub", Fresh.Exp.var("string_sub")),
];
