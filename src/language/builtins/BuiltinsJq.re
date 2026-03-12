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

    // jq_identity: . — returns [json]

    name: "jq_identity",
    str: {|fix jq_identity -> fun json -> [json]|},
    arg: Typ.term_of(JSON.t),
    ret: List(JSON.t),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("jq_identity"),
            fn(
              Pat.var("json"),
              list_lit([var("json")]),
              None,
              Some("jq_identity+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // jq_iterate: .[] — List -> elements; Assoc -> values; else []

    name: "jq_iterate",
    str: {|fix jq_iterate -> fun json -> case json
             | List(xs) => xs
             | Assoc(pairs) => map(pairs, fun pair -> case pair | (_, v) => v end)
             | _ => []
           end|},
    arg: Typ.term_of(JSON.t),
    ret: List(JSON.t),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("jq_iterate"),
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
                      var("map"),
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
              Some("jq_iterate+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // jq_keys: keys — Assoc -> [List([String(k1), ...])]; List -> [List([Int(0), ...])]; else [Null]

    name: "jq_keys",
    str: {|fix jq_keys -> fun json -> case json
             | Assoc(pairs) => [List(map(pairs, fun pair -> case pair | (k, _) => String(k) end))]
             | List(xs) => [List(mapi(xs, fun (i, _) -> Int(i)))]
             | _ => [Null]
           end|},
    arg: Typ.term_of(JSON.t),
    ret: List(JSON.t),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("jq_keys"),
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
                          var("map"),
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
              Some("jq_keys+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // jq_values: values — Assoc -> [List(values)]; List -> [List(elements)]; else [Null]

    name: "jq_values",
    str: {|fix jq_values -> fun json -> case json
             | Assoc(pairs) => [List(map(pairs, fun pair -> case pair | (_, v) => v end))]
             | List(xs) => [List(xs)]
             | _ => [Null]
           end|},
    arg: Typ.term_of(JSON.t),
    ret: List(JSON.t),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("jq_values"),
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
                          var("map"),
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
              Some("jq_values+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // jq_length: length — Assoc/List -> [Int(len)]; String -> [Int(len)]; Null -> [Int(0)]; else [Null]

    name: "jq_length",
    str: {|fix jq_length -> fun json -> case json
             | Assoc(pairs) => [Int(length(pairs))]
             | List(xs) => [Int(length(xs))]
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
            Pat.var("jq_length"),
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
                        ap(Forward, var("length"), var("pairs")),
                      ),
                    ]),
                  ),
                  (
                    Pat.ap(JSON.pat_json_list, Pat.var("xs")),
                    list_lit([
                      ap(
                        Forward,
                        JSON.json_int,
                        ap(Forward, var("length"), var("xs")),
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
              Some("jq_length+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // jq_type: type — returns [String("null")], [String("boolean")], etc.

    name: "jq_type",
    str: {|fix jq_type -> fun json -> case json
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
            Pat.var("jq_type"),
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
              Some("jq_type+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // ---- Tier 2: Parameterized Filters ----

    // jq_field: String -> JSON -> [JSON] — .foo
    // Assoc -> look up key, return [value] or [Null]; else [Null]

    name: "jq_field",
    str: {|fix jq_field -> fun key -> fun json -> case json
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
            Pat.var("jq_field"),
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
              Some("jq_field+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // jq_index: Int -> JSON -> [JSON] — .[n]
    // List -> nth element or [Null]; else [Null]

    name: "jq_index",
    str: {|fix jq_index -> fun n -> fun json -> case json
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
            Pat.var("jq_index"),
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
              Some("jq_index+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // jq_has: String -> JSON -> [JSON] — has("k")
    // Assoc -> [Bool(true/false)]; else [Bool(false)]

    name: "jq_has",
    str: {|fix jq_has -> fun key -> fun json -> case json
             | Assoc(pairs) => [Bool(mem_assoc(pairs, key))]
             | _ => [Bool(false)]
           end|},
    arg: Atom(String),
    ret: Arrow(JSON.t, list(JSON.t)),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("jq_has"),
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
              Some("jq_has+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // ---- Tier 3: Higher-Order Combinators ----

    // jq_pipe: (JSON -> [JSON], JSON -> [JSON]) -> JSON -> [JSON]
    // jq_pipe(f, g)(x) = flat_map(f(x), g)

    name: "jq_pipe",
    str: {|fix jq_pipe -> fun (f, g) -> fun json -> flat_map(f(json), g)|},
    arg: Prod([arrow(JSON.t, list(JSON.t)), arrow(JSON.t, list(JSON.t))]),
    ret: Arrow(JSON.t, list(JSON.t)),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("jq_pipe"),
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
              Some("jq_pipe+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // jq_select: (JSON -> [JSON]) -> JSON -> [JSON]
    // Keep input if f(input) produces any truthy value (not Null/Bool(false)), else []

    name: "jq_select",
    str: {|fix jq_select -> fun pred -> fun json ->
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
            Pat.var("jq_select"),
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
              Some("jq_select+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // jq_map: (JSON -> [JSON]) -> JSON -> [JSON]
    // Apply filter to each element, collect into [List(results)]
    // For List: apply f to each element, flat_map results, wrap in List
    // For Assoc: apply f to each value, flat_map results, wrap in List

    name: "jq_map",
    str: {|fix jq_map -> fun f -> fun json -> case json
             | List(xs) => [List(flat_map(xs, f))]
             | Assoc(pairs) => [List(flat_map(map(pairs, fun p -> case p | (_, v) => v end), f))]
             | _ => [Null]
           end|},
    arg: Arrow(JSON.t, list(JSON.t)),
    ret: Arrow(JSON.t, list(JSON.t)),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("jq_map"),
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
                                var("map"),
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
              Some("jq_map+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    // jq: [JSON -> [JSON]] -> JSON -> [JSON]
    // Compose a list of filters left-to-right via fold_left + flat_map.
    // jq([f1, f2, f3])(json) = fold_left([f1, f2, f3], (acc, f) => flat_map(acc, f), [json])

    name: "jq",
    str: {|fix jq -> fun filters -> fun json -> fold_left(filters, fun (acc, f) -> flat_map(acc, f), [json])|},
    arg: List(arrow(JSON.t, list(JSON.t))),
    ret: Arrow(JSON.t, list(JSON.t)),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            Pat.var("jq"),
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
              Some("jq+"),
            ),
            None,
          )
        )
      );
    },
  },
];
