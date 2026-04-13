module Fresh = IdTagged.FreshGrammar;
open Fresh.Typ;
module JSON = BuiltinsADT.JSON;

module E = Fresh.Exp;
module P = Fresh.Pat;

let call = (name: string, a) => E.ap(Forward, E.var(name), a);
let call2 = (name: string, a, b) => E.ap(Forward, call(name, a), b);
let call3 = (name: string, a, b, c) => E.ap(Forward, call2(name, a, b), c);
let call4 = (name: string, a, b, c, d) =>
  E.ap(Forward, call3(name, a, b, c), d);

let t2 = (a, b) => E.tuple([a, b]);
let t3 = (a, b, c) => E.tuple([a, b, c]);
let t4 = (a, b, c, d) => E.tuple([a, b, c, d]);

let list1 = x => E.list_lit([x]);
let list2 = (x, y) => E.list_lit([x, y]);

let append = (a, b) => call("append", t2(a, b));
let map_ = (xs, f) => call("map", t2(xs, f));
let mapi_ = (xs, f) => call("mapi", t2(xs, f));
let flat_map_ = (xs, f) => call("flat_map", t2(xs, f));
let filter_ = (xs, f) => call("filter", t2(xs, f));
let range_ = (start_, end_) => call("range", t2(start_, end_));
let length_ = xs => call("length", xs);
let concat_ = xss => call("concat", xss);

let sconcat = (a, b) => E.bin_op(String(Concat), a, b);
let int_add = (a, b) => E.bin_op(Int(Plus), a, b);
let int_sub = (a, b) => E.bin_op(Int(Minus), a, b);
let eq_ = (a, b) => E.bin_op(Poly(Equals), a, b);
let lt_ = (a, b) => E.bin_op(Int(LessThan), a, b);
let gt_ = (a, b) => E.bin_op(Int(GreaterThan), a, b);
let le_ = (a, b) => E.bin_op(Int(LessThanOrEqual), a, b);

let jstr = s => E.ap(Forward, JSON.json_string, E.string(s));
let jint = i => E.ap(Forward, JSON.json_int, E.int(i));
let jlist = xs => E.ap(Forward, JSON.json_list, E.list_lit(xs));
let jassoc = pairs => E.ap(Forward, JSON.json_assoc, E.list_lit(pairs));
let pair = (k: string, v) => E.tuple([E.string(k), v]);
let json_t = Typ.term_of(JSON.t);

let builtins: list(BuiltinsUtil.hazel_fn) = [
  {
    name: "get_petri_def",
    str: {|fix get_petri_def -> fun net ->
  jq1([jq_field("petriNetDefinition")])(net)|},
    arg: json_t,
    ret: json_t,
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("get_petri_def"),
            fn(
              P.var("net"),
              call2(
                "jq1",
                E.list_lit([
                  call("jq_field", E.string("petriNetDefinition")),
                ]),
                E.var("net"),
              ),
              None,
              Some("get_petri_def+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "set_petri_def",
    str: {|fix set_petri_def -> fun net -> fun pnd ->
  jq1([jq_update("petriNetDefinition", fun _ -> [pnd])])(net)|},
    arg: json_t,
    ret: Arrow(JSON.t, JSON.t),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("set_petri_def"),
            fn(
              P.var("net"),
              fn(
                P.var("pnd"),
                call2(
                  "jq1",
                  E.list_lit([
                    call(
                      "jq_update",
                      t2(
                        E.string("petriNetDefinition"),
                        fn(P.wild(), list1(E.var("pnd")), None, None),
                      ),
                    ),
                  ]),
                  E.var("net"),
                ),
                None,
                None,
              ),
              None,
              Some("set_petri_def+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "get_nodes",
    str: {|fix get_nodes -> fun net ->
  jq([jq_field("petriNetDefinition"), jq_field("nodes"), jq_iterate])(net)|},
    arg: json_t,
    ret: List(JSON.t),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("get_nodes"),
            fn(
              P.var("net"),
              call2(
                "jq",
                E.list_lit([
                  call("jq_field", E.string("petriNetDefinition")),
                  call("jq_field", E.string("nodes")),
                  E.var("jq_iterate"),
                ]),
                E.var("net"),
              ),
              None,
              Some("get_nodes+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "get_arcs",
    str: {|fix get_arcs -> fun net ->
  jq([jq_field("petriNetDefinition"), jq_field("arcs"), jq_iterate])(net)|},
    arg: json_t,
    ret: List(JSON.t),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("get_arcs"),
            fn(
              P.var("net"),
              call2(
                "jq",
                E.list_lit([
                  call("jq_field", E.string("petriNetDefinition")),
                  call("jq_field", E.string("arcs")),
                  E.var("jq_iterate"),
                ]),
                E.var("net"),
              ),
              None,
              Some("get_arcs+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "set_nodes",
    str: {|fix set_nodes -> fun net -> fun nodes ->
  let pnd = get_petri_def(net) in
  let pnd2 = jq1([jq_set("nodes", List(nodes))])(pnd) in
  set_petri_def(net)(pnd2)|},
    arg: json_t,
    ret: Arrow(list(JSON.t), JSON.t),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("set_nodes"),
            fn(
              P.var("net"),
              fn(
                P.var("nodes"),
                let_(
                  P.var("pnd"),
                  call("get_petri_def", E.var("net")),
                  let_(
                    P.var("pnd2"),
                    call2(
                      "jq1",
                      list1(
                        call(
                          "jq_set",
                          t2(
                            E.string("nodes"),
                            E.ap(Forward, JSON.json_list, E.var("nodes")),
                          ),
                        ),
                      ),
                      E.var("pnd"),
                    ),
                    call2("set_petri_def", E.var("net"), E.var("pnd2")),
                  ),
                ),
                None,
                None,
              ),
              None,
              Some("set_nodes+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "set_arcs",
    str: {|fix set_arcs -> fun net -> fun arcs ->
  let pnd = get_petri_def(net) in
  let pnd2 = jq1([jq_set("arcs", List(arcs))])(pnd) in
  set_petri_def(net)(pnd2)|},
    arg: json_t,
    ret: Arrow(list(JSON.t), JSON.t),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("set_arcs"),
            fn(
              P.var("net"),
              fn(
                P.var("arcs"),
                let_(
                  P.var("pnd"),
                  call("get_petri_def", E.var("net")),
                  let_(
                    P.var("pnd2"),
                    call2(
                      "jq1",
                      list1(
                        call(
                          "jq_set",
                          t2(
                            E.string("arcs"),
                            E.ap(Forward, JSON.json_list, E.var("arcs")),
                          ),
                        ),
                      ),
                      E.var("pnd"),
                    ),
                    call2("set_petri_def", E.var("net"), E.var("pnd2")),
                  ),
                ),
                None,
                None,
              ),
              None,
              Some("set_arcs+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "get_id",
    str: {|fix get_id -> fun node ->
  case jq1([jq_field("id")])(node)
  | String(x) => x
  | _ => ""
  end|},
    arg: json_t,
    ret: Atom(String),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("get_id"),
            fn(
              P.var("node"),
              match(
                call2(
                  "jq1",
                  list1(call("jq_field", E.string("id"))),
                  E.var("node"),
                ),
                [
                  (P.ap(JSON.pat_json_string, P.var("x")), E.var("x")),
                  (P.wild(), E.string("")),
                ],
              ),
              None,
              Some("get_id+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "arc_source",
    str: {|fix arc_source -> fun arc ->
  case jq1([jq_field("source")])(arc)
  | String(x) => x
  | _ => ""
  end|},
    arg: json_t,
    ret: Atom(String),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("arc_source"),
            fn(
              P.var("arc"),
              match(
                call2(
                  "jq1",
                  list1(call("jq_field", E.string("source"))),
                  E.var("arc"),
                ),
                [
                  (P.ap(JSON.pat_json_string, P.var("x")), E.var("x")),
                  (P.wild(), E.string("")),
                ],
              ),
              None,
              Some("arc_source+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "arc_target",
    str: {|fix arc_target -> fun arc ->
  case jq1([jq_field("target")])(arc)
  | String(x) => x
  | _ => ""
  end|},
    arg: json_t,
    ret: Atom(String),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("arc_target"),
            fn(
              P.var("arc"),
              match(
                call2(
                  "jq1",
                  list1(call("jq_field", E.string("target"))),
                  E.var("arc"),
                ),
                [
                  (P.ap(JSON.pat_json_string, P.var("x")), E.var("x")),
                  (P.wild(), E.string("")),
                ],
              ),
              None,
              Some("arc_target+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "arc_set_source",
    str: {|fix arc_set_source -> fun arc -> fun src ->
  jq1([jq_set("source", String(src))])(arc)|},
    arg: json_t,
    ret: Arrow(string(), JSON.t),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("arc_set_source"),
            fn(
              P.var("arc"),
              fn(
                P.var("src"),
                call2(
                  "jq1",
                  list1(
                    call(
                      "jq_set",
                      t2(
                        E.string("source"),
                        E.ap(Forward, JSON.json_string, E.var("src")),
                      ),
                    ),
                  ),
                  E.var("arc"),
                ),
                None,
                None,
              ),
              None,
              Some("arc_set_source+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "arc_set_target",
    str: {|fix arc_set_target -> fun arc -> fun tgt ->
  jq1([jq_set("target", String(tgt))])(arc)|},
    arg: json_t,
    ret: Arrow(string(), JSON.t),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("arc_set_target"),
            fn(
              P.var("arc"),
              fn(
                P.var("tgt"),
                call2(
                  "jq1",
                  list1(
                    call(
                      "jq_set",
                      t2(
                        E.string("target"),
                        E.ap(Forward, JSON.json_string, E.var("tgt")),
                      ),
                    ),
                  ),
                  E.var("arc"),
                ),
                None,
                None,
              ),
              None,
              Some("arc_set_target+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "mk_place_tokens",
    str: {|fix mk_place_tokens -> fun id -> fun tokens ->
  Assoc([
    ("data", Assoc([
      ("label", String(id)),
      ("type", String("place")),
      ("initialTokenCounts", Assoc([("default", Int(tokens))])),
      ("tokenCounts", Assoc([("default", Int(tokens))]))
    ])),
    ("id", String(id)),
    ("position", Assoc([("x", Int(0)), ("y", Int(0))])),
    ("type", String("place")),
    ("height", Int(130)),
    ("width", Int(130))
  ])|},
    arg: Atom(String),
    ret: Arrow(int(), JSON.t),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("mk_place_tokens"),
            fn(
              P.var("id"),
              fn(
                P.var("tokens"),
                jassoc([
                  pair(
                    "data",
                    jassoc([
                      pair(
                        "label",
                        E.ap(Forward, JSON.json_string, E.var("id")),
                      ),
                      pair("type", jstr("place")),
                      pair(
                        "initialTokenCounts",
                        jassoc([
                          pair(
                            "default",
                            E.ap(Forward, JSON.json_int, E.var("tokens")),
                          ),
                        ]),
                      ),
                      pair(
                        "tokenCounts",
                        jassoc([
                          pair(
                            "default",
                            E.ap(Forward, JSON.json_int, E.var("tokens")),
                          ),
                        ]),
                      ),
                    ]),
                  ),
                  pair("id", E.ap(Forward, JSON.json_string, E.var("id"))),
                  pair(
                    "position",
                    jassoc([pair("x", jint(0)), pair("y", jint(0))]),
                  ),
                  pair("type", jstr("place")),
                  pair("height", jint(130)),
                  pair("width", jint(130)),
                ]),
                None,
                None,
              ),
              None,
              Some("mk_place_tokens+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "mk_place",
    str: {|fix mk_place -> fun id ->
  mk_place_tokens(id)(0)|},
    arg: Atom(String),
    ret: json_t,
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("mk_place"),
            fn(
              P.var("id"),
              call2("mk_place_tokens", E.var("id"), E.int(0)),
              None,
              Some("mk_place+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "mk_transition",
    str: {|fix mk_transition -> fun id ->
  Assoc([
    ("data", Assoc([
      ("label", String(id)),
      ("type", String("transition"))
    ])),
    ("id", String(id)),
    ("position", Assoc([("x", Int(0)), ("y", Int(0))])),
    ("type", String("transition")),
    ("height", Int(80)),
    ("width", Int(160))
  ])|},
    arg: Atom(String),
    ret: json_t,
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("mk_transition"),
            fn(
              P.var("id"),
              jassoc([
                pair(
                  "data",
                  jassoc([
                    pair(
                      "label",
                      E.ap(Forward, JSON.json_string, E.var("id")),
                    ),
                    pair("type", jstr("transition")),
                  ]),
                ),
                pair("id", E.ap(Forward, JSON.json_string, E.var("id"))),
                pair(
                  "position",
                  jassoc([pair("x", jint(0)), pair("y", jint(0))]),
                ),
                pair("type", jstr("transition")),
                pair("height", jint(80)),
                pair("width", jint(160)),
              ]),
              None,
              Some("mk_transition+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "mk_arc",
    str: {|fix mk_arc -> fun src -> fun tgt -> fun weight ->
  Assoc([
    ("source", String(src)),
    ("sourceHandle", Null),
    ("target", String(tgt)),
    ("targetHandle", Null),
    ("id", String("arc__" ++ src ++ "-" ++ tgt)),
    ("type", String("default")),
    ("data", Assoc([
      ("tokenWeights", Assoc([
        ("default", Int(weight))
      ]))
    ])),
    ("interactionWidth", Int(8))
  ])|},
    arg: Atom(String),
    ret: Arrow(string(), arrow(int(), JSON.t)),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("mk_arc"),
            fn(
              P.var("src"),
              fn(
                P.var("tgt"),
                fn(
                  P.var("weight"),
                  jassoc([
                    pair(
                      "source",
                      E.ap(Forward, JSON.json_string, E.var("src")),
                    ),
                    pair("sourceHandle", JSON.json_null),
                    pair(
                      "target",
                      E.ap(Forward, JSON.json_string, E.var("tgt")),
                    ),
                    pair("targetHandle", JSON.json_null),
                    pair(
                      "id",
                      E.ap(
                        Forward,
                        JSON.json_string,
                        sconcat(
                          sconcat(
                            sconcat(E.string("arc__"), E.var("src")),
                            E.string("-"),
                          ),
                          E.var("tgt"),
                        ),
                      ),
                    ),
                    pair("type", jstr("default")),
                    pair(
                      "data",
                      jassoc([
                        pair(
                          "tokenWeights",
                          jassoc([
                            pair(
                              "default",
                              E.ap(Forward, JSON.json_int, E.var("weight")),
                            ),
                          ]),
                        ),
                      ]),
                    ),
                    pair("interactionWidth", jint(8)),
                  ]),
                  None,
                  None,
                ),
                None,
                None,
              ),
              None,
              Some("mk_arc+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "seq_place_arcs",
    str: {|fix seq_place_arcs -> fun current_place -> fun prefix -> fun i -> fun n ->
  if i > n
  then []
  else
    let tid = prefix ++ "_t" ++ string_of_int(i) in
    if i == n
    then [mk_arc(current_place)(tid)(1), mk_arc(tid)(prefix ++ "_pout")(1)]
    else
      let nextp = prefix ++ "_p" ++ string_of_int(i) in
      [mk_arc(current_place)(tid)(1), mk_arc(tid)(nextp)(1)] @ seq_place_arcs(nextp)(prefix)(i + 1)(n)|},
    arg: Atom(String),
    ret: Unknown(Internal),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("seq_place_arcs"),
            fn(
              P.var("current_place"),
              fn(
                P.var("prefix"),
                fn(
                  P.var("i"),
                  fn(
                    P.var("n"),
                    if_(
                      gt_(E.var("i"), E.var("n")),
                      E.list_lit([]),
                      let_(
                        P.var("tid"),
                        sconcat(
                          sconcat(E.var("prefix"), E.string("_t")),
                          call("string_of_int", E.var("i")),
                        ),
                        if_(
                          eq_(E.var("i"), E.var("n")),
                          list2(
                            call3(
                              "mk_arc",
                              E.var("current_place"),
                              E.var("tid"),
                              E.int(1),
                            ),
                            call3(
                              "mk_arc",
                              E.var("tid"),
                              sconcat(E.var("prefix"), E.string("_pout")),
                              E.int(1),
                            ),
                          ),
                          let_(
                            P.var("nextp"),
                            sconcat(
                              sconcat(E.var("prefix"), E.string("_p")),
                              call("string_of_int", E.var("i")),
                            ),
                            append(
                              list2(
                                call3(
                                  "mk_arc",
                                  E.var("current_place"),
                                  E.var("tid"),
                                  E.int(1),
                                ),
                                call3(
                                  "mk_arc",
                                  E.var("tid"),
                                  E.var("nextp"),
                                  E.int(1),
                                ),
                              ),
                              call4(
                                "seq_place_arcs",
                                E.var("nextp"),
                                E.var("prefix"),
                                int_add(E.var("i"), E.int(1)),
                                E.var("n"),
                              ),
                            ),
                          ),
                        ),
                      ),
                    ),
                    None,
                    None,
                  ),
                  None,
                  None,
                ),
                None,
                None,
              ),
              None,
              Some("seq_place_arcs+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "build_sequence_place_module",
    str: {|fix build_sequence_place_module -> fun prefix -> fun n ->
  let n1 = if n < 1 then 1 else n in
  let pin = prefix ++ "_pin" in
  let pout = prefix ++ "_pout" in
  let mids = map(range(1, n1 - 1), fun i -> mk_place(prefix ++ "_p" ++ string_of_int(i))) in
  let trans = map(range(1, n1), fun i -> mk_transition(prefix ++ "_t" ++ string_of_int(i))) in
  let nodes = [mk_place(pin)] @ mids @ [mk_place(pout)] @ trans in
  let arcs = seq_place_arcs(pin)(prefix)(1)(n1) in
  (nodes, arcs, pin, pout)|},
    arg: Atom(String),
    ret: Unknown(Internal),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("build_sequence_place_module"),
            fn(
              P.var("prefix"),
              fn(
                P.var("n"),
                let_(
                  P.var("n1"),
                  if_(lt_(E.var("n"), E.int(1)), E.int(1), E.var("n")),
                  let_(
                    P.var("pin"),
                    sconcat(E.var("prefix"), E.string("_pin")),
                    let_(
                      P.var("pout"),
                      sconcat(E.var("prefix"), E.string("_pout")),
                      let_(
                        P.var("mids"),
                        map_(
                          range_(
                            E.int(1),
                            int_sub(E.var("n1"), E.int(1)),
                          ),
                          fn(
                            P.var("i"),
                            call(
                              "mk_place",
                              sconcat(
                                sconcat(E.var("prefix"), E.string("_p")),
                                call("string_of_int", E.var("i")),
                              ),
                            ),
                            None,
                            None,
                          ),
                        ),
                        let_(
                          P.var("trans"),
                          map_(
                            range_(E.int(1), E.var("n1")),
                            fn(
                              P.var("i"),
                              call(
                                "mk_transition",
                                sconcat(
                                  sconcat(E.var("prefix"), E.string("_t")),
                                  call("string_of_int", E.var("i")),
                                ),
                              ),
                              None,
                              None,
                            ),
                          ),
                          let_(
                            P.var("nodes"),
                            append(
                              append(
                                append(
                                  list1(call("mk_place", E.var("pin"))),
                                  E.var("mids"),
                                ),
                                list1(call("mk_place", E.var("pout"))),
                              ),
                              E.var("trans"),
                            ),
                            let_(
                              P.var("arcs"),
                              call4(
                                "seq_place_arcs",
                                E.var("pin"),
                                E.var("prefix"),
                                E.int(1),
                                E.var("n1"),
                              ),
                              t4(
                                E.var("nodes"),
                                E.var("arcs"),
                                E.var("pin"),
                                E.var("pout"),
                              ),
                            ),
                          ),
                        ),
                      ),
                    ),
                  ),
                ),
                None,
                None,
              ),
              None,
              Some("build_sequence_place_module+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "build_parallel_place_module",
    str: {|fix build_parallel_place_module -> fun prefix -> fun n ->
  let n1 = if n < 2 then 2 else n in
  let pin = prefix ++ "_pin" in
  let pout = prefix ++ "_pout" in
  let split_t = prefix ++ "_split" in
  let join_t = prefix ++ "_join" in
  let branch_ids = map(range(1, n1 + 1), fun i -> prefix ++ "_b" ++ string_of_int(i)) in
  let branch_places = map(branch_ids, fun bid -> mk_place(bid)) in
  let branch_arcs = flat_map(branch_ids, fun bid -> [mk_arc(split_t)(bid)(1), mk_arc(bid)(join_t)(1)]) in
  let nodes = [mk_place(pin), mk_place(pout), mk_transition(split_t), mk_transition(join_t)] @ branch_places in
  let arcs = [mk_arc(pin)(split_t)(1)] @ branch_arcs @ [mk_arc(join_t)(pout)(1)] in
  (nodes, arcs, pin, pout)|},
    arg: Atom(String),
    ret: Unknown(Internal),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("build_parallel_place_module"),
            fn(
              P.var("prefix"),
              fn(
                P.var("n"),
                let_(
                  P.var("n1"),
                  if_(lt_(E.var("n"), E.int(2)), E.int(2), E.var("n")),
                  let_(
                    P.var("pin"),
                    sconcat(E.var("prefix"), E.string("_pin")),
                    let_(
                      P.var("pout"),
                      sconcat(E.var("prefix"), E.string("_pout")),
                      let_(
                        P.var("split_t"),
                        sconcat(E.var("prefix"), E.string("_split")),
                        let_(
                          P.var("join_t"),
                          sconcat(E.var("prefix"), E.string("_join")),
                          let_(
                            P.var("branch_ids"),
                            map_(
                              range_(
                                E.int(1),
                                int_add(E.var("n1"), E.int(1)),
                              ),
                              fn(
                                P.var("i"),
                                sconcat(
                                  sconcat(E.var("prefix"), E.string("_b")),
                                  call("string_of_int", E.var("i")),
                                ),
                                None,
                                None,
                              ),
                            ),
                            let_(
                              P.var("branch_places"),
                              map_(
                                E.var("branch_ids"),
                                fn(
                                  P.var("bid"),
                                  call("mk_place", E.var("bid")),
                                  None,
                                  None,
                                ),
                              ),
                              let_(
                                P.var("branch_arcs"),
                                flat_map_(
                                  E.var("branch_ids"),
                                  fn(
                                    P.var("bid"),
                                    list2(
                                      call3(
                                        "mk_arc",
                                        E.var("split_t"),
                                        E.var("bid"),
                                        E.int(1),
                                      ),
                                      call3(
                                        "mk_arc",
                                        E.var("bid"),
                                        E.var("join_t"),
                                        E.int(1),
                                      ),
                                    ),
                                    None,
                                    None,
                                  ),
                                ),
                                let_(
                                  P.var("nodes"),
                                  append(
                                    E.list_lit([
                                      call("mk_place", E.var("pin")),
                                      call("mk_place", E.var("pout")),
                                      call(
                                        "mk_transition",
                                        E.var("split_t"),
                                      ),
                                      call("mk_transition", E.var("join_t")),
                                    ]),
                                    E.var("branch_places"),
                                  ),
                                  let_(
                                    P.var("arcs"),
                                    append(
                                      append(
                                        list1(
                                          call3(
                                            "mk_arc",
                                            E.var("pin"),
                                            E.var("split_t"),
                                            E.int(1),
                                          ),
                                        ),
                                        E.var("branch_arcs"),
                                      ),
                                      list1(
                                        call3(
                                          "mk_arc",
                                          E.var("join_t"),
                                          E.var("pout"),
                                          E.int(1),
                                        ),
                                      ),
                                    ),
                                    t4(
                                      E.var("nodes"),
                                      E.var("arcs"),
                                      E.var("pin"),
                                      E.var("pout"),
                                    ),
                                  ),
                                ),
                              ),
                            ),
                          ),
                        ),
                      ),
                    ),
                  ),
                ),
                None,
                None,
              ),
              None,
              Some("build_parallel_place_module+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "build_choice_place_module",
    str: {|fix build_choice_place_module -> fun prefix -> fun n ->
  let n1 = if n < 2 then 2 else n in
  let pin = prefix ++ "_pin" in
  let pout = prefix ++ "_pout" in
  let branch_ids = map(range(1, n1 + 1), fun i -> prefix ++ "_b" ++ string_of_int(i)) in
  let choose_ids = map(range(1, n1 + 1), fun i -> prefix ++ "_choose" ++ string_of_int(i)) in
  let done_ids = map(range(1, n1 + 1), fun i -> prefix ++ "_done" ++ string_of_int(i)) in
  let nodes = [mk_place(pin), mk_place(pout)] @ map(branch_ids, fun bid -> mk_place(bid)) @ map(choose_ids, fun tid -> mk_transition(tid)) @ map(done_ids, fun tid -> mk_transition(tid)) in
  let arcs_choose = flat_map(range(1, n1 + 1), fun i ->
      let b = prefix ++ "_b" ++ string_of_int(i) in
      let c = prefix ++ "_choose" ++ string_of_int(i) in
      [mk_arc(pin)(c)(1), mk_arc(c)(b)(1)]
    ) in
  let arcs_done = flat_map(range(1, n1 + 1), fun i ->
      let b = prefix ++ "_b" ++ string_of_int(i) in
      let d = prefix ++ "_done" ++ string_of_int(i) in
      [mk_arc(b)(d)(1), mk_arc(d)(pout)(1)]
    ) in
  (nodes, arcs_choose @ arcs_done, pin, pout)|},
    arg: Atom(String),
    ret: Unknown(Internal),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("build_choice_place_module"),
            fn(
              P.var("prefix"),
              fn(
                P.var("n"),
                let_(
                  P.var("n1"),
                  if_(lt_(E.var("n"), E.int(2)), E.int(2), E.var("n")),
                  let_(
                    P.var("pin"),
                    sconcat(E.var("prefix"), E.string("_pin")),
                    let_(
                      P.var("pout"),
                      sconcat(E.var("prefix"), E.string("_pout")),
                      let_(
                        P.var("branch_ids"),
                        map_(
                          range_(
                            E.int(1),
                            int_add(E.var("n1"), E.int(1)),
                          ),
                          fn(
                            P.var("i"),
                            sconcat(
                              sconcat(E.var("prefix"), E.string("_b")),
                              call("string_of_int", E.var("i")),
                            ),
                            None,
                            None,
                          ),
                        ),
                        let_(
                          P.var("choose_ids"),
                          map_(
                            range_(
                              E.int(1),
                              int_add(E.var("n1"), E.int(1)),
                            ),
                            fn(
                              P.var("i"),
                              sconcat(
                                sconcat(
                                  E.var("prefix"),
                                  E.string("_choose"),
                                ),
                                call("string_of_int", E.var("i")),
                              ),
                              None,
                              None,
                            ),
                          ),
                          let_(
                            P.var("done_ids"),
                            map_(
                              range_(
                                E.int(1),
                                int_add(E.var("n1"), E.int(1)),
                              ),
                              fn(
                                P.var("i"),
                                sconcat(
                                  sconcat(
                                    E.var("prefix"),
                                    E.string("_done"),
                                  ),
                                  call("string_of_int", E.var("i")),
                                ),
                                None,
                                None,
                              ),
                            ),
                            let_(
                              P.var("nodes"),
                              append(
                                append(
                                  append(
                                    E.list_lit([
                                      call("mk_place", E.var("pin")),
                                      call("mk_place", E.var("pout")),
                                    ]),
                                    map_(
                                      E.var("branch_ids"),
                                      fn(
                                        P.var("bid"),
                                        call("mk_place", E.var("bid")),
                                        None,
                                        None,
                                      ),
                                    ),
                                  ),
                                  map_(
                                    E.var("choose_ids"),
                                    fn(
                                      P.var("tid"),
                                      call("mk_transition", E.var("tid")),
                                      None,
                                      None,
                                    ),
                                  ),
                                ),
                                map_(
                                  E.var("done_ids"),
                                  fn(
                                    P.var("tid"),
                                    call("mk_transition", E.var("tid")),
                                    None,
                                    None,
                                  ),
                                ),
                              ),
                              let_(
                                P.var("arcs_choose"),
                                flat_map_(
                                  range_(
                                    E.int(1),
                                    int_add(E.var("n1"), E.int(1)),
                                  ),
                                  fn(
                                    P.var("i"),
                                    let_(
                                      P.var("b"),
                                      sconcat(
                                        sconcat(
                                          E.var("prefix"),
                                          E.string("_b"),
                                        ),
                                        call("string_of_int", E.var("i")),
                                      ),
                                      let_(
                                        P.var("c"),
                                        sconcat(
                                          sconcat(
                                            E.var("prefix"),
                                            E.string("_choose"),
                                          ),
                                          call("string_of_int", E.var("i")),
                                        ),
                                        list2(
                                          call3(
                                            "mk_arc",
                                            E.var("pin"),
                                            E.var("c"),
                                            E.int(1),
                                          ),
                                          call3(
                                            "mk_arc",
                                            E.var("c"),
                                            E.var("b"),
                                            E.int(1),
                                          ),
                                        ),
                                      ),
                                    ),
                                    None,
                                    None,
                                  ),
                                ),
                                let_(
                                  P.var("arcs_done"),
                                  flat_map_(
                                    range_(
                                      E.int(1),
                                      int_add(E.var("n1"), E.int(1)),
                                    ),
                                    fn(
                                      P.var("i"),
                                      let_(
                                        P.var("b"),
                                        sconcat(
                                          sconcat(
                                            E.var("prefix"),
                                            E.string("_b"),
                                          ),
                                          call("string_of_int", E.var("i")),
                                        ),
                                        let_(
                                          P.var("d"),
                                          sconcat(
                                            sconcat(
                                              E.var("prefix"),
                                              E.string("_done"),
                                            ),
                                            call(
                                              "string_of_int",
                                              E.var("i"),
                                            ),
                                          ),
                                          list2(
                                            call3(
                                              "mk_arc",
                                              E.var("b"),
                                              E.var("d"),
                                              E.int(1),
                                            ),
                                            call3(
                                              "mk_arc",
                                              E.var("d"),
                                              E.var("pout"),
                                              E.int(1),
                                            ),
                                          ),
                                        ),
                                      ),
                                      None,
                                      None,
                                    ),
                                  ),
                                  t4(
                                    E.var("nodes"),
                                    append(
                                      E.var("arcs_choose"),
                                      E.var("arcs_done"),
                                    ),
                                    E.var("pin"),
                                    E.var("pout"),
                                  ),
                                ),
                              ),
                            ),
                          ),
                        ),
                      ),
                    ),
                  ),
                ),
                None,
                None,
              ),
              None,
              Some("build_choice_place_module+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "build_decision_free_choice_place_module",
    str: {|fix build_decision_free_choice_place_module -> fun prefix -> fun n ->
  let n1 = if n < 2 then 2 else n in
  let pin = prefix ++ "_pin" in
  let pout = prefix ++ "_pout" in
  let branch_ids = map(range(1, n1 + 1), fun i -> prefix ++ "_b" ++ string_of_int(i)) in
  let choose_ids = map(range(1, n1 + 1), fun i -> prefix ++ "_choose" ++ string_of_int(i)) in
  let done_ids = map(range(1, n1 + 1), fun i -> prefix ++ "_done" ++ string_of_int(i)) in
  let control_ids = map(range(1, n1 + 1), fun i -> prefix ++ "_ctrl" ++ string_of_int(i)) in
  let control_places = mapi(control_ids, fun (i, cid) -> if i == 0 then mk_place_tokens(cid)(1) else mk_place(cid)) in
  let nodes = [mk_place(pin), mk_place(pout)] @ map(branch_ids, fun bid -> mk_place(bid)) @ control_places @ map(choose_ids, fun tid -> mk_transition(tid)) @ map(done_ids, fun tid -> mk_transition(tid)) in
  let arcs = concat(
    mapi(range(1, n1 + 1), fun (_idx, i) ->
      let b = prefix ++ "_b" ++ string_of_int(i) in
      let c = prefix ++ "_choose" ++ string_of_int(i) in
      let d = prefix ++ "_done" ++ string_of_int(i) in
      let ctrl = prefix ++ "_ctrl" ++ string_of_int(i) in
      let next_i = if i == n1 then 1 else i + 1 in
      let next_ctrl = prefix ++ "_ctrl" ++ string_of_int(next_i) in
      [mk_arc(pin)(c)(1), mk_arc(ctrl)(c)(1), mk_arc(c)(b)(1), mk_arc(b)(d)(1), mk_arc(d)(pout)(1), mk_arc(d)(next_ctrl)(1)]
    )
  ) in
  (nodes, arcs, pin, pout)|},
    arg: Atom(String),
    ret: Unknown(Internal),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("build_decision_free_choice_place_module"),
            fn(
              P.var("prefix"),
              fn(
                P.var("n"),
                let_(
                  P.var("n1"),
                  if_(lt_(E.var("n"), E.int(2)), E.int(2), E.var("n")),
                  let_(
                    P.var("pin"),
                    sconcat(E.var("prefix"), E.string("_pin")),
                    let_(
                      P.var("pout"),
                      sconcat(E.var("prefix"), E.string("_pout")),
                      let_(
                        P.var("branch_ids"),
                        map_(
                          range_(
                            E.int(1),
                            int_add(E.var("n1"), E.int(1)),
                          ),
                          fn(
                            P.var("i"),
                            sconcat(
                              sconcat(E.var("prefix"), E.string("_b")),
                              call("string_of_int", E.var("i")),
                            ),
                            None,
                            None,
                          ),
                        ),
                        let_(
                          P.var("choose_ids"),
                          map_(
                            range_(
                              E.int(1),
                              int_add(E.var("n1"), E.int(1)),
                            ),
                            fn(
                              P.var("i"),
                              sconcat(
                                sconcat(
                                  E.var("prefix"),
                                  E.string("_choose"),
                                ),
                                call("string_of_int", E.var("i")),
                              ),
                              None,
                              None,
                            ),
                          ),
                          let_(
                            P.var("done_ids"),
                            map_(
                              range_(
                                E.int(1),
                                int_add(E.var("n1"), E.int(1)),
                              ),
                              fn(
                                P.var("i"),
                                sconcat(
                                  sconcat(
                                    E.var("prefix"),
                                    E.string("_done"),
                                  ),
                                  call("string_of_int", E.var("i")),
                                ),
                                None,
                                None,
                              ),
                            ),
                            let_(
                              P.var("control_ids"),
                              map_(
                                range_(
                                  E.int(1),
                                  int_add(E.var("n1"), E.int(1)),
                                ),
                                fn(
                                  P.var("i"),
                                  sconcat(
                                    sconcat(
                                      E.var("prefix"),
                                      E.string("_ctrl"),
                                    ),
                                    call("string_of_int", E.var("i")),
                                  ),
                                  None,
                                  None,
                                ),
                              ),
                              let_(
                                P.var("control_places"),
                                mapi_(
                                  E.var("control_ids"),
                                  fn(
                                    P.tuple([P.var("i"), P.var("cid")]),
                                    if_(
                                      eq_(E.var("i"), E.int(0)),
                                      call2(
                                        "mk_place_tokens",
                                        E.var("cid"),
                                        E.int(1),
                                      ),
                                      call("mk_place", E.var("cid")),
                                    ),
                                    None,
                                    None,
                                  ),
                                ),
                                let_(
                                  P.var("nodes"),
                                  append(
                                    append(
                                      append(
                                        append(
                                          E.list_lit([
                                            call("mk_place", E.var("pin")),
                                            call("mk_place", E.var("pout")),
                                          ]),
                                          map_(
                                            E.var("branch_ids"),
                                            fn(
                                              P.var("bid"),
                                              call("mk_place", E.var("bid")),
                                              None,
                                              None,
                                            ),
                                          ),
                                        ),
                                        E.var("control_places"),
                                      ),
                                      map_(
                                        E.var("choose_ids"),
                                        fn(
                                          P.var("tid"),
                                          call(
                                            "mk_transition",
                                            E.var("tid"),
                                          ),
                                          None,
                                          None,
                                        ),
                                      ),
                                    ),
                                    map_(
                                      E.var("done_ids"),
                                      fn(
                                        P.var("tid"),
                                        call("mk_transition", E.var("tid")),
                                        None,
                                        None,
                                      ),
                                    ),
                                  ),
                                  let_(
                                    P.var("arcs"),
                                    concat_(
                                      mapi_(
                                        range_(
                                          E.int(1),
                                          int_add(E.var("n1"), E.int(1)),
                                        ),
                                        fn(
                                          P.tuple([
                                            P.var("_idx"),
                                            P.var("i"),
                                          ]),
                                          let_(
                                            P.var("b"),
                                            sconcat(
                                              sconcat(
                                                E.var("prefix"),
                                                E.string("_b"),
                                              ),
                                              call(
                                                "string_of_int",
                                                E.var("i"),
                                              ),
                                            ),
                                            let_(
                                              P.var("c"),
                                              sconcat(
                                                sconcat(
                                                  E.var("prefix"),
                                                  E.string("_choose"),
                                                ),
                                                call(
                                                  "string_of_int",
                                                  E.var("i"),
                                                ),
                                              ),
                                              let_(
                                                P.var("d"),
                                                sconcat(
                                                  sconcat(
                                                    E.var("prefix"),
                                                    E.string("_done"),
                                                  ),
                                                  call(
                                                    "string_of_int",
                                                    E.var("i"),
                                                  ),
                                                ),
                                                let_(
                                                  P.var("ctrl"),
                                                  sconcat(
                                                    sconcat(
                                                      E.var("prefix"),
                                                      E.string("_ctrl"),
                                                    ),
                                                    call(
                                                      "string_of_int",
                                                      E.var("i"),
                                                    ),
                                                  ),
                                                  let_(
                                                    P.var("next_i"),
                                                    if_(
                                                      eq_(
                                                        E.var("i"),
                                                        E.var("n1"),
                                                      ),
                                                      E.int(1),
                                                      int_add(
                                                        E.var("i"),
                                                        E.int(1),
                                                      ),
                                                    ),
                                                    let_(
                                                      P.var("next_ctrl"),
                                                      sconcat(
                                                        sconcat(
                                                          E.var("prefix"),
                                                          E.string("_ctrl"),
                                                        ),
                                                        call(
                                                          "string_of_int",
                                                          E.var("next_i"),
                                                        ),
                                                      ),
                                                      E.list_lit([
                                                        call3(
                                                          "mk_arc",
                                                          E.var("pin"),
                                                          E.var("c"),
                                                          E.int(1),
                                                        ),
                                                        call3(
                                                          "mk_arc",
                                                          E.var("ctrl"),
                                                          E.var("c"),
                                                          E.int(1),
                                                        ),
                                                        call3(
                                                          "mk_arc",
                                                          E.var("c"),
                                                          E.var("b"),
                                                          E.int(1),
                                                        ),
                                                        call3(
                                                          "mk_arc",
                                                          E.var("b"),
                                                          E.var("d"),
                                                          E.int(1),
                                                        ),
                                                        call3(
                                                          "mk_arc",
                                                          E.var("d"),
                                                          E.var("pout"),
                                                          E.int(1),
                                                        ),
                                                        call3(
                                                          "mk_arc",
                                                          E.var("d"),
                                                          E.var("next_ctrl"),
                                                          E.int(1),
                                                        ),
                                                      ]),
                                                    ),
                                                  ),
                                                ),
                                              ),
                                            ),
                                          ),
                                          None,
                                          None,
                                        ),
                                      ),
                                    ),
                                    t4(
                                      E.var("nodes"),
                                      E.var("arcs"),
                                      E.var("pin"),
                                      E.var("pout"),
                                    ),
                                  ),
                                ),
                              ),
                            ),
                          ),
                        ),
                      ),
                    ),
                  ),
                ),
                None,
                None,
              ),
              None,
              Some("build_decision_free_choice_place_module+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "refine_place_with_module",
    str: {|fix refine_place_with_module -> fun net -> fun old_place -> fun module_def ->
  let (module_nodes, module_arcs, pin, pout) = module_def in
  let nodes = get_nodes(net) in
  let arcs = get_arcs(net) in
  let kept_nodes = filter(nodes, fun node -> get_id(node) != old_place) in
  let rewired_arcs =
    map(
      arcs,
      fun arc ->
        let src = arc_source(arc) in
        let tgt = arc_target(arc) in
        if tgt == old_place
        then arc_set_target(arc)(pin)
        else if src == old_place
        then arc_set_source(arc)(pout)
        else arc
    )
  in
  let net1 = set_nodes(net)(kept_nodes @ module_nodes) in
  set_arcs(net1)(rewired_arcs @ module_arcs)|},
    arg: json_t,
    ret: Unknown(Internal),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("refine_place_with_module"),
            fn(
              P.var("net"),
              fn(
                P.var("old_place"),
                fn(
                  P.var("module_def"),
                  let_(
                    P.tuple([
                      P.var("module_nodes"),
                      P.var("module_arcs"),
                      P.var("pin"),
                      P.var("pout"),
                    ]),
                    E.var("module_def"),
                    let_(
                      P.var("nodes"),
                      call("get_nodes", E.var("net")),
                      let_(
                        P.var("arcs"),
                        call("get_arcs", E.var("net")),
                        let_(
                          P.var("kept_nodes"),
                          filter_(
                            E.var("nodes"),
                            fn(
                              P.var("node"),
                              E.bin_op(
                                Poly(NotEquals),
                                call("get_id", E.var("node")),
                                E.var("old_place"),
                              ),
                              None,
                              None,
                            ),
                          ),
                          let_(
                            P.var("rewired_arcs"),
                            map_(
                              E.var("arcs"),
                              fn(
                                P.var("arc"),
                                let_(
                                  P.var("src"),
                                  call("arc_source", E.var("arc")),
                                  let_(
                                    P.var("tgt"),
                                    call("arc_target", E.var("arc")),
                                    if_(
                                      eq_(E.var("tgt"), E.var("old_place")),
                                      call2(
                                        "arc_set_target",
                                        E.var("arc"),
                                        E.var("pin"),
                                      ),
                                      if_(
                                        eq_(
                                          E.var("src"),
                                          E.var("old_place"),
                                        ),
                                        call2(
                                          "arc_set_source",
                                          E.var("arc"),
                                          E.var("pout"),
                                        ),
                                        E.var("arc"),
                                      ),
                                    ),
                                  ),
                                ),
                                None,
                                None,
                              ),
                            ),
                            let_(
                              P.var("net1"),
                              call2(
                                "set_nodes",
                                E.var("net"),
                                append(
                                  E.var("kept_nodes"),
                                  E.var("module_nodes"),
                                ),
                              ),
                              call2(
                                "set_arcs",
                                E.var("net1"),
                                append(
                                  E.var("rewired_arcs"),
                                  E.var("module_arcs"),
                                ),
                              ),
                            ),
                          ),
                        ),
                      ),
                    ),
                  ),
                  None,
                  None,
                ),
                None,
                None,
              ),
              None,
              Some("refine_place_with_module+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "refine_place_sequence",
    str: {|fix refine_place_sequence -> fun net -> fun place_id -> fun prefix -> fun n ->
  refine_place_with_module(net)(place_id)(build_sequence_place_module(prefix)(n))|},
    arg: json_t,
    ret: Unknown(Internal),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("refine_place_sequence"),
            fn(
              P.var("net"),
              fn(
                P.var("place_id"),
                fn(
                  P.var("prefix"),
                  fn(
                    P.var("n"),
                    call3(
                      "refine_place_with_module",
                      E.var("net"),
                      E.var("place_id"),
                      call2(
                        "build_sequence_place_module",
                        E.var("prefix"),
                        E.var("n"),
                      ),
                    ),
                    None,
                    None,
                  ),
                  None,
                  None,
                ),
                None,
                None,
              ),
              None,
              Some("refine_place_sequence+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "refine_place_parallel",
    str: {|fix refine_place_parallel -> fun net -> fun place_id -> fun prefix -> fun n ->
  refine_place_with_module(net)(place_id)(build_parallel_place_module(prefix)(n))|},
    arg: json_t,
    ret: Unknown(Internal),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("refine_place_parallel"),
            fn(
              P.var("net"),
              fn(
                P.var("place_id"),
                fn(
                  P.var("prefix"),
                  fn(
                    P.var("n"),
                    call3(
                      "refine_place_with_module",
                      E.var("net"),
                      E.var("place_id"),
                      call2(
                        "build_parallel_place_module",
                        E.var("prefix"),
                        E.var("n"),
                      ),
                    ),
                    None,
                    None,
                  ),
                  None,
                  None,
                ),
                None,
                None,
              ),
              None,
              Some("refine_place_parallel+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "refine_place_choice",
    str: {|fix refine_place_choice -> fun net -> fun place_id -> fun prefix -> fun n ->
  refine_place_with_module(net)(place_id)(build_choice_place_module(prefix)(n))|},
    arg: json_t,
    ret: Unknown(Internal),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("refine_place_choice"),
            fn(
              P.var("net"),
              fn(
                P.var("place_id"),
                fn(
                  P.var("prefix"),
                  fn(
                    P.var("n"),
                    call3(
                      "refine_place_with_module",
                      E.var("net"),
                      E.var("place_id"),
                      call2(
                        "build_choice_place_module",
                        E.var("prefix"),
                        E.var("n"),
                      ),
                    ),
                    None,
                    None,
                  ),
                  None,
                  None,
                ),
                None,
                None,
              ),
              None,
              Some("refine_place_choice+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "refine_place_decision_free_choice",
    str: {|fix refine_place_decision_free_choice -> fun net -> fun place_id -> fun prefix -> fun n ->
  refine_place_with_module(net)(place_id)(build_decision_free_choice_place_module(prefix)(n))|},
    arg: json_t,
    ret: Unknown(Internal),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("refine_place_decision_free_choice"),
            fn(
              P.var("net"),
              fn(
                P.var("place_id"),
                fn(
                  P.var("prefix"),
                  fn(
                    P.var("n"),
                    call3(
                      "refine_place_with_module",
                      E.var("net"),
                      E.var("place_id"),
                      call2(
                        "build_decision_free_choice_place_module",
                        E.var("prefix"),
                        E.var("n"),
                      ),
                    ),
                    None,
                    None,
                  ),
                  None,
                  None,
                ),
                None,
                None,
              ),
              None,
              Some("refine_place_decision_free_choice+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "add_resource_place_with_pairs",
    str: {|fix add_resource_place_with_pairs -> fun net -> fun resource_place_id -> fun initial_tokens -> fun pairs ->
  let nodes = get_nodes(net) in
  let arcs = get_arcs(net) in
  let resource_node = mk_place_tokens(resource_place_id)(initial_tokens) in
  let lock_arcs = flat_map(pairs, fun (ta, tb) -> [mk_arc(resource_place_id)(ta)(1), mk_arc(tb)(resource_place_id)(1)]) in
  let net1 = set_nodes(net)(nodes @ [resource_node]) in
  set_arcs(net1)(arcs @ lock_arcs)|},
    arg: json_t,
    ret: Unknown(Internal),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("add_resource_place_with_pairs"),
            fn(
              P.var("net"),
              fn(
                P.var("resource_place_id"),
                fn(
                  P.var("initial_tokens"),
                  fn(
                    P.var("pairs"),
                    let_(
                      P.var("nodes"),
                      call("get_nodes", E.var("net")),
                      let_(
                        P.var("arcs"),
                        call("get_arcs", E.var("net")),
                        let_(
                          P.var("resource_node"),
                          call2(
                            "mk_place_tokens",
                            E.var("resource_place_id"),
                            E.var("initial_tokens"),
                          ),
                          let_(
                            P.var("lock_arcs"),
                            flat_map_(
                              E.var("pairs"),
                              fn(
                                P.tuple([P.var("ta"), P.var("tb")]),
                                list2(
                                  call3(
                                    "mk_arc",
                                    E.var("resource_place_id"),
                                    E.var("ta"),
                                    E.int(1),
                                  ),
                                  call3(
                                    "mk_arc",
                                    E.var("tb"),
                                    E.var("resource_place_id"),
                                    E.int(1),
                                  ),
                                ),
                                None,
                                None,
                              ),
                            ),
                            let_(
                              P.var("net1"),
                              call2(
                                "set_nodes",
                                E.var("net"),
                                append(
                                  E.var("nodes"),
                                  list1(E.var("resource_node")),
                                ),
                              ),
                              call2(
                                "set_arcs",
                                E.var("net1"),
                                append(E.var("arcs"), E.var("lock_arcs")),
                              ),
                            ),
                          ),
                        ),
                      ),
                    ),
                    None,
                    None,
                  ),
                  None,
                  None,
                ),
                None,
                None,
              ),
              None,
              Some("add_resource_place_with_pairs+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "add_nonshared_resource",
    str: {|fix add_nonshared_resource -> fun net -> fun resource_place_id -> fun initial_tokens -> fun ta -> fun tb ->
  add_resource_place_with_pairs(net)(resource_place_id)(initial_tokens)([(ta, tb)])|},
    arg: json_t,
    ret: Unknown(Internal),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("add_nonshared_resource"),
            fn(
              P.var("net"),
              fn(
                P.var("resource_place_id"),
                fn(
                  P.var("initial_tokens"),
                  fn(
                    P.var("ta"),
                    fn(
                      P.var("tb"),
                      call4(
                        "add_resource_place_with_pairs",
                        E.var("net"),
                        E.var("resource_place_id"),
                        E.var("initial_tokens"),
                        list1(t2(E.var("ta"), E.var("tb"))),
                      ),
                      None,
                      None,
                    ),
                    None,
                    None,
                  ),
                  None,
                  None,
                ),
                None,
                None,
              ),
              None,
              Some("add_nonshared_resource+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "add_shared_parallel_resource",
    str: {|fix add_shared_parallel_resource -> fun net -> fun resource_place_id -> fun initial_tokens -> fun pairs ->
  add_resource_place_with_pairs(net)(resource_place_id)(initial_tokens)(pairs)|},
    arg: json_t,
    ret: Unknown(Internal),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("add_shared_parallel_resource"),
            fn(
              P.var("net"),
              fn(
                P.var("resource_place_id"),
                fn(
                  P.var("initial_tokens"),
                  fn(
                    P.var("pairs"),
                    call4(
                      "add_resource_place_with_pairs",
                      E.var("net"),
                      E.var("resource_place_id"),
                      E.var("initial_tokens"),
                      E.var("pairs"),
                    ),
                    None,
                    None,
                  ),
                  None,
                  None,
                ),
                None,
                None,
              ),
              None,
              Some("add_shared_parallel_resource+"),
            ),
            None,
          )
        )
      );
    },
  },
  {
    name: "add_shared_sequential_resource",
    str: {|fix add_shared_sequential_resource -> fun net -> fun resource_place_id -> fun initial_tokens -> fun groups ->
  let net0 = add_resource_place_with_pairs(net)(resource_place_id)(initial_tokens)(concat(groups)) in
  let l = length(groups) in
  if l <= 1
  then net0
  else
    let stage_ids = map(range(1, l + 1), fun i -> resource_place_id ++ "_stage_" ++ string_of_int(i)) in
    let stage_nodes = mapi(stage_ids, fun (i, sid) -> if i == 0 then mk_place_tokens(sid)(1) else mk_place(sid)) in
    let stage_arcs =
      concat(
        mapi(
          groups,
          fun (i, pairs) ->
            let stage_in = resource_place_id ++ "_stage_" ++ string_of_int(i + 1) in
            let stage_out =
              if i + 1 == l
              then resource_place_id ++ "_stage_1"
              else resource_place_id ++ "_stage_" ++ string_of_int(i + 2)
            in
            flat_map(
              pairs,
              fun (ta, tb) ->
                [mk_arc(stage_in)(ta)(1), mk_arc(tb)(stage_out)(1)]
            )
        )
      )
    in
    let nodes = get_nodes(net0) in
    let arcs = get_arcs(net0) in
    let net1 = set_nodes(net0)(nodes @ stage_nodes) in
    set_arcs(net1)(arcs @ stage_arcs)|},
    arg: json_t,
    ret: Unknown(Internal),
    imp: {
      Fresh.(
        Exp.(
          fix_f(
            P.var("add_shared_sequential_resource"),
            fn(
              P.var("net"),
              fn(
                P.var("resource_place_id"),
                fn(
                  P.var("initial_tokens"),
                  fn(
                    P.var("groups"),
                    let_(
                      P.var("net0"),
                      call4(
                        "add_resource_place_with_pairs",
                        E.var("net"),
                        E.var("resource_place_id"),
                        E.var("initial_tokens"),
                        concat_(E.var("groups")),
                      ),
                      let_(
                        P.var("l"),
                        length_(E.var("groups")),
                        if_(
                          le_(E.var("l"), E.int(1)),
                          E.var("net0"),
                          let_(
                            P.var("stage_ids"),
                            map_(
                              range_(
                                E.int(1),
                                int_add(E.var("l"), E.int(1)),
                              ),
                              fn(
                                P.var("i"),
                                sconcat(
                                  sconcat(
                                    E.var("resource_place_id"),
                                    E.string("_stage_"),
                                  ),
                                  call("string_of_int", E.var("i")),
                                ),
                                None,
                                None,
                              ),
                            ),
                            let_(
                              P.var("stage_nodes"),
                              mapi_(
                                E.var("stage_ids"),
                                fn(
                                  P.tuple([P.var("i"), P.var("sid")]),
                                  if_(
                                    eq_(E.var("i"), E.int(0)),
                                    call2(
                                      "mk_place_tokens",
                                      E.var("sid"),
                                      E.int(1),
                                    ),
                                    call("mk_place", E.var("sid")),
                                  ),
                                  None,
                                  None,
                                ),
                              ),
                              let_(
                                P.var("stage_arcs"),
                                concat_(
                                  mapi_(
                                    E.var("groups"),
                                    fn(
                                      P.tuple([P.var("i"), P.var("pairs")]),
                                      let_(
                                        P.var("stage_in"),
                                        sconcat(
                                          sconcat(
                                            E.var("resource_place_id"),
                                            E.string("_stage_"),
                                          ),
                                          call(
                                            "string_of_int",
                                            int_add(E.var("i"), E.int(1)),
                                          ),
                                        ),
                                        let_(
                                          P.var("stage_out"),
                                          if_(
                                            eq_(
                                              int_add(E.var("i"), E.int(1)),
                                              E.var("l"),
                                            ),
                                            sconcat(
                                              E.var("resource_place_id"),
                                              E.string("_stage_1"),
                                            ),
                                            sconcat(
                                              sconcat(
                                                E.var("resource_place_id"),
                                                E.string("_stage_"),
                                              ),
                                              call(
                                                "string_of_int",
                                                int_add(
                                                  E.var("i"),
                                                  E.int(2),
                                                ),
                                              ),
                                            ),
                                          ),
                                          flat_map_(
                                            E.var("pairs"),
                                            fn(
                                              P.tuple([
                                                P.var("ta"),
                                                P.var("tb"),
                                              ]),
                                              list2(
                                                call3(
                                                  "mk_arc",
                                                  E.var("stage_in"),
                                                  E.var("ta"),
                                                  E.int(1),
                                                ),
                                                call3(
                                                  "mk_arc",
                                                  E.var("tb"),
                                                  E.var("stage_out"),
                                                  E.int(1),
                                                ),
                                              ),
                                              None,
                                              None,
                                            ),
                                          ),
                                        ),
                                      ),
                                      None,
                                      None,
                                    ),
                                  ),
                                ),
                                let_(
                                  P.var("nodes"),
                                  call("get_nodes", E.var("net0")),
                                  let_(
                                    P.var("arcs"),
                                    call("get_arcs", E.var("net0")),
                                    let_(
                                      P.var("net1"),
                                      call2(
                                        "set_nodes",
                                        E.var("net0"),
                                        append(
                                          E.var("nodes"),
                                          E.var("stage_nodes"),
                                        ),
                                      ),
                                      call2(
                                        "set_arcs",
                                        E.var("net1"),
                                        append(
                                          E.var("arcs"),
                                          E.var("stage_arcs"),
                                        ),
                                      ),
                                    ),
                                  ),
                                ),
                              ),
                            ),
                          ),
                        ),
                      ),
                    ),
                    None,
                    None,
                  ),
                  None,
                  None,
                ),
                None,
                None,
              ),
              None,
              Some("add_shared_sequential_resource+"),
            ),
            None,
          )
        )
      );
    },
  },
];
