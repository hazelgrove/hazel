open Alcotest;
open Haz3lcore;
open Util;

module SystemOne = OpenRouter.SystemOne;

let mk_zipper = (code: string): Zipper.t =>
  switch (Parser.to_zipper(~root=Exp, code)) {
  | Some(z) => z
  | None => Alcotest.fail("Failed to parse: " ++ code)
  };

/* Literal match: StringUtil.plain_search takes a regexp, and code has parens. */
let contains = (needle: string, haystack: string): bool =>
  switch (Str.search_forward(Str.regexp_string(needle), haystack, 0)) {
  | _ => true
  | exception Not_found => false
  };

let node = (~code="x", path: string): JevNav.node => {
  path,
  name: path,
  typ: "",
  code,
  refs: [],
  used_by: [],
};

let is_failed = (reply: SystemOne.reply): bool =>
  switch (reply) {
  | Failed(_) => true
  | Answers(_) => false
  };

let question: SystemOne.noul_question = {
  id: "foo/bar",
  instructions: "i",
  criteria_true: "t",
  criteria_false: "f",
};

/* [helper] is used only inside [outer/inner]'s definition, so the edge must
   attribute to the nested binding, not to [outer]. */
let nested_program = {|let helper = fun x -> x + 1 # bump # in
let outer = fun n ->
  let inner = helper(n) in
  inner * 2
in
outer(3)|};

let find_node = (nodes: list(JevNav.node), path: string): JevNav.node =>
  switch (List.find_opt((n: JevNav.node) => n.path == path, nodes)) {
  | Some(n) => n
  | None => Alcotest.fail("no node " ++ path)
  };

/* Synchronous stand-in for Jev: canned probabilities by path, 0.0 otherwise. */
let fake_decide =
    (~calls: ref(int), p_of: list((string, float))): JevNav.decide =>
  (~state as _, ~questions, ~handler) => {
    calls := calls^ + 1;
    handler(
      Answers(
        List.map(
          (q: SystemOne.noul_question): SystemOne.answer =>
            {
              id: q.id,
              p_yes:
                List.assoc_opt(q.id, p_of) |> Option.value(~default=0.0),
            },
          questions,
        ),
        {
          input_tokens: 10,
          cost_usd: Some(0.5),
        },
      ),
    );
  };

let run_select =
    (
      ~max_tokens=8000,
      ~intent="double the bumped value",
      decide: JevNav.decide,
    )
    : JevNav.selection => {
  let result = ref(None);
  JevNav.select(
    ~decide,
    ~max_tokens,
    ~intent,
    ~on_done=s => result := Some(s),
    mk_zipper(nested_program),
  );
  switch (result^) {
  | Some(s) => s
  | None => Alcotest.fail("select did not call on_done")
  };
};

let system_one_tests = (
  "JevNav.SystemOne",
  [
    test_case("json_of_request shape", `Quick, () =>
      check(
        string,
        "body",
        {|{"model":"m","state":{"intent":"x"},"questions":{"foo/bar":{"type":"noul","instructions":"i","criteria":{"true":"t","false":"f"}}}}|},
        API.Json.to_string(
          SystemOne.json_of_request(
            ~model_id="m",
            ~state=`Assoc([("intent", `String("x"))]),
            [question],
          ),
        ),
      )
    ),
    test_case(
      "reply_of_json success",
      `Quick,
      () => {
        let json =
          API.Json.from_string(
            {|{"model":"m","answers":{"a":{"type":"noul","noul":0.9},"b":{"type":"noul","noul":0}},
             "usage":{"input_tokens":42,"output_tokens":null,"cost":"0.001"}}|},
          );
        switch (SystemOne.reply_of_json(~ids=["a", "b"], Some(json))) {
        | Answers(answers, usage) =>
          check(
            list(pair(string, float(1e-9))),
            "answers",
            [("a", 0.9), ("b", 0.0)],
            List.map((a: SystemOne.answer) => (a.id, a.p_yes), answers),
          );
          check(int, "input tokens", 42, usage.input_tokens);
          check(option(float(1e-9)), "cost", Some(0.001), usage.cost_usd);
        | Failed(_, msg) => Alcotest.fail(msg)
        };
      },
    ),
    test_case(
      "reply_of_json missing answer fails",
      `Quick,
      () => {
        let json =
          API.Json.from_string(
            {|{"answers":{"a":{"type":"noul","noul":0.9}}}|},
          );
        check(
          bool,
          "failed",
          true,
          is_failed(SystemOne.reply_of_json(~ids=["a", "b"], Some(json))),
        );
      },
    ),
    test_case(
      "reply_of_json HTTP error",
      `Quick,
      () => {
        let json =
          API.Json.from_string(
            {|{"error":{"code":401,"message":"No auth credentials found"}}|},
          );
        switch (SystemOne.reply_of_json(~ids=["a"], Some(json))) {
        | Failed(code, msg) =>
          check(int, "code", 401, code);
          check(string, "message", "No auth credentials found", msg);
        | Answers(_) => Alcotest.fail("expected Failed")
        };
        check(
          bool,
          "no response fails",
          true,
          is_failed(SystemOne.reply_of_json(~ids=["a"], None)),
        );
      },
    ),
  ],
);

let nodes_tests = (
  "JevNav.nodes",
  [
    test_case(
      "nested program: paths, folding, refs, used_by",
      `Quick,
      () => {
        let nodes = JevNav.nodes_of(mk_zipper(nested_program));
        check(
          list(string),
          "paths in program order",
          ["helper", "outer", "outer/inner"],
          List.map((n: JevNav.node) => n.path, nodes),
        );
        let helper = find_node(nodes, "helper");
        let outer = find_node(nodes, "outer");
        let inner = find_node(nodes, "outer/inner");
        check(bool, "outer folds inner", true, contains("⋱", outer.code));
        check(
          bool,
          "outer hides inner's code",
          false,
          contains("helper(n)", outer.code),
        );
        check(
          bool,
          "inner shows own code",
          true,
          contains("helper(n)", inner.code),
        );
        check(
          bool,
          "comment stripped",
          false,
          contains("bump", helper.code),
        );
        check(
          bool,
          "body excluded",
          false,
          contains("outer(3)", outer.code),
        );
        /* Unannotated parameter: statics only knows the result type. */
        check(string, "helper type", "? -> Int", helper.typ);
        check(list(string), "inner refs", ["helper"], inner.refs);
        check(
          list(string),
          "helper used_by",
          ["outer/inner"],
          helper.used_by,
        );
        check(list(string), "outer refs", ["outer/inner"], outer.refs);
        check(list(string), "inner used_by", ["outer"], inner.used_by);
      },
    ),
    test_case(
      "shadowed bindings get distinct #k paths the node map resolves",
      `Quick,
      () => {
        let z = mk_zipper("let n = 1 in let n = n + 1 in n");
        let paths =
          List.map((n: JevNav.node) => n.path, JevNav.nodes_of(z));
        check(list(string), "paths", ["n#1", "n#2"], paths);
      },
    ),
    test_case("empty program has no nodes", `Quick, () =>
      check(
        int,
        "count",
        0,
        List.length(JevNav.nodes_of(mk_zipper("1 + 2"))),
      )
    ),
    test_case(
      "fn-sugar and type alias bindings",
      `Quick,
      () => {
        let nodes =
          JevNav.nodes_of(
            mk_zipper(
              "type T = Int in let g = fun x -> x in let f(y) = g(y) in f(1)",
            ),
          );
        check(
          list(string),
          "paths",
          ["T", "g", "f"],
          List.map((n: JevNav.node) => n.path, nodes),
        );
        check(
          list(string),
          "g used_by",
          ["f"],
          find_node(nodes, "g").used_by,
        );
        check(
          bool,
          "f code",
          true,
          contains("g(y)", find_node(nodes, "f").code),
        );
      },
    ),
  ],
);

let pure_tests = (
  "JevNav.pure",
  [
    test_case(
      "question names the path in every field",
      `Quick,
      () => {
        let q = JevNav.question_of(node("foo/bar"));
        check(string, "id", "foo/bar", q.id);
        List.iter(
          text => check(bool, text, true, contains("`foo/bar`", text)),
          [q.instructions, q.criteria_true, q.criteria_false],
        );
      },
    ),
    test_case(
      "question asks for strict need",
      `Quick,
      () => {
        let q = JevNav.question_of(node("foo/bar"));
        check(
          string,
          "instructions",
          "Must the agent read or change binding `foo/bar` to carry out the intent?",
          q.instructions,
        );
        check(
          bool,
          "no 'relevant'",
          false,
          contains("relevant", q.instructions),
        );
        check(
          bool,
          "false side is positive",
          true,
          contains("can stay folded", q.criteria_false),
        );
      },
    ),
    test_case(
      "mentioned_paths matches whole words only",
      `Quick,
      () => {
        let nodes =
          List.map(
            node,
            [
              "Fuel",
              "Fuel/fuel_cost",
              "Trip/cost",
              "Trip/total",
              "Bill/total",
            ],
          );
        let mentioned = intent => JevNav.mentioned_paths(~intent, nodes);
        check(
          list(string),
          "dotted",
          ["Fuel/fuel_cost"],
          mentioned("fix Fuel.fuel_cost."),
        );
        check(
          list(string),
          "slash",
          ["Trip/total"],
          mentioned("change Trip/total"),
        );
        check(
          list(string),
          "unique suffix",
          ["Trip/cost"],
          mentioned("the cost is off"),
        );
        check(
          list(string),
          "substring is not a mention",
          ["Fuel/fuel_cost"],
          mentioned("fuel_cost doubles"),
        );
        check(
          list(string),
          "shared suffix needs the path",
          [],
          mentioned("total is wrong"),
        );
      },
    ),
    test_case("state_of uses Jev's field names", `Quick, () =>
      check(
        string,
        "state",
        {|{"intent":"go","bindings":{"a":{"name":"a","type":"","code":"x","refs":[],"used_by":[]}}}|},
        API.Json.to_string(JevNav.state_of(~intent="go", [node("a")])),
      )
    ),
    test_case(
      "batches pack greedily and split",
      `Quick,
      () => {
        let nodes = List.map(node, ["a", "b", "c"]);
        let one = JevNav.tokens_of(node("a"));
        check(
          int,
          "all fit",
          1,
          List.length(JevNav.batches(~max_tokens=1000, nodes)),
        );
        check(
          list(list(string)),
          "two per batch",
          [["a", "b"], ["c"]],
          JevNav.batches(~max_tokens=2 * one, nodes)
          |> List.map(List.map((n: JevNav.node) => n.path)),
        );
      },
    ),
    test_case(
      "oversize node gets own batch with first line",
      `Quick,
      () => {
        let big = node(~code="head\n" ++ String.make(400, 'x'), "big");
        let batched =
          JevNav.batches(~max_tokens=50, [node("a"), big, node("b")]);
        check(
          list(list(string)),
          "batches",
          [["a"], ["big"], ["b"]],
          List.map(List.map((n: JevNav.node) => n.path), batched),
        );
        check(
          string,
          "truncated",
          "head",
          List.nth(batched, 1) |> List.hd |> (n => n.code),
        );
      },
    ),
    test_case(
      "jev_says_yes takes Jev's own yes/no",
      `Quick,
      () => {
        let answers: list(SystemOne.answer) =
          List.map(
            ((id, p_yes)): SystemOne.answer =>
              {
                id,
                p_yes,
              },
            [
              ("yes", 0.9),
              ("lean_yes", 0.51),
              ("even", 0.5),
              ("no", 0.1),
            ],
          );
        check(
          list(string),
          "yes",
          ["yes", "lean_yes"],
          JevNav.jev_says_yes(answers),
        );
      },
    ),
    test_case(
      "close_ancestors adds prefixes, dedupes, parents first", `Quick, () =>
      check(
        list(string),
        "closed",
        ["foo", "foo/bar", "foo/bar/baz", "goo"],
        JevNav.close_ancestors(["foo/bar/baz", "foo", "goo"]),
      )
    ),
  ],
);

let select_tests = (
  "JevNav.select",
  [
    test_case(
      "fake decide opens yes plus ancestors",
      `Quick,
      () => {
        ignore(JevNav.Log.drain());
        let calls = ref(0);
        let s =
          run_select(
            fake_decide(~calls, [("outer/inner", 0.9), ("helper", 0.4)]),
          );
        check(list(string), "open", ["outer", "outer/inner"], s.open_paths);
        check(list(string), "yes", ["outer/inner"], s.metrics.yes);
        check(list(string), "closure", ["outer"], s.metrics.closure_added);
        check(int, "requests", 1, s.metrics.requests);
        check(int, "questions", 3, s.metrics.questions);
        check(int, "input tokens", 10, s.metrics.input_tokens);
        check(bool, "failed", false, s.metrics.failed);
        check(int, "logged once", 1, List.length(JevNav.Log.drain()));
        check(int, "drain clears", 0, List.length(JevNav.Log.drain()));
      },
    ),
    test_case(
      "mentioned bindings open without asking Jev",
      `Quick,
      () => {
        let calls = ref(0);
        let questioned = ref([]);
        let decide: JevNav.decide =
          (~state, ~questions, ~handler) => {
            questioned :=
              questioned^
              @ List.map((q: SystemOne.noul_question) => q.id, questions);
            fake_decide(~calls, [], ~state, ~questions, ~handler);
          };
        let s = run_select(~intent="make outer/inner use helper", decide);
        check(
          list(string),
          "open",
          ["outer", "outer/inner", "helper"] |> List.sort(compare),
          s.open_paths |> List.sort(compare),
        );
        check(list(string), "only the rest asked", ["outer"], questioned^);
        ignore(JevNav.Log.drain());
        let none = ref(0);
        let all =
          run_select(
            ~intent="helper outer inner",
            fake_decide(~calls=none, []),
          );
        check(int, "all mentioned: no request", 0, none^);
        check(int, "requests", 0, all.metrics.requests);
        check(int, "opened", 3, List.length(all.open_paths));
        ignore(JevNav.Log.drain());
      },
    ),
    test_case(
      "one request per batch, usage summed",
      `Quick,
      () => {
        let calls = ref(0);
        let s = run_select(~max_tokens=1, fake_decide(~calls, []));
        check(int, "decide calls", 3, calls^);
        check(int, "requests", 3, s.metrics.requests);
        check(int, "input tokens", 30, s.metrics.input_tokens);
        check(float(1e-9), "cost", 1.5, s.metrics.cost_usd);
        ignore(JevNav.Log.drain());
      },
    ),
    test_case(
      "failing decide fails safe",
      `Quick,
      () => {
        let failing: JevNav.decide =
          (~state as _, ~questions as _, ~handler) =>
            handler(Failed(500, "boom"));
        let s = run_select(failing);
        check(bool, "failed", true, s.metrics.failed);
        check(list(string), "open", [], s.open_paths);
        ignore(JevNav.Log.drain());
      },
    ),
  ],
);

let tests = [system_one_tests, nodes_tests, pure_tests, select_tests];
