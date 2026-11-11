open Util;
open Language;
open Haz3lcore;
open Js_of_ocaml;

/* Runs the agent's hole fill over benchmark.json, one theorem at a time,
 * with the model behind a JS bridge (run.js), and reports per theorem:
 * whether the reference proof checks, every exchange with the model, the
 * loop's statuses, and the checker's verdict on what the fill wrote.
 *
 * An intervention changes what the model is told, by rewriting the
 * messages on their way to the model, so it can be measured without
 * changing the product. */

module Fill = Web.AgentFill;

let core = Web.Theorems.with_proof_settings(CoreSettings.on);

let evaluate = (program: Exp.t): Web.EvalResult.Model.t => {
  let statics =
    CachedStatics.init_from_term(
      ~settings=CoreSettings.on,
      ~is_dynamic_term=false,
      program,
    );
  Web.EvalResult.Update.calculate(
    ~settings=CoreSettings.on,
    ~queue_worker=None,
    ~is_edited=true,
    statics,
    Web.EvalResult.Model.init,
  );
};

let zipper = (src: string): Zipper.t =>
  switch (Parser.to_zipper(src, ~root=Exp)) {
  | Some(z) => z
  | None => failwith("doesn't parse: " ++ src)
  };

let term = (z: Zipper.t): Exp.t =>
  MakeTerm.from_zip_for_sem(z, ~root=Exp).term;

/* The named theorem's status and proof text, as the editor would show. */
let theorem = (~name, src: string): (string, string) => {
  let cell = evaluate(term(zipper(src)));
  switch (
    Web.Theorems.Model.theorems(cell.theorems)
    |> List.find_opt(((_, thm: Web.Theorems.Model.theorem)) =>
         thm.name == name
       )
  ) {
  | Some((_, thm)) => (
      ProofMap.full_status_of_proof(cell.theorems.proof_map, thm.proof)
      |> (
        fun
        | Proven => "proven"
        | ProvenModulo(_) => "proven-modulo"
        | Refuted => "refuted"
        | Incomplete => "incomplete"
      ),
      Fill.print_any(~inline=true, Proof(thm.proof)),
    )
  | None => ("missing", "")
  };
};

// Interventions

/* How `@` and the builtin functions the program uses compute. */
let builtin_defs = (program: string): string => {
  let uses = name =>
    StringUtil.match(StringUtil.regexp("\\b" ++ name ++ "\\("), program);
  let defs =
    Environment.fold(
      ((name, def), acc) =>
        uses(name)
          ? acc
            @ [
              "let "
              ++ name
              ++ " = "
              ++ Fill.print_any(~inline=true, Exp(def))
              ++ " in",
            ]
          : acc,
      [],
      Builtins.env_init,
    );
  String.concat(
    "\n",
    [
      "",
      "How builtins in this program compute (one `eval` takes one of these steps):",
      "- `xs @ ys` appends lists: `[] @ m` evaluates to `m`, and `(y :: l) @ m` to `y :: (l @ m)`. `[x]` is `x :: []`.",
    ]
    @ (defs == [] ? [] : ["```", ...defs] @ ["```"]),
  );
};

/* Worked examples of the shapes the benchmark's harder proofs take,
 * over a different type, so they show the shape and not the answer. */
let shapes =
  String.concat(
    "\n",
    [
      "",
      "## More examples",
      "",
      "Induction, citing the inductive hypothesis inside a focus:",
      "```",
      "type Nt = +Z+S(Nt) in",
      "let plus = fun (a, b) -> case a | Z => b | S(p) => S(plus((p, b))) end in",
      "theorem plus_z = forall n: Nt -> plus((n, Z)) == n proof",
      "  induction n",
      "  | Z => from plus((Z, Z)) do eval end; axiom refl_eq end",
      "  | S(p) => from plus((S(p), Z)) do eval end; from plus((p, Z)) do axiom ih end end; axiom refl_eq end",
      "  end",
      "in 0",
      "```",
      "",
      "When the inductive hypothesis has to hold for other values of a second variable (an accumulator), `generalize` it first and re-introduce it in each case:",
      "```",
      "type Nt = +Z+S(Nt) in",
      "let to_int = fun n -> case n | Z => 0 | S(p) => 1 + to_int(p) end in",
      "let count = fun (n, acc) -> case n | Z => acc | S(p) => count((p, acc + 1)) end in",
      "theorem count_to_int = forall n: Nt -> forall acc: Int -> acc + to_int(n) == count((n, acc)) proof",
      "  generalize acc => induction n",
      "  | Z => forall acc => from to_int(Z) do eval end; from count((Z, acc)) do eval end; from acc + 0 do rewrite acc end end; axiom refl_eq end",
      "  | S(p) => forall acc => from to_int(S(p)) do eval end; from count((S(p), acc)) do eval end;",
      "    from acc + (1 + to_int(p)) do rewrite (acc + 1) + to_int(p) end end; from count((p, acc + 1)) do axiomrev ih end end; axiom refl_eq end",
      "  end",
      "in 0",
      "```",
    ],
  );

let shape_examples = [
  (
    "plus_z",
    "type Nt = +Z+S(Nt) in\nlet plus = fun (a, b) -> case a | Z => b | S(p) => S(plus((p, b))) end in\ntheorem plus_z = forall n: Nt -> plus((n, Z)) == n proof induction n | Z => from plus((Z, Z)) do eval end; axiom refl_eq end | S(p) => from plus((S(p), Z)) do eval end; from plus((p, Z)) do axiom ih end end; axiom refl_eq end end in 0",
  ),
  (
    "count_to_int",
    "type Nt = +Z+S(Nt) in\nlet to_int = fun n -> case n | Z => 0 | S(p) => 1 + to_int(p) end in\nlet count = fun (n, acc) -> case n | Z => acc | S(p) => count((p, acc + 1)) end in\ntheorem count_to_int = forall n: Nt -> forall acc: Int -> acc + to_int(n) == count((n, acc)) proof generalize acc => induction n | Z => forall acc => from to_int(Z) do eval end; from count((Z, acc)) do eval end; from acc + 0 do rewrite acc end end; axiom refl_eq end | S(p) => forall acc => from to_int(S(p)) do eval end; from count((S(p), acc)) do eval end; from acc + (1 + to_int(p)) do rewrite (acc + 1) + to_int(p) end end; from count((p, acc + 1)) do axiomrev ih end end; axiom refl_eq end end in 0",
  ),
];

/* The goal and the facts in scope at each `?` the reply left, and at the
 * start of each induction case, so the model learns `ih` and `case_eq`
 * once it has picked a split. */
let goals_at_holes =
    (~program: Exp.t, ~dynamics, ~hole_id, reply: string): list(string) =>
  switch (
    Fill.ProofHole.parse(reply),
    Fill.ProofHole.theorem_of(~hole_id, program),
  ) {
  | (Ok(attempt), Some((theorem_id, _))) =>
    let env =
      Option.bind(dynamics, (d: Dynamics.t) =>
        List.find_opt(((id, _, _, _)) => id == theorem_id, d.theorems)
      );
    switch (
      env,
      EditorTransform.Rewrite.proof_in_exp(
        ~target_id=hole_id,
        attempt,
        program,
      ),
    ) {
    | (Some((_, _, env, _)), Some((_, filled))) =>
      switch (Fill.ProofHole.check_theorem(~core, ~env, ~theorem_id, filled)) {
      | Some(pm) =>
        let describe = (where, p: Proof.t) =>
          switch (ProofMap.lookup(Proof.rep_id(p), pm)) {
          | Some({incoming: Some(goal), ctx, _}) =>
            let facts =
              SemanticCtx.facts(ctx)
              |> List.fold_left(
                   (acc, (name, _, prop)) =>
                     List.mem_assoc(name, acc)
                       ? acc : acc @ [(name, Fill.print_exp(prop))],
                   [],
                 )
              |> List.filter(((name, _)) =>
                   List.mem(
                     name,
                     ["ih", "case_eq", "assume", "have", "where"],
                   )
                   || !String.contains(name, '_')
                   && String.length(name) <= 3
                 );
            [
              where
              ++ ": the goal is `"
              ++ Fill.print_exp(goal)
              ++ "`"
              ++ (
                facts == []
                  ? ""
                  : "; hypotheses: "
                    ++ String.concat(
                         ", ",
                         List.map(
                           ((n, p)) => "`" ++ n ++ "`: `" ++ p ++ "`",
                           facts,
                         ),
                       )
              ),
            ];
          | _ => []
          };
        let rec go = (p: Proof.t) =>
          (
            switch (p.term) {
            | EmptyHole => describe("At a `?`", p)
            | Induction(_, _, cases) =>
              List.concat_map(
                ((pat, body)) =>
                  describe(
                    "In the case `" ++ Fill.print_any(Pat(pat)) ++ "`",
                    body,
                  ),
                cases,
              )
            | _ => []
            }
          )
          @ List.concat_map(go, Proof.children(p));
        go(attempt);
      | None => []
      }
    | _ => []
    };
  | _ => []
  };

let goals_system = "\nYou may leave `?` where you aren't sure yet: the checker then tells you the goal and the hypotheses at each `?` and at the start of each induction case, and you can fill them in the next round. Write the whole proof each round.";

// Running

let bridge_ask: Fill.Loop.ask =
  (messages, k) => {
    let json =
      `List(
        List.map(
          (m: OpenRouter.Message.Model.t) =>
            `Assoc([
              (
                "role",
                `String(OpenRouter.Message.Utils.string_of_role(m.role)),
              ),
              ("content", `String(m.content)),
            ]),
          messages,
        ),
      )
      |> Yojson.Safe.to_string;
    Js.Unsafe.fun_call(
      Js.Unsafe.global##.benchAsk,
      [|
        Js.Unsafe.inject(Js.string(json)),
        Js.Unsafe.inject(
          Js.wrap_callback((ok, text) =>
            k(
              Js.to_bool(ok)
                ? Ok(Js.to_string(text)) : Error(Js.to_string(text)),
            )
          ),
        ),
      |],
    );
  };

let with_message = (i, f, messages: list(OpenRouter.Message.Model.t)) =>
  List.mapi(
    (j, m: OpenRouter.Message.Model.t) =>
      j == i
        ? {
          ...m,
          content: f(m.content),
        }
        : m,
    messages,
  );

let run_item =
    (~intervention: string, item: Yojson.Safe.t, k: Yojson.Safe.t => unit) => {
  open Yojson.Safe.Util;
  let name = item |> member("name") |> to_string;
  let src = item |> member("program") |> to_string;
  let reference =
    switch (item |> member("reference")) {
    | `String(r) => Some(fst(theorem(~name, r)))
    | _ => None
    };
  let z = zipper(src);
  let program = term(z);
  let dynamics = Option.map(snd, evaluate(program).theorems.checked);
  let hole_id =
    switch (Fill.Typed.llm_hole_ids(z)) {
    | [id, ..._] => id
    | [] => failwith("no ?? in " ++ name)
    };
  let has = i => String.contains(intervention, i);
  /* Interventions by letter: d builtin definitions, s shape examples,
   * g goals and hypotheses at the reply's holes and cases. */
  let ask = (messages: list(OpenRouter.Message.Model.t), k) => {
    let messages =
      messages
      |> with_message(0, s =>
           s ++ (has('s') ? shapes : "") ++ (has('g') ? goals_system : "")
         )
      |> with_message(1, s => has('d') ? s ++ builtin_defs(src) : s);
    let n = List.length(messages);
    let messages =
      has('g') && n > 2
        ? switch (List.nth(messages, n - 2)) {
          | {role: Assistant, content: reply, _} =>
            let goals = goals_at_holes(~program, ~dynamics, ~hole_id, reply);
            with_message(
              n - 1,
              s =>
                goals == []
                  ? s
                  : s
                    ++ "\n\nWhere your proof stands:\n"
                    ++ String.concat("\n", List.map(g => "- " ++ g, goals)),
              messages,
            );
          | _ => messages
          }
        : messages;
    bridge_ask(messages, k);
  };
  let statuses = ref([]);
  let written = ref(None);
  let finished = ref(false);
  let finish = () => {
    let (status, proof) =
      switch (written^) {
      | Some(text) => theorem(~name, text)
      | None => ("unwritten", "")
      };
    k(
      `Assoc([
        ("name", `String(name)),
        ("tier", item |> member("tier")),
        (
          "reference",
          switch (reference) {
          | Some(s) => `String(s)
          | None => `Null
          },
        ),
        (
          "statuses",
          `List(
            List.map(s => `String(Fill.Status.to_string(s)), statuses^),
          ),
        ),
        ("result", `String(status)),
        ("proof", `String(proof)),
      ]),
    );
  };
  Fill.Typed.fill(
    ~core,
    ~ask=Ok(ask),
    ~dynamics,
    ~app={
      set_status: s => {
        statuses := statuses^ @ [s];
        switch (s) {
        | Filling(_)
        | InChat => ()
        | Filled(_)
        | GaveUp(_)
        | Failed(_)
        | Cancelled
        | FilledInChat =>
          if (! finished^) {
            finished := true;
            finish();
          }
        };
      },
      write: patch =>
        written :=
          Some(
            EditorTransform.apply_patch(z, patch)
            |> Printer.of_zipper(~holes="?"),
          ),
    },
    ~program,
    hole_id,
  );
};

// The chat agent's way: proof tools in a tool-calling loop

/* The chat bridge in run.js: an OpenAI-style request with tools, answered
 * with the model's message as JSON. */
let bridge_chat =
    (
      messages: list(Yojson.Safe.t),
      tools: list(Yojson.Safe.t),
      k: result(Yojson.Safe.t, string) => unit,
    ) =>
  Js.Unsafe.fun_call(
    Js.Unsafe.global##.benchChat,
    [|
      Js.Unsafe.inject(
        Js.string(
          Yojson.Safe.to_string(
            `Assoc([
              ("messages", `List(messages)),
              ("tools", `List(tools)),
            ]),
          ),
        ),
      ),
      Js.Unsafe.inject(
        Js.wrap_callback((ok, text) =>
          k(
            Js.to_bool(ok)
              ? Ok(Yojson.Safe.from_string(Js.to_string(text)))
              : Error(Js.to_string(text)),
          )
        ),
      ),
    |],
  );

let tools_system = "You are Filbert, the agent in the Hazel editor. The user asks you to fill a hole in a theorem's proof. Use the tools: read the `proofs` docs, look at the hole with proof_holes, try proofs with check_proof as often as you need, and write the proof with fill_proof once it proves the goal.";

/* One theorem, the way the chat agent fills it: the request typing `??`
 * sends, the proof tools, at most `turns` model calls. */
let run_tools_item =
    (~turns: int, item: Yojson.Safe.t, k: Yojson.Safe.t => unit) => {
  open Yojson.Safe.Util;
  let name = item |> member("name") |> to_string;
  let src = item |> member("program") |> to_string;
  let reference =
    switch (item |> member("reference")) {
    | `String(r) => Some(fst(theorem(~name, r)))
    | _ => None
    };
  let z = zipper(src);
  let program = term(z);
  let hole_id =
    switch (Fill.Typed.llm_hole_ids(z)) {
    | [id, ..._] => id
    | [] => failwith("no ?? in " ++ name)
    };
  let cell: Web.CellEditor.Model.t = {
    ...Web.CellEditor.Model.mk(Editor.Model.mk(z, ~root=Exp)),
    result: evaluate(program),
  };
  let tools = [
    ProofTools.proof_holes,
    ProofTools.check_proof,
    ProofTools.fill_proof,
    DocsTools.read_docs,
  ];
  let finish = (~turn, ~why, cell: Web.CellEditor.Model.t) => {
    let text = Printer.of_zipper(~holes="?", cell.editor.editor.state.zipper);
    let (status, proof) = theorem(~name, text);
    k(
      `Assoc([
        ("name", `String(name)),
        ("tier", item |> member("tier")),
        (
          "reference",
          switch (reference) {
          | Some(s) => `String(s)
          | None => `Null
          },
        ),
        (
          "statuses",
          `List([
            `String(why ++ " after " ++ string_of_int(turn) ++ " calls"),
          ]),
        ),
        ("result", `String(status)),
        ("proof", `String(proof)),
      ]),
    );
  };
  let execute = (cell, call: Yojson.Safe.t) => {
    let id = call |> member("id") |> to_string;
    let f = call |> member("function");
    let tool_name = f |> member("name") |> to_string;
    let args =
      switch (f |> member("arguments")) {
      | `String(a) =>
        try(Yojson.Safe.from_string(a)) {
        | _ => `Assoc([])
        }
      | a => a
      };
    let (proved, content, cell) =
      switch (CompositionUtils.Public.action_of(~tool_name, ~args)) {
      | ProofRequest(r) =>
        let (ok, text, edited) =
          Web.AgentProofTools.answer(
            ~settings=Web.Settings.Model.init,
            r,
            cell,
          );
        let wrote =
          switch (r) {
          | FillProof(_) => ok
          | ProofHoles
          | CheckProof(_) => false
          };
        (wrote, text, Option.value(~default=cell, edited));
      | DocsRequest(topic) => (
          false,
          switch (DocPacks.lookup(topic)) {
          | Some(pack) => pack.body
          | None => "Unknown docs topic " ++ topic
          },
          cell,
        )
      | Action(_)
      | Failure(_) => (false, "That tool isn't available here.", cell)
      };
    (
      proved,
      `Assoc([
        ("role", `String("tool")),
        ("tool_call_id", `String(id)),
        ("content", `String(content)),
      ]),
      cell,
    );
  };
  let rec turn = (n, messages, cell) =>
    if (n > turns) {
      finish(~turn=n - 1, ~why="stopped", cell);
    } else {
      bridge_chat(messages, tools, reply =>
        switch (reply) {
        | Error(why) => finish(~turn=n, ~why="failed: " ++ why, cell)
        | Ok(message) =>
          switch (message |> member("tool_calls")) {
          | `List([_, ..._] as calls) =>
            let (proved, results, cell) =
              List.fold_left(
                ((proved, results, cell), call) => {
                  let (p, result, cell) = execute(cell, call);
                  (proved || p, results @ [result], cell);
                },
                (false, [], cell),
                calls,
              );
            let assistant =
              `Assoc([
                ("role", `String("assistant")),
                (
                  "content",
                  switch (message |> member("content")) {
                  | `String(c) => `String(c)
                  | _ => `String("")
                  },
                ),
                ("tool_calls", `List(calls)),
              ]);
            proved
              ? finish(~turn=n, ~why="filled", cell)
              : turn(n + 1, messages @ [assistant, ...results], cell);
          | _ => finish(~turn=n, ~why="answered without a tool", cell)
          }
        }
      );
    };
  turn(
    1,
    [
      `Assoc([
        ("role", `String("system")),
        ("content", `String(tools_system)),
      ]),
      `Assoc([
        ("role", `String("user")),
        ("content", `String(Fill.Typed.request(~core, program, hole_id))),
      ]),
    ],
    cell,
  );
};

let () = {
  let config =
    Js.to_string(Js.Unsafe.global##.benchConfig) |> Yojson.Safe.from_string;
  open Yojson.Safe.Util;
  let intervention = config |> member("intervention") |> to_string;
  let only =
    switch (config |> member("only")) {
    | `List(names) => Some(List.map(to_string, names))
    | _ => None
    };
  let items =
    config
    |> member("items")
    |> to_list
    |> List.filter(item =>
         switch (only) {
         | Some(names) =>
           List.mem(item |> member("name") |> to_string, names)
         | None => true
         }
       );
  let examples =
    config |> member("check_examples") |> to_bool
      ? List.map(
          ((name, src)) => (name, `String(fst(theorem(~name, src)))),
          shape_examples,
        )
      : [];
  let rec go = (items, acc) =>
    switch (items) {
    | [] =>
      Js.Unsafe.fun_call(
        Js.Unsafe.global##.benchDone,
        [|
          Js.Unsafe.inject(
            Js.string(
              `Assoc([
                ("examples", `Assoc(examples)),
                ("results", `List(List.rev(acc))),
              ])
              |> Yojson.Safe.to_string,
            ),
          ),
        |],
      )
    | [item, ...rest] =>
      let () =
        Js.Unsafe.fun_call(
          Js.Unsafe.global##.benchProgress,
          [|
            Js.Unsafe.inject(
              Js.string(item |> member("name") |> to_string),
            ),
          |],
        );
      switch (config |> member("mode")) {
      | `String("tools") =>
        run_tools_item(
          ~turns=config |> member("turns") |> to_int, item, result =>
          go(rest, [result, ...acc])
        )
      | _ =>
        run_item(~intervention, item, result => go(rest, [result, ...acc]))
      };
    };
  go(items, []);
};
