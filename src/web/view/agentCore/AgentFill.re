open Util;
open Language;
open Js_of_ocaml;

/* Fills a proof hole with a model's proof. Each round asks the model for
 * proof text, writes it in place of the hole in a copy of the program
 * and checks that copy as Theorems does, with the statics of the copy
 * and the theorem's environment from the last evaluation. When the
 * checker objects (marks, open cases, open obligations, a goal left
 * over), the objections go back to the model for another round. Only the
 * final attempt is written into the editor: the first that proves the
 * goal, or, after the last round, the one with the fewest objections. */

let max_rounds = 3;

/* Used when the agent panel has no model selected. */
let default_model_id = "google/gemini-3-flash-preview";

module Status = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Filling(int)
    | Filled(int)
    | GaveUp(int, string)
    | Failed(string);

  let to_string = (status: t): string => {
    let rounds = n => Printf.sprintf("%d round%s", n, n == 1 ? "" : "s");
    switch (status) {
    | Filling(1) => "filling…"
    | Filling(n) => Printf.sprintf("filling… (round %d)", n)
    | Filled(n) => "filled in " ++ rounds(n)
    | GaveUp(n, last) => "gave up after " ++ rounds(n) ++ ": " ++ last
    | Failed(why) => "couldn't fill: " ++ why
    };
  };
};

let print_exp = (e: Exp.t): string =>
  e
  |> Haz3lcore.ExpToSegment.exp_to_segment(
       /* The model always sees the same notation, whatever the user's
          display settings. */
       ~settings=ProofMarkView.code_view_settings(CoreSettings.on),
     )
  |> Haz3lcore.Printer.of_segment(~holes="?", ~refractors=[]);

let print_any = (a: Any.t): string =>
  a
  |> Haz3lcore.ExpToSegment.any_to_segment(
       ~settings=Haz3lcore.ExpToSegment.Settings.editable(~inline=true),
     )
  |> Haz3lcore.Printer.of_segment(~holes="?", ~refractors=[]);

/* What the model is told about the hole. */
module Hole = {
  type t = {
    /* The cell's program with the hole written `??`. */
    program: string,
    theorem: string,
    statement: string,
    goal: string,
    /* Innermost first, one per name: a name cites its innermost fact. */
    facts: list((string, string)),
  };

  let mk =
      (
        ~program: Exp.t,
        ~theorem: string,
        ~statement: Exp.t,
        ~hole_id: Id.t,
        entry: ProofMap.entry,
      )
      : option(t) => {
    open OptUtil.Syntax;
    let* goal = entry.incoming;
    let+ (_, marked) =
      Haz3lcore.EditorTransform.Rewrite.proof_in_exp(
        ~target_id=hole_id,
        Proof.fresh(Invalid("??")),
        program,
      );
    let facts =
      List.fold_left(
        (facts, (name, _, prop)) =>
          List.mem_assoc(name, facts)
            ? facts : facts @ [(name, print_exp(prop))],
        [],
        SemanticCtx.facts(entry.ctx),
      );
    {
      program:
        marked
        |> Haz3lcore.ExpToSegment.exp_to_segment(
             ~settings=
               Haz3lcore.ExpToSegment.Settings.editable(~inline=false),
           )
        |> Haz3lcore.Printer.of_segment(~holes="?", ~refractors=[]),
      theorem,
      statement: print_exp(statement),
      goal: print_exp(goal),
      facts,
    };
  };
};

module Prompt = {
  let system: string =
    String.concat(
      "\n",
      [
        "You write proofs for Hazel's prover. You will be shown a program with one unfinished proof marked `??`, the goal at that point and the facts in scope there. Reply with proof text to replace `??`, and nothing else: no explanation, no markdown fences, not the surrounding program. The proof must turn the goal into `true`.",
        "",
      ]
      @ Haz3lcore.ProofGrammarNotes.self
      @ ["", "## Builtin axioms", "", "Citable by name in every proof:", ""]
      @ List.map(
          ((name, stmt)) =>
            "- `" ++ name ++ "`: `" ++ print_exp(stmt) ++ "`",
          Axioms.axioms,
        ),
    );

  let first = (hole: Hole.t): string =>
    String.concat(
      "\n",
      [
        "Program:",
        "```",
        hole.program,
        "```",
        "",
        "The `??` is in the proof of theorem `"
        ++ hole.theorem
        ++ "`, whose statement is `"
        ++ hole.statement
        ++ "`.",
        "",
        "Goal at `??`: `" ++ hole.goal ++ "`",
        "",
      ]
      @ (
        switch (hole.facts) {
        | [] => ["No hypotheses or theorems are in scope at `??`."]
        | facts => [
            "Facts in scope at `??` (besides the builtin axioms):",
            ...List.map(
                 ((name, prop)) => "- `" ++ name ++ "`: `" ++ prop ++ "`",
                 facts,
               ),
          ]
        }
      ),
    );

  let retry = (complaints: list(string)): string =>
    String.concat(
      "\n",
      ["The checker doesn't accept your proof:"]
      @ List.map(c => "- " ++ c, complaints)
      @ [
        "Reply with a corrected, complete proof to replace `??`, and nothing else.",
      ],
    );
};

/* A reply, checked in place of the hole. */
module Attempt = {
  /* An attempt proves its goal when it has no complaints. */
  type t = {
    proof: Proof.t,
    complaints: list(string),
  };

  /* Models often fence code despite being asked not to. */
  let strip_fences = (reply: string): string =>
    switch (String.split_on_char('\n', String.trim(reply))) {
    | [first, ...rest] when String.starts_with(~prefix="```", first) =>
      rest
      |> List.filter(line =>
           !String.starts_with(~prefix="```", String.trim(line))
         )
      |> String.concat("\n")
    | _ => reply
    };

  /* The reply read as the proof of a throwaway theorem, so it parses
   * with whatever the proof grammar is. The parser can raise on malformed
   * text; such a reply must never reach the editor, which would parse it
   * again. */
  let parse = (reply: string): result(Proof.t, string) => {
    let text = String.trim(strip_fences(reply));
    switch (
      Haz3lcore.Parser.to_term(
        "theorem fill = true proof " ++ text ++ " in 0",
        ~root=Exp,
      )
      |> Option.map(Exp.term_of)
    ) {
    | Some(Theorem(_, _, proof, _)) => Ok(proof)
    | Some(_)
    | None => Error("your reply doesn't read as a proof")
    | exception e =>
      Error(
        "your reply doesn't read as a proof (the parser raised "
        ++ Printexc.to_string(e)
        ++ ")",
      )
    };
  };

  let complaint = (step: Proof.t, mark: ProofMark.t): string => {
    let text = print_any(Proof(step));
    let text =
      String.length(text) > 80 ? String.sub(text, 0, 77) ++ "..." : text;
    let mark =
      switch (mark) {
      | PatternNotFound({selector, ordinal}) =>
        Printf.sprintf(
          "the goal has fewer than %d occurrences of `%s`",
          ordinal,
          print_exp(selector),
        )
      | NothingToStep => "the root of the goal can't take an evaluation step"
      | UnknownEquality(name) => "`" ++ name ++ "` is not a fact in scope"
      | m => ProofMark.show(m)
      };
    "`" ++ text ++ "`: " ++ mark;
  };

  /* `None` when the hole or the theorem isn't in the checked program, as
   * after an edit the evaluation hasn't caught up with. */
  let check =
      (
        ~settings: CoreSettings.t,
        ~checked as (
          statics: Haz3lcore.CachedStatics.t,
          dynamics: Dynamics.t,
        ),
        ~theorem_id: Id.t,
        ~hole_id: Id.t,
        proof: Proof.t,
      )
      : option(t) => {
    open OptUtil.Syntax;
    let* (_, program) =
      Haz3lcore.EditorTransform.Rewrite.proof_in_exp(
        ~target_id=hole_id,
        proof,
        statics.term,
      );
    let* (_, _, env, _) =
      List.find_opt(((id, _, _, _)) => id == theorem_id, dynamics.theorems);
    let statics =
      Haz3lcore.CachedStatics.init_from_term(
        ~settings,
        ~is_dynamic_term=false,
        program,
      );
    let* theorem = Exp.find_by_id(theorem_id, statics.elaborated);
    let+ {proof_map: pm, _}: Evaluator.Proofs.checked_theorem =
      Evaluator.Proofs.check_theorem(
        ~statics=statics.info_map,
        ~env,
        theorem,
      );
    let rec steps = (p: Proof.t) => [
      p,
      ...List.concat_map(steps, Proof.children(p)),
    ];
    let marks =
      List.concat_map(
        step =>
          List.map(
            complaint(step),
            ProofMap.marks_of(Proof.rep_id(step), pm),
          ),
        steps(proof),
      );
    let is_true = (e: Exp.t) =>
      Exp.fast_equal(e, Exp.fresh(Atom(Bool(true))));
    let left_open = (p: Proof.t) =>
      switch (ProofMap.lookup(Proof.rep_id(p), pm)) {
      | Some({outgoing: Some(g), _}) when is_true(g) => None
      | Some({outgoing: Some(g), _}) =>
        Some("left the goal `" ++ print_exp(g) ++ "`")
      | Some({outgoing: None, _})
      | None => Some("checking didn't reach the end")
      };
    /* An induction steps to `true` once its cases cover the scrutinee,
     * so its cases are checked one by one. */
    let open_cases =
      List.concat_map(
        (step: Proof.t) =>
          switch (step.term) {
          | Induction(_, _, cases) =>
            List.filter_map(
              ((pat, body)) =>
                left_open(body)
                |> Option.map(why =>
                     "the case `" ++ print_any(Pat(pat)) ++ "` " ++ why
                   ),
              cases,
            )
          | _ => []
          },
        steps(proof),
      );
    let obligations =
      List.map(
        (ob: Obligation.t) =>
          "the obligation `" ++ print_exp(ob.goal) ++ "` is left unproven",
        ProofMap.pending_obligations(pm, proof),
      );
    let unfinished =
      switch (marks, left_open(proof)) {
      | ([], Some(why)) => ["your proof " ++ why]
      | _ => []
      };
    {
      proof,
      complaints: marks @ open_cases @ obligations @ unfinished,
    };
  };
};

/* The rounds, with the model and the checker passed in, so tests can
 * stand in for both. */
module Loop = {
  type ask =
    (list(OpenRouter.Message.Model.t), result(string, string) => unit) =>
    unit;

  let run =
      (
        ~ask: ask,
        ~check: Proof.t => option(Attempt.t),
        ~set_status: Status.t => unit,
        ~write: Proof.t => unit,
        ~log: string => unit,
        hole: Hole.t,
      )
      : unit => {
    let system = OpenRouter.Message.Utils.mk_system_msg(Prompt.system);
    let fewer_complaints = (a: Attempt.t, best: option(Attempt.t)) =>
      switch (best) {
      | Some(b) when List.length(b.complaints) < List.length(a.complaints) => b
      | _ => a
      };
    let give_up = (best: option(Attempt.t), status) => {
      Option.iter((a: Attempt.t) => write(a.proof), best);
      set_status(status);
    };
    /* `history` is the exchange so far, `prompt` this round's message. */
    let rec round = (n, history, prompt, best) => {
      set_status(Filling(n));
      log(Printf.sprintf("round %d, to the model:\n%s", n, prompt));
      let messages =
        history @ [OpenRouter.Message.Utils.mk_user_msg(prompt)];
      ask([system, ...messages], reply =>
        switch (reply) {
        | Error(why) =>
          log(Printf.sprintf("round %d failed: %s", n, why));
          give_up(best, Failed(why));
        | Ok(reply) =>
          log(Printf.sprintf("round %d, from the model:\n%s", n, reply));
          let next = (complaints, best) =>
            switch (complaints) {
            | [last, ..._] when n >= max_rounds =>
              give_up(best, GaveUp(n, last))
            | _ =>
              round(
                n + 1,
                messages @ [OpenRouter.Message.Utils.mk_assistant_msg(reply)],
                Prompt.retry(complaints),
                best,
              )
            };
          /* A reply that doesn't parse, or that crashes the checker, is
           * never kept: the editor would hit the same failure. */
          switch (Attempt.parse(reply)) {
          | Error(why) => next([why], best)
          | Ok(proof) =>
            switch (check(proof)) {
            | exception e =>
              next(
                [
                  "the checker raised "
                  ++ Printexc.to_string(e)
                  ++ " on your proof",
                ],
                best,
              )
            | None => give_up(best, Failed("the program changed; try again"))
            | Some({complaints: [], _} as a) =>
              write(a.proof);
              set_status(Filled(n));
            | Some(a) => next(a.complaints, Some(fewer_complaints(a, best)))
            }
          };
        }
      );
    };
    log("system prompt:\n" ++ Prompt.system);
    round(1, [], Prompt.first(hole), None);
  };
};

let ask_openrouter = (~key: string, ~model_id: string): Loop.ask =>
  (messages, k) => {
    let payload =
      OpenRouter.Payload.Utils.mk_default(
        ~model_id,
        ~messages,
        ~tools=[],
        (),
      );
    OpenRouter.Utils.start_chat(~key, ~payload, ~handler=response =>
      switch (OpenRouter.Utils.handle_chat(response)) {
      | Some(Reply(reply)) => k(Ok(reply.content))
      | Some(Error({message, code})) =>
        k(Error(Printf.sprintf("request failed (%d): %s", code, message)))
      | None => k(Error("no response from the model"))
      }
    );
  };

let log = (msg: string): unit =>
  Firebug.console##log(Js.string("[agent fill] " ++ msg));

/* Starts filling `hole`, a hole in `theorem`'s proof, reporting through
 * `set_status` and writing the result with `write`. */
let start =
    (
      ~globals: Globals.t,
      ~checked: option((Haz3lcore.CachedStatics.t, Dynamics.t)),
      ~proof_map: ProofMap.t,
      ~theorem_id: Id.t,
      ~theorem: string,
      ~statement: option(Exp.t),
      ~set_status: Status.t => unit,
      ~write: Proof.t => unit,
      hole: Proof.t,
    )
    : unit => {
  let agent = globals.settings.agent_globals;
  let hole_id = Proof.rep_id(hole);
  switch (
    Option.map(String.trim, agent.api_key),
    checked,
    statement,
    ProofMap.lookup(hole_id, proof_map),
  ) {
  | (None | Some(""), _, _, _) =>
    set_status(Failed("set an OpenRouter key in the agent panel"))
  | (_, None, _, _)
  | (_, _, None, _)
  | (_, _, _, None) =>
    set_status(Failed("the checker hasn't reached this hole yet"))
  | (
      Some(key),
      Some((statics, _) as checked),
      Some(statement),
      Some(entry),
    ) =>
    switch (
      Hole.mk(~program=statics.term, ~theorem, ~statement, ~hole_id, entry)
    ) {
    | None => set_status(Failed("the checker found no goal at this hole"))
    | Some(described) =>
      let model_id =
        AgentGlobals.get_active_llm_id(agent)
        |> Option.value(~default=default_model_id);
      log("model " ++ model_id);
      Loop.run(
        ~ask=ask_openrouter(~key, ~model_id),
        ~check=
          Attempt.check(
            ~settings=globals.settings.core,
            ~checked,
            ~theorem_id,
            ~hole_id,
          ),
        ~set_status,
        ~write,
        ~log,
        described,
      );
    }
  };
};

module View = {
  open Virtual_dom.Vdom;

  let button =
      (~status: option(Status.t), start: unit => unit): list(Node.t) => {
    let filling =
      switch (status) {
      | Some(Filling(_)) => true
      | Some(Filled(_) | GaveUp(_) | Failed(_))
      | None => false
      };
    [
      Node.button(
        ~attrs=
          [
            Attr.classes(["agent-fill-button"]),
            Attr.title("Ask the agent to write a proof here"),
            Attr.on_click(_ => Ui_effect.of_sync_fun(start, ())),
          ]
          @ (filling ? [Attr.disabled] : []),
        [Node.text("fill with agent")],
      ),
    ];
  };

  let status = (status: option(Status.t)): list(Node.t) =>
    switch (status) {
    | Some(s) => [
        Node.div(
          ~attrs=[
            Attr.classes(["agent-fill-status"]),
            Attr.title(Status.to_string(s)),
          ],
          [Node.text(Status.to_string(s))],
        ),
      ]
    | None => []
    };
};
