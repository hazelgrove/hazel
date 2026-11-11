open Util;
open Language;
open Js_of_ocaml;

/* Fills a hole with a model's code. Each round asks the model for the
 * hole's replacement, writes it in place of the hole in a copy of the
 * program and checks that copy: an expression's statics, or a proof
 * through the checker as Theorems runs it, with the theorem's
 * environment from the last evaluation. When the copy has problems
 * (static errors; proof marks, open cases, open obligations, a goal left
 * over) they go back to the model for another round. Only the final
 * attempt is written into the editor: the first without problems, or,
 * after the last round, the one with the fewest. */

let max_rounds = 20;

/* Used when the agent panel has no model selected. */
let default_model_id = "google/gemini-3-flash-preview";

module Status = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Filling(int)
    | Filled(int)
    | GaveUp(int, string)
    | Failed(string)
    | Cancelled;

  let to_string = (status: t): string => {
    let rounds = n => Printf.sprintf("%d round%s", n, n == 1 ? "" : "s");
    switch (status) {
    | Filling(1) => "filling…"
    | Filling(n) => Printf.sprintf("filling… (round %d)", n)
    | Filled(n) => "filled in " ++ rounds(n)
    | GaveUp(n, last) => "gave up after " ++ rounds(n) ++ ": " ++ last
    | Failed(why) => "couldn't fill: " ++ why
    | Cancelled => "cancelled"
    };
  };
};

/* What a fill reports to, and writes through. */
type app = {
  set_status: Status.t => unit,
  write: Haz3lcore.EditorTransform.patch => unit,
};

/* A goal as the model sees it. The checker inlines definitions into
 * goals, so a function is printed by its name, as the stepper shows it,
 * not by its body. Ascriptions are left out: elaboration adds them
 * (`[]: [Int]`), and a binder's type prints fully expanded. The
 * default display settings are used, not the user's, so the model's
 * notation doesn't depend on them. */
let print_exp = (e: Exp.t): string =>
  e
  |> Haz3lcore.ExpToSegment.exp_to_segment(
       ~settings={
         ...ProofMarkView.code_view_settings(CoreSettings.on),
         fold_fn_bodies: `Text,
         show_ascriptions: false,
       },
     )
  |> Haz3lcore.Printer.of_segment(~holes="?", ~refractors=[]);

let print_any = (~inline=true, a: Any.t): string =>
  a
  |> Haz3lcore.ExpToSegment.any_to_segment(
       ~settings=Haz3lcore.ExpToSegment.Settings.editable(~inline),
     )
  |> Haz3lcore.Printer.of_segment(~holes="?", ~refractors=[]);

/* The program as the model sees it: the hole being filled written `??`,
 * every other hole `?`. */
let print_program = (~hole_id: Id.t, program: Exp.t): string => {
  let mark = (id, ann: IdTagged.IdTag.t): IdTagged.IdTag.t => {
    ...ann,
    lexeme:
      id == hole_id
        ? Some(Token.llm_hole)
        : Option.bind(ann.lexeme, l => Token.is_llm_hole(l) ? None : Some(l)),
  };
  let f_exp = (cont, e: Exp.t) =>
    switch (e.term) {
    | EmptyHole => {
        ...e,
        annotation: mark(Exp.rep_id(e), e.annotation),
      }
    | _ => cont(e)
    };
  let f_proof = (cont, p: Proof.t) =>
    switch (p.term) {
    | EmptyHole => {
        ...p,
        annotation: mark(Proof.rep_id(p), p.annotation),
      }
    | _ => cont(p)
    };
  print_any(~inline=false, Exp(Exp.map_term(~f_exp, ~f_proof, program)));
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
    |> String.trim
  | _ => String.trim(reply)
  };

/* `text` parsed as an expression. The parser can raise on malformed
 * text; such a reply must never reach the editor, which would parse it
 * again. */
let parse_exp = (text: string): result(Exp.t, string) =>
  switch (Haz3lcore.Parser.to_term(text, ~root=Exp)) {
  | Some(e) => Ok(e)
  | None => Error("your reply doesn't parse")
  | exception e =>
    Error(
      "your reply doesn't parse (the parser raised "
      ++ Printexc.to_string(e)
      ++ ")",
    )
  };

/* The rounds, with the model and the checker passed in, so tests can
 * stand in for both. */
module Loop = {
  type ask =
    (list(OpenRouter.Message.Model.t), result(string, string) => unit) =>
    unit;

  /* What checking makes of a reply. */
  type outcome('a) =
    /* The reply didn't parse, or the checker raised on it. It is never
     * kept: the editor would fail on it the same way. */
    | Unusable(string)
    /* The hole is gone from the program the fill started from. */
    | HoleGone
    /* The attempt and its problems; none means it fills the hole. */
    | Checked('a, list(string));

  let run =
      (
        ~ask: ask,
        ~system: string,
        ~retry: list(string) => string,
        ~attempt: string => outcome('a),
        ~set_status: Status.t => unit,
        ~write: 'a => unit,
        ~log: string => unit,
        /* Once true, the fill asks nothing more and writes nothing. */
        ~cancelled: unit => bool=() => false,
        first: string,
      )
      : unit => {
    let system = OpenRouter.Message.Utils.mk_system_msg(system);
    let fewer_problems = ((a, problems), best) =>
      switch (best) {
      | Some((_, best_problems) as b)
          when List.length(best_problems) < List.length(problems) => b
      | _ => (a, problems)
      };
    let give_up = (best, status) => {
      Option.iter(((a, _)) => write(a), best);
      set_status(status);
    };
    /* `history` is the exchange so far, `prompt` this round's message. */
    let rec round = (n, history, prompt, best) =>
      if (!cancelled()) {
        round_(n, history, prompt, best);
      }
    and round_ = (n, history, prompt, best) => {
      set_status(Filling(n));
      log(Printf.sprintf("round %d, to the model:\n%s", n, prompt));
      let messages =
        history @ [OpenRouter.Message.Utils.mk_user_msg(prompt)];
      ask([system, ...messages], reply =>
        switch (reply) {
        | _ when cancelled() => log(Printf.sprintf("round %d cancelled", n))
        | Error(why) =>
          log(Printf.sprintf("round %d failed: %s", n, why));
          give_up(best, Failed(why));
        | Ok(reply) =>
          log(Printf.sprintf("round %d, from the model:\n%s", n, reply));
          let next = (problems, best) =>
            switch (problems) {
            | [last, ..._] when n >= max_rounds =>
              give_up(best, GaveUp(n, last))
            | _ =>
              round(
                n + 1,
                messages @ [OpenRouter.Message.Utils.mk_assistant_msg(reply)],
                retry(problems),
                best,
              )
            };
          switch (attempt(reply)) {
          | exception e =>
            next(
              ["checking your reply raised " ++ Printexc.to_string(e)],
              best,
            )
          | Unusable(why) => next([why], best)
          | HoleGone =>
            give_up(best, Failed("the program changed; try again"))
          | Checked(a, []) =>
            write(a);
            set_status(Filled(n));
          | Checked(a, problems) =>
            next(problems, Some(fewer_problems((a, problems), best)))
          };
        }
      );
    };
    log("system prompt:\n" ++ system.content);
    round(1, [], first, None);
  };
};

let log = (msg: string): unit =>
  Firebug.console##log(Js.string("[agent fill] " ++ msg));

/* The fills in progress, by the hole each fills, so an edit that removes
 * a hole, or the stop button, can cancel its fill. A fill is a chain of
 * callbacks that lives only as long as the page, so this is not part of
 * any model. */
module Active = {
  let cancels: ref(Id.Map.t(unit => unit)) = ref(Id.Map.empty);

  /* Registers a fill of `hole_id`. The returned app takes it off the
   * register when the fill ends; the returned function says whether it
   * has been cancelled. */
  let track = (~hole_id: Id.t, app: app): (app, unit => bool) => {
    let cancelled = ref(false);
    cancels :=
      Id.Map.add(
        hole_id,
        () => {
          cancelled := true;
          cancels := Id.Map.remove(hole_id, cancels^);
          app.set_status(Cancelled);
        },
        cancels^,
      );
    let set_status = (status: Status.t) => {
      switch (status) {
      | Filling(_) => ()
      | Filled(_)
      | GaveUp(_)
      | Failed(_)
      | Cancelled => cancels := Id.Map.remove(hole_id, cancels^)
      };
      app.set_status(status);
    };
    (
      {
        ...app,
        set_status,
      },
      () => cancelled^,
    );
  };

  let cancel_all = () => Id.Map.iter((_, cancel) => cancel(), cancels^);

  /* Cancels each fill whose hole isn't among `ids`, the ids of the
   * program as edited. */
  let cancel_missing = (ids: list(Id.t)) =>
    Id.Map.iter(
      (hole_id, cancel) =>
        if (!List.mem(hole_id, ids)) {
          cancel();
        },
      cancels^,
    );
};

/* The model the agent panel would use, asked through its key. */
let ask_agent = (agent: AgentGlobals.Model.t): result(Loop.ask, string) =>
  switch (Option.map(String.trim, agent.api_key)) {
  | None
  | Some("") => Error("set an OpenRouter key in the agent panel")
  | Some(key) =>
    let model_id =
      AgentGlobals.get_active_llm_id(agent)
      |> Option.value(~default=default_model_id);
    log("model " ++ model_id);
    Ok(
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
            k(
              Error(
                Printf.sprintf("request failed (%d): %s", code, message),
              ),
            )
          | None => k(Error("no response from the model"))
          }
        );
      },
    );
  };

// Proof holes

module ProofHole = {
  /* What the model is told about the hole. */
  type t = {
    /* The program with the hole written `??`. */
    program: string,
    theorem: string,
    statement: string,
    goal: string,
    /* Innermost first, one per name: a name cites its innermost fact. */
    facts: list((string, string)),
  };

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

  let first = (hole: t): string =>
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

  let retry = (problems: list(string)): string =>
    String.concat(
      "\n",
      ["The checker doesn't accept your proof:"]
      @ List.map(p => "- " ++ p, problems)
      @ [
        "Reply with a corrected, complete proof to replace `??`, and nothing else.",
      ],
    );

  /* The reply read as the proof of a throwaway theorem, so it parses
   * with whatever the proof grammar is, then printed and read again: that
   * is the proof the editor will hold, which can differ when the printer
   * drops parentheses the reply needed. */
  let parse = (reply: string): result(Proof.t, string) => {
    let read = text =>
      switch (parse_exp("theorem fill = true proof " ++ text ++ " in 0")) {
      | Ok({term: Theorem(_, _, proof, _), _}) => Ok(proof)
      | Ok(_) => Error("your reply doesn't read as a proof")
      | Error(why) => Error(why)
      };
    Stdlib.Result.bind(read(strip_fences(reply)), proof =>
      read(print_any(~inline=false, Proof(proof)))
    );
  };

  /* The theorem whose proof has the hole: its id and statement. */
  let theorem_of = (~hole_id: Id.t, program: Exp.t): option((Id.t, Exp.t)) => {
    let found = ref(None);
    let rec has_hole = (p: Proof.t) =>
      Proof.rep_id(p) == hole_id || List.exists(has_hole, Proof.children(p));
    let f_exp = (cont, e: Exp.t) => {
      switch (e.term) {
      | Theorem(_, stmt, proof, _) when has_hole(proof) =>
        found := Some((Exp.rep_id(e), stmt))
      | _ => ()
      };
      cont(e);
    };
    let _ = Exp.map_term(~f_exp, program);
    found^;
  };

  /* Checks the theorem in `program` as Theorems does: the program's own
   * statics, the theorem's environment from an evaluation. */
  let check_theorem =
      (~core, ~env, ~theorem_id: Id.t, program: Exp.t): option(ProofMap.t) => {
    let statics =
      Haz3lcore.CachedStatics.init_from_term(
        ~settings=core,
        ~is_dynamic_term=false,
        program,
      );
    Option.bind(Exp.find_by_id(theorem_id, statics.elaborated), theorem =>
      Evaluator.Proofs.check_theorem(~statics=statics.info_map, ~env, theorem)
    )
    |> Option.map((checked: Evaluator.Proofs.checked_theorem) =>
         checked.proof_map
       );
  };

  /* A marked step with the goal it was given: a step that lands in the
   * wrong scope, such as inside a focus it was meant to follow, shows by
   * its goal. */
  let mark_text = (pm: ProofMap.t, step: Proof.t, mark: ProofMark.t): string => {
    let text = print_any(Proof(step));
    let text =
      String.length(text) > 80 ? String.sub(text, 0, 77) ++ "..." : text;
    let goal =
      switch (ProofMap.lookup(Proof.rep_id(step), pm)) {
      | Some({incoming: Some(g), _}) =>
        " on the goal `" ++ print_exp(g) ++ "`"
      | Some({incoming: None, _})
      | None => ""
      };
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
      | RuleDoesNotApply({equality, direction}) =>
        Printf.sprintf(
          "`%s` doesn't match the root of the goal (%s)",
          equality,
          switch (direction) {
          | Right => "left to right"
          | Left => "right to left"
          },
        )
      | m => ProofMark.show(m)
      };
    "`" ++ text ++ "`" ++ goal ++ ": " ++ mark;
  };

  /* Everything that keeps `proof` from proving its goal; none means it
   * does. */
  let problems = (pm: ProofMap.t, proof: Proof.t): list(string) => {
    let rec steps = (p: Proof.t) => [
      p,
      ...List.concat_map(steps, Proof.children(p)),
    ];
    let marks =
      List.concat_map(
        step =>
          List.map(
            mark_text(pm, step),
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
    marks @ open_cases @ obligations @ unfinished;
  };

  /* `proof` in place of the hole, checked. */
  let check =
      (~core, ~env, ~theorem_id, ~hole_id, program: Exp.t, proof: Proof.t)
      : Loop.outcome(Proof.t) =>
    switch (
      Haz3lcore.EditorTransform.Rewrite.proof_in_exp(
        ~target_id=hole_id,
        proof,
        program,
      )
    ) {
    | None => HoleGone
    | Some((_, filled)) =>
      switch (check_theorem(~core, ~env, ~theorem_id, filled)) {
      | Some(pm) => Checked(proof, problems(pm, proof))
      | None => HoleGone
      }
    };

  /* Fills `hole_id`, a hole in a theorem's proof in `program`, checking
   * against `dynamics`, an evaluation of the program that reached the
   * theorem. */
  let start =
      (
        ~core: CoreSettings.t,
        ~ask: result(Loop.ask, string),
        ~dynamics: option(Dynamics.t),
        ~app: app,
        ~program: Exp.t,
        hole_id: Id.t,
      )
      : unit => {
    let theorem = theorem_of(~hole_id, program);
    let recorded =
      switch (theorem, dynamics) {
      | (Some((theorem_id, _)), Some(dynamics)) =>
        List.find_opt(
          ((id, _, _, _)) => id == theorem_id,
          dynamics.theorems,
        )
      | _ => None
      };
    switch (ask, theorem, recorded) {
    | (Error(why), _, _) => app.set_status(Failed(why))
    | (_, None, _)
    | (_, _, None) =>
      app.set_status(Failed("the evaluation hasn't reached this theorem"))
    | (Ok(ask), Some((theorem_id, statement)), Some((_, name, env, _))) =>
      let (app, cancelled) = Active.track(~hole_id, app);
      let entry =
        Option.bind(check_theorem(~core, ~env, ~theorem_id, program), pm =>
          ProofMap.lookup(hole_id, pm)
        );
      switch (entry) {
      | Some({incoming: Some(goal), ctx, _}) =>
        let facts =
          List.fold_left(
            (facts, (name, _, prop)) =>
              List.mem_assoc(name, facts)
                ? facts : facts @ [(name, print_exp(prop))],
            [],
            SemanticCtx.facts(ctx),
          );
        let hole = {
          program: print_program(~hole_id, program),
          theorem: name,
          statement: print_exp(statement),
          goal: print_exp(goal),
          facts,
        };
        Loop.run(
          ~ask,
          ~system,
          ~retry,
          ~attempt=
            reply =>
              switch (parse(reply)) {
              | Error(why) => Unusable(why)
              | Ok(proof) =>
                check(~core, ~env, ~theorem_id, ~hole_id, program, proof)
              },
          ~cancelled,
          ~set_status=app.set_status,
          ~write=
            p =>
              app.write(
                Haz3lcore.EditorTransform.mk_proof_patch(
                  ~target_id=hole_id,
                  p,
                ),
              ),
          ~log,
          first(hole),
        );
      | Some({incoming: None, _})
      | None => app.set_status(Failed("the checker found no goal here"))
      };
    };
  };
};

// Expression holes

module ExpHole = {
  /* What the model is told about the hole. */
  type t = {
    /* The program with the hole written `??`. */
    program: string,
    /* None when the hole may have any type. */
    expected: option(string),
    /* `name: type`, innermost first. */
    variables: list(string),
    /* `type Name = ...`, innermost first. */
    types: list(string),
  };

  let system: string =
    String.concat(
      "\n",
      [
        "You write Hazel code. You will be shown a program with one hole marked `??`, the type the hole is expected to have, and the variables and types in scope there. Reply with an expression to replace `??`, and nothing else: no explanation, no markdown fences, not the surrounding program. The expression must have no static errors in place.",
        "",
      ]
      @ Haz3lcore.HazelSyntaxNotes.self,
    );

  let first = (hole: t): string => {
    let section = (title, none, lines) =>
      switch (lines) {
      | [] => ["", none]
      | lines => ["", title, ...List.map(l => "- `" ++ l ++ "`", lines)]
      };
    String.concat(
      "\n",
      [
        "Program:",
        "```",
        hole.program,
        "```",
        "",
        switch (hole.expected) {
        | Some(ty) => "Expected type of `??`: `" ++ ty ++ "`"
        | None => "`??` may have any type."
        },
      ]
      @ section(
          "Variables in scope at `??` (besides the builtins):",
          "No variables of the program are in scope at `??`.",
          hole.variables,
        )
      @ section(
          "Type definitions in scope at `??`:",
          "No type definitions are in scope at `??`.",
          hole.types,
        ),
    );
  };

  let retry = (problems: list(string)): string =>
    String.concat(
      "\n",
      [
        "With your code in place of `??`, the program has these static errors:",
      ]
      @ List.map(p => "- " ++ p, problems)
      @ [
        "Reply with a corrected expression to replace `??`, and nothing else.",
      ],
    );

  let statics = (~core, program: Exp.t) =>
    Haz3lcore.CachedStatics.init_from_term(
      ~settings=core,
      ~is_dynamic_term=false,
      program,
    );

  let describe =
      (~program: Exp.t, ~info_map: Statics.Map.t, hole_id: Id.t): option(t) =>
    Statics.Map.lookup_exp(hole_id, info_map)
    |> Option.map((info: Info.exp) => {
         let expected =
           switch (Typ.term_of(info.ana)) {
           | Unknown(_) => None
           | _ => Some(print_any(Typ(info.ana)))
           };
         /* The program's own bindings: what the hole's context adds to
          * the builtins it started from. */
         let entries =
           Ctx.added_bindings(info.ctx, Builtins.ctx_init(Some(Int)))
           |> Ctx.filter_shadowed
           |> (ctx => ctx.entries);
         {
           program: print_program(~hole_id, program),
           expected,
           variables:
             List.filter_map(
               (entry: Ctx.entry) =>
                 switch (entry) {
                 | VarEntry({name, typ, _}) =>
                   Some(name ++ ": " ++ print_any(Typ(typ)))
                 | ConstructorEntry(_)
                 | TVarEntry(_)
                 | LivelitEntry(_)
                 | TheoremEntry(_) => None
                 },
               entries,
             ),
           types:
             List.filter_map(
               (entry: Ctx.entry) =>
                 switch (entry) {
                 | TVarEntry({name, kind: Singleton(ty), _}) =>
                   Some("type " ++ name ++ " = " ++ print_any(Typ(ty)))
                 | TVarEntry({kind: Abstract, _})
                 | VarEntry(_)
                 | ConstructorEntry(_)
                 | LivelitEntry(_)
                 | TheoremEntry(_) => None
                 },
               entries,
             ),
         };
       });

  /* `exp` in place of the hole; its problems are the static errors the
   * program didn't have before. */
  let check =
      (~core, ~errors_before: list(string), ~hole_id, program, exp: Exp.t)
      : Loop.outcome(Exp.t) =>
    switch (
      Haz3lcore.EditorTransform.Rewrite.exp(
        ~target_id=Some(hole_id),
        exp,
        program,
      )
    ) {
    | None => HoleGone
    | Some((_, filled)) =>
      let errors = Haz3lcore.ErrorPrint.all(statics(~core, filled).info_map);
      Checked(exp, List.filter(e => !List.mem(e, errors_before), errors));
    };

  let start =
      (
        ~core: CoreSettings.t,
        ~ask: result(Loop.ask, string),
        ~app: app,
        ~program: Exp.t,
        hole_id: Id.t,
      )
      : unit => {
    let info_map = statics(~core, program).info_map;
    switch (ask, describe(~program, ~info_map, hole_id)) {
    | (Error(why), _) => app.set_status(Failed(why))
    | (_, None) =>
      app.set_status(Failed("the statics haven't reached this hole"))
    | (Ok(ask), Some(hole)) =>
      let (app, cancelled) = Active.track(~hole_id, app);
      let errors_before = Haz3lcore.ErrorPrint.all(info_map);
      Loop.run(
        ~ask,
        ~system,
        ~retry,
        ~attempt=
          reply =>
            /* Read back as the editor will hold it, as for proofs. */
            switch (
              Stdlib.Result.bind(parse_exp(strip_fences(reply)), exp =>
                parse_exp(print_any(~inline=false, Exp(exp)))
              )
            ) {
            | Error(why) => Unusable(why)
            | Ok(exp) => check(~core, ~errors_before, ~hole_id, program, exp)
            },
        ~cancelled,
        ~set_status=app.set_status,
        ~write=
          e =>
            app.write(
              Haz3lcore.EditorTransform.mk_patch(~target_id=hole_id, e),
            ),
        ~log,
        first(hole),
      );
    };
  };
};

// Typing `??`

/* A fill starts when an edit types `??`, not whenever a program has one:
 * a program loaded with `??` in it starts nothing. */
module Typed = {
  let tiles = (z: Haz3lcore.Zipper.t): list(Haz3lcore.Tile.t) => {
    let rec go = (seg: Haz3lcore.Segment.t) =>
      List.concat_map(
        (p: Haz3lcore.Piece.t) =>
          switch (p) {
          | Tile(t) => [t, ...List.concat_map(go, t.children)]
          | Grout(_)
          | Secondary(_)
          | Projector(_) => []
          },
        seg,
      );
    go(Haz3lcore.Zipper.unselect_and_zip(z));
  };

  let llm_hole_ids = (z: Haz3lcore.Zipper.t): list(Id.t) =>
    tiles(z)
    |> List.filter(t => Haz3lcore.Tile.label(t) == [Token.llm_hole])
    |> List.map((t: Haz3lcore.Tile.t) => t.id);

  /* After any edit: cancels the fills whose hole the edit removed. */
  let edited = (z: Haz3lcore.Zipper.t): unit =>
    if (!Id.Map.is_empty(Active.cancels^)) {
      Active.cancel_missing(
        List.map((t: Haz3lcore.Tile.t) => t.id, tiles(z)),
      );
    };

  /* The `??` holes that `action` typed, going from `before` to `after`. */
  let holes =
      (
        action: CodeEditable.Update.t,
        ~before: Haz3lcore.Zipper.t,
        ~after: Haz3lcore.Zipper.t,
      )
      : list(Id.t) =>
    switch (action) {
    | Perform(Insert(_))
    | PerformConfined(_, Insert(_)) =>
      let existing = llm_hole_ids(before);
      List.filter(id => !List.mem(id, existing), llm_hole_ids(after));
    | Perform(_)
    | PerformConfined(_)
    | TAB
    | ContextMenu(_)
    | DebugConsole(_) => []
    };

  /* Fills `hole_id`, a `??` in `program`, as an expression or a proof. */
  let fill =
      (~core, ~ask, ~dynamics, ~app, ~program: Exp.t, hole_id: Id.t): unit =>
    Option.is_some(Exp.find_by_id(hole_id, program))
      ? ExpHole.start(~core, ~ask, ~app, ~program, hole_id)
      : ProofHole.start(~core, ~ask, ~dynamics, ~app, ~program, hole_id);

  /* Starts a fill for each hole `action` typed. The fills start after
   * the edit, so typing isn't held up by their statics. */
  let start =
      (
        ~core: CoreSettings.t,
        ~agent: AgentGlobals.Model.t,
        ~dynamics: option(Dynamics.t),
        ~app: app,
        ~before: Haz3lcore.Zipper.t,
        ~after: Haz3lcore.Zipper.t,
        action: CodeEditable.Update.t,
      )
      : unit =>
    switch (holes(action, ~before, ~after)) {
    | [] => ()
    | ids =>
      JsUtil.delay(
        0.0,
        () => {
          let program =
            Haz3lcore.MakeTerm.from_zip_for_sem(after, ~root=Exp).term;
          let ask = ask_agent(agent);
          List.iter(fill(~core, ~ask, ~dynamics, ~app, ~program), ids);
        },
      )
    };
};

module View = {
  open Virtual_dom.Vdom;

  let button =
      (~status: option(Status.t), start: unit => unit): list(Node.t) => {
    let filling =
      switch (status) {
      | Some(Filling(_)) => true
      | Some(Filled(_) | GaveUp(_) | Failed(_) | Cancelled)
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
    | Some(s) =>
      let stop =
        switch (s) {
        | Filling(_) => [
            Node.button(
              ~attrs=[
                Attr.classes(["agent-fill-stop"]),
                Attr.title("Stop filling"),
                Attr.on_click(_ =>
                  Ui_effect.of_sync_fun(Active.cancel_all, ())
                ),
              ],
              [Node.text("stop")],
            ),
          ]
        | Filled(_)
        | GaveUp(_)
        | Failed(_)
        | Cancelled => []
        };
      [
        Node.div(
          ~attrs=[
            Attr.classes(["agent-fill-status"]),
            Attr.title(Status.to_string(s)),
          ],
          [Node.text(Status.to_string(s))],
        ),
      ]
      @ stop;
    | None => []
    };
};
