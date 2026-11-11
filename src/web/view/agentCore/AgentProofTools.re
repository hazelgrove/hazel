open Language;

/* Answers the agent's proof tools (CompositionUtils.proof_request) on a
 * cell: its program as edited, checked with the theorem environments of
 * the cell's last evaluation, as the stepper checks it. */

module ProofHole = AgentFill.ProofHole;

/* An unfinished proof: a `?` or `??` in a theorem's proof. */
type hole = {
  id: Id.t,
  theorem_id: Id.t,
  theorem: string,
  typed: bool,
};

/* The program's proof holes, in program order. */
let holes = (program: Exp.t): list(hole) => {
  let found = ref([]);
  let f_exp = (cont, e: Exp.t) => {
    switch (e.term) {
    | Theorem(name, _, proof, _) =>
      let rec in_proof = (p: Proof.t) =>
        switch (p.term) {
        | EmptyHole => [
            {
              id: Proof.rep_id(p),
              theorem_id: Exp.rep_id(e),
              theorem: Pat.get_var(name) |> Option.value(~default="?"),
              typed: p.annotation.lexeme == Some(Token.llm_hole),
            },
          ]
        | _ => List.concat_map(in_proof, Proof.children(p))
        };
      found := found^ @ in_proof(proof);
    | _ => ()
    };
    cont(e);
  };
  let _ = Exp.map_term(~f_exp, program);
  found^;
};

/* Everything a tool needs about hole `n`, numbered from 1. */
type context = {
  hole,
  env: Environment.t(Exp.t),
  /* The theorem's proof map with the program as it is. */
  pm: ProofMap.t,
};

let context =
    (~core, ~dynamics: option(Dynamics.t), program: Exp.t, n: int)
    : result(context, string) =>
  switch (List.nth_opt(holes(program), n - 1), dynamics) {
  | (None, _) =>
    Error(
      Printf.sprintf(
        "There is no proof hole %d; proof_holes lists the holes.",
        n,
      ),
    )
  | (Some(_), None) =>
    Error(
      "The program hasn't been evaluated yet, so its proofs can't be checked.",
    )
  | (Some(hole), Some(dynamics)) =>
    switch (
      List.find_opt(
        ((id, _, _, _)) => id == hole.theorem_id,
        dynamics.theorems,
      )
    ) {
    | None =>
      Error(
        "The evaluation hasn't reached theorem `"
        ++ hole.theorem
        ++ "`, so its proof can't be checked yet.",
      )
    | Some((_, _, env, _)) =>
      switch (
        ProofHole.check_theorem(
          ~core,
          ~env,
          ~theorem_id=hole.theorem_id,
          program,
        )
      ) {
      | Some(pm) =>
        Ok({
          hole,
          env,
          pm,
        })
      | None =>
        Error("The checker couldn't check theorem `" ++ hole.theorem ++ "`.")
      }
    }
  };

let describe = (n: int, {hole, pm, _}: context): string => {
  let header =
    Printf.sprintf(
      "Hole %d%s, in the proof of theorem `%s`",
      n,
      hole.typed ? " (`??`, to fill)" : "",
      hole.theorem,
    );
  switch (ProofMap.lookup(hole.id, pm)) {
  | Some({incoming: Some(goal), ctx, _}) =>
    String.concat(
      "\n",
      [
        header ++ ":",
        "  goal: `" ++ AgentFill.print_exp(goal) ++ "`",
        "  facts in scope (besides the builtin axioms):",
      ]
      @ List.map(
          ((name, prop)) => "  - `" ++ name ++ "`: `" ++ prop ++ "`",
          ProofHole.facts(ctx),
        ),
    )
  | Some({incoming: None, _})
  | None => header ++ ": no goal reaches it, because an earlier step fails."
  };
};

/* What the prover makes of `text` in place of hole `n`: whether it
 * proves the goal, and the report to give the agent. */
let check =
    (~core, ~program: Exp.t, ctx: context, text: string)
    : result((Proof.t, bool, string), string) =>
  switch (ProofHole.parse(text)) {
  | Error(why) => Error(why)
  | Ok(proof) =>
    switch (
      ProofHole.checked_map(
        ~core,
        ~env=ctx.env,
        ~theorem_id=ctx.hole.theorem_id,
        ~hole_id=ctx.hole.id,
        program,
        proof,
      )
    ) {
    | None => Error("The hole is no longer in the program.")
    | Some(pm) =>
      let known =
        switch (ProofMap.lookup(ctx.hole.id, ctx.pm)) {
        | Some({ctx, _}) => ProofHole.facts(ctx)
        | None => []
        };
      let problems = ProofHole.problems(pm, proof);
      let goals = ProofHole.open_goals(~known, pm, proof);
      let section = (title, lines) =>
        lines == [] ? [] : [title, ...List.map(l => "- " ++ l, lines)];
      Ok((
        proof,
        problems == [],
        problems == []
          ? "This proof proves the goal."
          : String.concat(
              "\n",
              section("The prover doesn't accept this proof yet:", problems)
              @ section("Where the proof stands:", goals),
            ),
      ));
    }
  };

/* The answer to `request` on `cell`: whether it succeeded, what to tell
 * the agent, and the cell after it, when the request edits. */
let answer =
    (
      ~settings: Settings.t,
      request: Haz3lcore.CompositionUtils.proof_request,
      cell: CellEditor.Model.t,
    )
    : (bool, string, option(CellEditor.Model.t)) => {
  let core = Theorems.with_proof_settings(settings.core);
  let zipper = cell.editor.editor.state.zipper;
  let program = Haz3lcore.MakeTerm.from_zip_for_sem(zipper, ~root=Exp).term;
  let dynamics = Option.map(snd, cell.result.theorems.checked);
  switch (request) {
  | ProofHoles =>
    switch (holes(program)) {
    | [] => (true, "The program has no unfinished proofs.", None)
    | hs => (
        true,
        List.mapi(
          (i, _) =>
            switch (context(~core, ~dynamics, program, i + 1)) {
            | Ok(ctx) => describe(i + 1, ctx)
            | Error(why) => Printf.sprintf("Hole %d: %s", i + 1, why)
            },
          hs,
        )
        |> String.concat("\n\n"),
        None,
      )
    }
  | CheckProof(n, text) =>
    switch (
      Stdlib.Result.bind(context(~core, ~dynamics, program, n), ctx =>
        check(~core, ~program, ctx, text)
      )
    ) {
    | Ok((_, proves, report)) => (proves, report, None)
    | Error(why) => (false, why, None)
    }
  | FillProof(n, text) =>
    switch (context(~core, ~dynamics, program, n)) {
    | Error(why) => (false, why, None)
    | Ok(ctx) =>
      switch (check(~core, ~program, ctx, text)) {
      | Error(why) => (false, why, None)
      | Ok((proof, proves, report)) =>
        let zipper =
          Haz3lcore.EditorTransform.apply_patch(
            zipper,
            Haz3lcore.EditorTransform.mk_proof_patch(
              ~target_id=ctx.hole.id,
              proof,
            ),
          );
        (
          proves,
          "Written into the program. " ++ report,
          Some({
            ...cell,
            editor: {
              ...cell.editor,
              editor: Haz3lcore.Editor.Model.mk(~root=Exp, zipper),
            },
          }),
        );
      }
    }
  };
};
