open Util;
open Language;
open Poly;

/* The write box's quick search: the steps that take a row's goal to the
 * goal the user typed, found by breadth-first search, so the fewest steps
 * win.
 *
 * Its moves are steps a user could write: `step`, at the goal's root or
 * through a focus on a part that can step; reflexivity; each fact, theorem
 * and axiom in either direction, at each part it rewrites; and `algebra`
 * of a part with a variable in it to the typed part, when the algebra
 * check says they're equal. Arithmetic with no variables is left to
 * `step`. The checker verifies only Int arithmetic over Int variables, so
 * an `algebra` step with a call in it leaves its equation as an
 * obligation.
 * Each candidate is checked by the checker against the goal it starts
 * from, not by checking the theorem again, so a step found is a step the
 * checker accepts.
 *
 * Moves act only inside the smallest part of the goal that differs from
 * the typed goal: a step elsewhere would change something already right.
 * A rule can rewrite most goals somehow (`rewrite <- not_not` turns any
 * boolean `b` into `!!b`), so a rule's step is kept only if it shrinks
 * that part; a `step`, which may unfold a definition first, is kept
 * either way.
 *
 * The search goes `max_depth` steps deep, one more on a small goal, and
 * checks at most `max_checks` candidates. Writing out steps, it goes one
 * step deep: the box writes no step the user didn't type. It runs in
 * slices (`run`), so a slow check never holds up typing for longer than
 * one candidate takes. */

module Picker = StepperPicker;

let max_depth = 3;
/* A goal of at most this many nodes is searched one step deeper. */
let small_goal = 15;
let max_checks = 300;
/* The parts a step may act on, nearest the differing part first. */
let max_parts = 40;

/* A goal the search reached, the steps that reach it, in order, and the
 * size of its part that differs from the typed goal. */
[@deriving (show({with_path: false}), sexp, yojson)]
type node = {
  goal: Exp.t,
  steps: list(Proof.t),
  differs: int,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type move =
  | Eval
  | Rule;

[@deriving (show({with_path: false}), sexp, yojson)]
type status =
  | Searching
  /* No steps when the typed goal is the goal already. */
  | Found(list(Proof.t))
  | NotFound;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  /* The typed goal, without ascriptions, as every goal is compared. */
  wanted: Exp.t,
  status,
  /* The node being expanded, with its candidate steps not yet checked. */
  expanding: option((node, list((move, Proof.t)))),
  /* The nodes to expand at this depth, and those reached for the next. */
  frontier: list(node),
  next: list(node),
  /* Every goal reached, in the form `wanted` is in. */
  seen: list(Exp.t),
  checks: int,
  depth: int,
};

/* Goals compared up to ascriptions and the names of binders. */
let same = Equality.ignoring_ascriptions.exp;

/* A goal as it is compared: without ascriptions, and with a negated
 * number read as the negative number. Evaluation gives `-3` as a number,
 * while typing `-3` gives the negation of `3`. */
let comparable = (e: Exp.t): Exp.t =>
  DHExp.strip_ascriptions(e)
  |> Exp.map_term(~f_exp=(continue, e) => {
       let (term, rewrap) = Exp.unwrap(e);
       switch (term) {
       | UnOp(Int(Minus), {term: Atom(Int(n)), _}) =>
         rewrap(Atom(Int(Bigint.neg(n))))
       | _ => continue(e)
       };
     });

/* Whether `e` is the typed goal: as written, or else once the scope's
 * definitions they name are unfolded where they differ. */
let reaches = (goal: Picker.Goal.t, e: Exp.t, wanted: Exp.t): bool =>
  same(e, wanted)
  || MatchExp.equal_modulo(~defs=Picker.Goal.env(goal), e, wanted);

let rec unwrap = (e: Exp.t): Exp.t =>
  switch (Exp.term_of(e)) {
  | Parens(inner)
  | Asc(inner, _) => unwrap(inner)
  | _ => e
  };

/* The smallest part of `goal` holding every difference from `wanted`,
 * with the matching part of `wanted`, or None if they are the same. A
 * part in parens comes with its parens, where the checker's step on it
 * starts. */
let rec differing = (goal: Exp.t, wanted: Exp.t): option((Exp.t, Exp.t)) => {
  let (g, w) = (unwrap(goal), unwrap(wanted));
  let (gs, ws) = (Occurrence.children(g), Occurrence.children(w));
  let pairs =
    Exp.cls_of_term(Exp.term_of(g)) == Exp.cls_of_term(Exp.term_of(w))
    && List.length(gs) == List.length(ws)
      ? Some(List.zip_exn(gs, ws)) : None;
  switch (
    same(g, w),
    Option.map(~f=List.filter(~f=((a, b)) => !same(a, b)), pairs),
  ) {
  | (true, _) => None
  | (false, Some([(a, b)])) =>
    switch (differing(a, b)) {
    | Some(_) as part => part
    | None => Some((goal, wanted))
    }
  | (false, _) => Some((goal, wanted))
  };
};

let rec size = (e: Exp.t): int =>
  List.fold_left(
    ~f=(n, child) => n + size(child),
    ~init=1,
    Occurrence.children(e),
  );

let differs = (compared: Exp.t, wanted: Exp.t): int =>
  differing(compared, wanted)
  |> Option.value_map(~default=0, ~f=((p, _)) => size(p));

let start = (goal: Picker.Goal.t, ~wanted: Exp.t): t => {
  let wanted = comparable(wanted);
  let here = comparable(goal.written);
  {
    wanted,
    status: reaches(goal, here, wanted) ? Found([]) : Searching,
    expanding: None,
    frontier: [
      {
        goal: goal.written,
        steps: [],
        differs: differs(here, wanted),
      },
    ],
    next: [],
    seen: [here],
    checks: 0,
    depth:
      goal.settings.evaluation.write_out_steps
        ? 1 : size(here) <= small_goal ? max_depth + 1 : max_depth,
  };
};

/* What can be checked as `rewrite`: arithmetic, with no booleans in it.
 * The algebra check treats a call as an unknown, the same unknown for
 * equal calls, so `0 + len(ys)` is `len(ys)`. */
let rec arithmetic = (e: Exp.t): bool =>
  switch (Exp.term_of(e)) {
  | Atom(Int(_))
  | Var(_)
  | Ap(_) => true
  | Parens(e)
  | UnOp(Int(Minus), e) => arithmetic(e)
  | BinOp(Int(Plus | Minus | Times | Divide | Power), a, b) =>
    arithmetic(a) && arithmetic(b)
  | _ => false
  };

/* A goal prepared for proposing steps on: as written, and without
 * ascriptions, as it is compared. */
module Prepared = {
  type t = {
    written: Exp.t,
    compared: Exp.t,
  };

  let mk = (written: Exp.t): t => {
    written,
    compared: comparable(written),
  };

  /* The parts of `root` a step may act on, nearest it first, each as it
   * is compared. */
  let parts = (p: t, root: Exp.t): list((Exp.t, Exp.t)) => {
    let rec breadth_first = (found, n, queue) =>
      switch (queue) {
      | [] => List.rev(found)
      | _ when n >= max_parts => List.rev(found)
      | [e, ...rest] =>
        breadth_first([e, ...found], n + 1, rest @ Occurrence.children(e))
      };
    breadth_first([], 0, [root])
    |> List.filter_map(~f=node =>
         Exp.find_by_id(Exp.rep_id(node), p.compared)
         |> Option.map(~f=compared => (node, compared))
       );
  };

  /* `step` at `node`: through a focus, unless `node` is the whole goal. */
  let at = (p: t, node: Exp.t, step: TermBase.Proof.term): option(Proof.t) =>
    Id.equal(Exp.rep_id(node), Exp.rep_id(p.written))
      ? Some(Proof.fresh(step))
      : Picker.EvalSteps.checker_idx(p.written, node)
        |> Option.map(~f=index =>
             Proof.fresh(
               Picker.Terms.focus(~selector=node, ~index, Proof.fresh(step)),
             )
           );
};

/* The uses of the scope's facts, theorems and axioms at `parts`, or of
 * the one named `only`: each as the part, and the step rewriting it,
 * kept if `keep` takes the part and what the rule rewrites it to. */
let rule_uses =
    (
      ~only: option(string)=None,
      ~keep: (Exp.t, Exp.t) => bool,
      goal: Picker.Goal.t,
      p: Prepared.t,
      parts: list((Exp.t, Exp.t)),
    )
    : list((Exp.t, Proof.t)) => {
  let statics = ProofStatics.of_info_map(Picker.Goal.info_map(goal));
  let (scope, axioms) = Picker.scope_and_axioms(goal.ctx);
  scope
  @ axioms
  |> List.filter(~f=(entry: ProofCtx.entry) =>
       !entry.is_captured
       && !entry.is_shadowed
       && !Picker.Goal.cites_refuted(goal, entry.name)
       && !String.equal(entry.name, "refl")
       && Option.value_map(only, ~default=true, ~f=String.equal(entry.name))
     )
  |> List.concat_map(~f=(entry: ProofCtx.entry) => {
       let rule =
         ProofRule.BoolReading.apply(~statics=Some(statics), entry.rule);
       /* A side that is just one of the rule's variables matches every
        * part, as `rewrite <- and_true` rewrites any `a` to `a && true`.
        * Such a step only wraps the goal, so it isn't proposed. */
       let rec is_variable = (side: Exp.t) =>
         switch (Exp.term_of(side)) {
         | Parens(inner)
         | Asc(inner, _) => is_variable(inner)
         | Var(x) =>
           List.exists(
             ~f=
               (binding: Ctx.entry) =>
                 switch (binding) {
                 | VarEntry({name, _}) => String.equal(name, x)
                 | ConstructorEntry(_)
                 | TVarEntry(_)
                 | LivelitEntry(_)
                 | TheoremEntry(_) => false
                 },
             rule.bindings,
           )
         | _ => false
         };
       let (forward, backward) =
         switch (rule.conclusion) {
         | Equality(lhs, rhs) => (
             is_variable(lhs) ? None : Some(lhs),
             is_variable(rhs) ? None : Some(rhs),
           )
         | BoolFact(p) => (Some(p), None)
         | Other(_) => (None, None)
         };
       /* A part whose head can't match is passed over before matching. */
       let applies = (side, part: Exp.t) =>
         switch (side) {
         | Some(side) =>
           Exp.cls_of_term(Exp.term_of(unwrap(side)))
           == Exp.cls_of_term(Exp.term_of(unwrap(part)))
         | None => false
         };
       /* Forward first, as a rule is read. */
       let uses = (side, direction) =>
         List.filter_map(
           ~f=
             ((node, compared)) =>
               applies(side, compared)
                 ? {
                   let (l, r) =
                     ProofRule.Rewrite.apply(
                       ~statics,
                       ~env=Picker.Goal.env(goal),
                       rule,
                       compared,
                     );
                   switch (direction == Direction.Right ? r : l) {
                   | Some(result) when keep(node, result) =>
                     Prepared.at(
                       p,
                       node,
                       Picker.Terms.axiom_step(
                         ~direction,
                         ~equality=entry.name,
                       ),
                     )
                     |> Option.map(~f=step => (node, step))
                   | Some(_)
                   | None => None
                   };
                 }
                 : None,
           parts,
         );
       uses(forward, Right) @ uses(backward, Left);
     });
};

/* The steps to try from `from`, acting inside `part`, a part of it whose
 * typed replacement is `wanted_part`. */
let candidates =
    (
      ~algebra: (Exp.t, Exp.t) => bool,
      goal: Picker.Goal.t,
      ~wanted: Exp.t,
      from: node,
      (part: Exp.t, wanted_part: Exp.t),
    )
    : list((move, Proof.t)) => {
  let env = Picker.Goal.env(goal);
  let statics = ProofStatics.of_info_map(Picker.Goal.info_map(goal));
  let p = Prepared.mk(from.goal);
  let within = Prepared.parts(p, part);
  let part_compared =
    switch (within) {
    | [(_, compared), ..._] => compared
    | [] => comparable(part)
    };
  /* Whether rewriting `node` to `result` shrinks the part that differs
   * from the typed goal. Only the path down to `node` is rebuilt. */
  let shrinks = (node: Exp.t, result: Exp.t) => {
    let path =
      Picker.self_and_ancestors(Exp.rep_id(node), from.goal)
      |> Option.value(~default=[])
      |> List.map(~f=Exp.rep_id);
    let rewritten =
      Exp.map_term(
        ~f_exp=
          (cont, e) =>
            switch (Exp.rep_id(e)) {
            | id when Id.equal(id, Exp.rep_id(node)) => result
            | id when List.mem(path, id, ~equal=Id.equal) => cont(e)
            | _ => e
            },
        p.compared,
      );
    differs(rewritten, wanted) < from.differs;
  };
  let at = Prepared.at(p);
  let evals =
    Picker.EvalSteps.offered(~settings=goal.settings, ~env, from.goal)
    |> List.filter_map(~f=({source, _}: Picker.EvalSteps.offer) =>
         Exp.find_by_id(source, part)
         |> Option.bind(_, ~f=node => at(node, EvalStep))
       );
  let refls =
    MatchExp.find_refls(~statics, ~defs=env, part_compared)
    |> List.filter_map(~f=e => Exp.find_by_id(Exp.rep_id(e), part))
    |> List.filter_map(~f=node =>
         at(
           node,
           Picker.Terms.axiom_step(~direction=Right, ~equality="refl"),
         )
       );
  let rec has_variable = (e: Exp.t) =>
    switch (Exp.term_of(e)) {
    | Var(_) => true
    | _ => List.exists(~f=has_variable, Occurrence.children(e))
    };
  let algebraic =
    arithmetic(part)
    && has_variable(part)
    && arithmetic(wanted_part)
    && algebra(part_compared, wanted_part)
      ? Option.to_list(
          at(part, Picker.Terms.algebrite_step(~replacement=wanted_part)),
        )
      : [];
  let rules = rule_uses(~keep=shrinks, goal, p, within) |> List.map(~f=snd);
  List.map(~f=step => (Eval, step), evals)
  @ List.map(~f=step => (Rule, step), refls @ algebraic @ rules);
};

/* Searches on until `continue_` says to stop, the search ends, or it
 * reaches its bounds. `algebra` says whether two arithmetic expressions
 * are equal. */
let run =
    (
      ~continue_: unit => bool,
      ~algebra: (Exp.t, Exp.t) => bool,
      goal: Picker.Goal.t,
      search: t,
    )
    : t => {
  let rec loop = (t: t) =>
    switch (t.status, t.expanding, t.frontier) {
    | (Found(_) | NotFound, _, _) => t
    | (Searching, _, _) when !continue_() => t
    | (Searching, _, _) when t.checks >= max_checks => {
        ...t,
        status: NotFound,
      }
    | (Searching, Some((node, [(move, step), ...rest])), _) =>
      let t = {
        ...t,
        expanding: Some((node, rest)),
        checks: t.checks + 1,
      };
      let reached =
        Picker.outgoing(goal, node.goal, step)
        |> Option.map(~f=e => (e, comparable(e)));
      let steps = node.steps @ [step];
      switch (reached) {
      | Some((e, compared)) when !List.exists(~f=same(compared), t.seen) =>
        let differs =
          reaches(goal, compared, t.wanted)
            ? 0 : differs(compared, t.wanted);
        switch (differs, move) {
        | (0, _) =>
          loop({
            ...t,
            status: Found(steps),
          })
        | (_, Rule) when differs >= node.differs => loop(t)
        | (_, Eval | Rule) =>
          loop({
            ...t,
            seen: [compared, ...t.seen],
            next: [
              {
                goal: e,
                steps,
                differs,
              },
              ...t.next,
            ],
          })
        };
      | Some(_)
      | None => loop(t)
      };
    | (Searching, Some((_, [])) | None, [node, ...rest]) =>
      /* The differing part is found in the goal as compared, which keeps
       * the ids of all but the ascriptions it drops. */
      let compared = comparable(node.goal);
      let written = (part: Exp.t) =>
        Picker.self_and_ancestors(Exp.rep_id(part), compared)
        |> Option.value(~default=[])
        |> List.find_map(~f=e => Exp.find_by_id(Exp.rep_id(e), node.goal))
        |> Option.value(~default=node.goal);
      let steps =
        List.length(node.steps) >= t.depth
          ? []
          : differing(compared, t.wanted)
            |> Option.map(~f=((part, wanted)) => (written(part), wanted))
            |> Option.value_map(
                 ~default=[],
                 ~f=candidates(~algebra, goal, ~wanted=t.wanted, node),
               );
      loop({
        ...t,
        expanding: Some((node, steps)),
        frontier: rest,
      });
    | (Searching, Some((_, [])) | None, []) =>
      switch (t.next) {
      | [] => {
          ...t,
          status: NotFound,
        }
      | next =>
        loop({
          ...t,
          expanding: None,
          frontier: List.rev(next),
          next: [],
        })
      }
    };
  loop(search);
};

/* What the box lists under itself: the goal each use of a rule would
 * give. With a rule named, every use of that rule; otherwise every use of
 * any rule. With part of the goal selected, only the uses inside it. Run
 * in slices, as the search is. */
module Suggestions = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type suggestion = {
    step: Proof.t,
    /* The part of the goal it acts on. */
    part: Exp.t,
    /* The goal it gives. */
    result: Exp.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    rule: option(string),
    /* The selected part, by its id in the written goal. */
    at: option(Id.t),
    /* The uses not yet checked, None until they are listed. */
    pending: option(list((Exp.t, Proof.t))),
    found: list(suggestion),
  };

  let start = (~rule: option(string), ~at: option(Id.t)): t => {
    rule,
    at,
    pending: None,
    found: [],
  };

  let is_done = (t: t): bool => t.pending == Some([]);

  let run = (~continue_: unit => bool, goal: Picker.Goal.t, t: t): t => {
    let rec loop = (t: t) =>
      switch (t.pending) {
      | Some([]) => t
      | _ when !continue_() => t
      | None =>
        let p = Prepared.mk(goal.written);
        loop({
          ...t,
          pending:
            Some(
              rule_uses(
                ~only=t.rule,
                ~keep=(_, _) => true,
                goal,
                p,
                Prepared.parts(
                  p,
                  Option.bind(t.at, ~f=id => Exp.find_by_id(id, goal.written))
                  |> Option.value(~default=goal.written),
                ),
              ),
            ),
        });
      | Some([(part, step), ...rest]) =>
        let found =
          switch (Picker.outgoing(goal, goal.written, step)) {
          | Some(result) =>
            t.found
            @ [
              {
                step,
                part,
                result,
              },
            ]
          | None => t.found
          };
        loop({
          ...t,
          pending: Some(rest),
          found,
        });
      };
    loop(t);
  };
};

/* The rules the box knows by name: the scope's facts and theorems, then
 * the builtin axioms. */
let names =
    (goal: Picker.Goal.t)
    : list(
        (
          string,
          [
            | `Scope
            | `Axiom
          ],
        ),
      ) => {
  let (scope, axioms) = Picker.scope_and_axioms(goal.ctx);
  let named = (kind, entries) =>
    entries
    |> List.filter(~f=(entry: ProofCtx.entry) =>
         !entry.is_captured
         && !entry.is_shadowed
         && !Picker.Goal.cites_refuted(goal, entry.name)
       )
    |> List.map(~f=(entry: ProofCtx.entry) => (entry.name, kind));
  named(`Scope, scope) @ named(`Axiom, axioms);
};

/* What the box lists under itself. Typing the start of a name lists the
 * names it completes to, unless steps are written out; a name in full, or
 * any other text, lists the rules' uses. */
[@deriving (show({with_path: false}), sexp, yojson)]
type item =
  | Complete(string)
  | Use(Suggestions.suggestion);

let items =
    (
      goal: Picker.Goal.t,
      ~typed_name: option(string),
      suggestions: option(Suggestions.t),
    )
    : list(item) => {
  let completions =
    switch (typed_name) {
    | Some(_) when goal.settings.evaluation.write_out_steps => []
    | Some(start) =>
      names(goal)
      |> List.map(~f=fst)
      |> List.filter(~f=name =>
           !String.equal(name, start)
           && String.length(name) > String.length(start)
           && String.equal(
                String.sub(name, ~pos=0, ~len=String.length(start)),
                start,
              )
         )
      |> ListUtil.dedup_f(String.equal)
    | None => []
    };
  switch (completions, suggestions) {
  | ([_, ..._], _) => List.map(~f=name => Complete(name), completions)
  | ([], Some(s)) => List.map(~f=use => Use(use), s.found)
  | ([], None) => []
  };
};

/* `steps`, taken one after another from `goal`, with each focus's
 * selector generalized as the stepper's own picks are (`Picker.target`):
 * as much of it replaced by `$e` as still selects the same part of the
 * goal the focus starts from. */
let generalized = (goal: Picker.Goal.t, steps: list(Proof.t)): list(Proof.t) => {
  let statics = ProofStatics.of_info_map(Picker.Goal.info_map(goal));
  let env = Picker.Goal.env(goal);
  let generalize = (from: Exp.t, step: Proof.t): Proof.t =>
    switch (step.term) {
    | Focus({ordinal, selector, body}) =>
      let index =
        (
          ordinal
          |> Option.map(~f=Annotated.term_of)
          |> Option.value(~default=1)
        )
        - 1;
      switch (
        Occurrence.nth_env_target(~statics, ~env, selector, index, from)
      ) {
      | Some(part) =>
        let (selector, index) =
          Occurrence.generalize(
            ~statics,
            ~env,
            ~index,
            selector,
            Exp.rep_id(part),
            from,
          );
        Proof.fresh(Picker.Terms.focus(~selector, ~index, body));
      | None => step
      };
    | EmptyHole
    | Invalid(_)
    | MultiHole(_)
    | Seq(_)
    | Parens(_)
    | EvalStep
    | AxiomStep(_)
    | AlgebriteStep(_)
    | Forall(_)
    | Funext(_)
    | Assume(_)
    | Have(_)
    | Generalize(_)
    | Revert(_)
    | Alias(_)
    | Cases(_)
    | Induction(_) => step
    };
  let (_, written) =
    List.fold_left(
      ~f=
        ((from, written), step) => {
          let step =
            Option.value_map(from, ~default=step, ~f=from =>
              generalize(from, step)
            );
          let next =
            Option.bind(from, ~f=from => Picker.outgoing(goal, from, step));
          (next, written @ [step]);
        },
      ~init=(Some(goal.written), []),
      steps,
    );
  written;
};

/* Writing at a hole with a step after it, as in `?; from 1 + 4 do step
 * end`. A hole passes its goal through, so that step was written against
 * the hole's goal, and steps written in the hole change the goal it acts
 * on. The steps are written in place of the hole when the step after
 * still checks once they are taken. Otherwise the search looks for steps
 * back to the hole's goal, unless steps are written out: found, they are
 * written too; not found, the hole stays after the written steps,
 * marking the gap still to bridge. */
module Bridge = {
  /* The step after `hole` in its chain in `proof`. */
  let step_after = (proof: Proof.t, hole: Proof.t): option(Proof.t) => {
    let rec chain = (p: Proof.t): list(Proof.t) =>
      switch (StepperRows.split(p)) {
      | (head, Some(rest)) => [head, ...chain(rest)]
      | (head, None) => [head]
      };
    let found = ref(None);
    let _: Proof.t =
      Proof.map_term(
        ~f_proof=
          (continue, p) => {
            switch (p.term, found^) {
            | (Seq(_), None) =>
              let steps = chain(p);
              switch (
                List.findi(steps, ~f=(_, s) =>
                  Id.equal(Proof.rep_id(s), Proof.rep_id(hole))
                )
                |> Option.map(~f=fst)
              ) {
              | Some(i) => found := List.nth(steps, i + 1)
              | None => ()
              };
            | _ => ()
            };
            continue(p);
          },
        proof,
      );
    found^;
  };

  /* Bounds the search a write runs before the write lands. */
  let max_checks = 40;

  let rec is_rewrite = (step: Proof.t) =>
    switch (step.term) {
    | EvalStep
    | AxiomStep(_)
    | AlgebriteStep(_) => true
    | Focus({body, _})
    | Parens(body) => is_rewrite(StepperRows.unparen(body))
    | EmptyHole
    | Invalid(_)
    | MultiHole(_)
    | Seq(_)
    | Forall(_)
    | Funext(_)
    | Assume(_)
    | Have(_)
    | Generalize(_)
    | Revert(_)
    | Alias(_)
    | Cases(_)
    | Induction(_) => false
    };

  /* What writing `steps` at `row`, whose goal is `goal`, writes: the steps
   * alone, unless `row` is a hole with a step after it in `proof`. */
  let steps =
      (
        ~algebra: (Exp.t, Exp.t) => bool,
        ~proof: Proof.t,
        goal: Picker.Goal.t,
        row: StepperRows.t,
        steps: list(Proof.t),
      )
      : list(Proof.t) => {
    let seq = steps =>
      switch (List.rev(steps)) {
      | [] => None
      | [last, ...earlier] =>
        Some(
          List.fold_left(
            ~f=(rest, step) => Proof.fresh(Seq(step, rest)),
            ~init=last,
            earlier,
          ),
        )
      };
    let after =
      row.kind == Hole && List.for_all(~f=is_rewrite, steps)
        ? step_after(proof, row.step) : None;
    switch (after, seq(steps)) {
    | (Some(next), Some(written)) =>
      let next_checks =
        Picker.check(goal, goal.written, Proof.fresh(Seq(written, next)))
        != None;
      switch (Picker.outgoing(goal, goal.written, written)) {
      | _ when next_checks => steps
      /* The steps back are the user's to write. */
      | Some(_) when goal.settings.evaluation.write_out_steps =>
        steps @ [Proof.fresh(EmptyHole)]
      | Some(reached) =>
        let from =
          Picker.Goal.mk(~settings=goal.settings, ~ctx=goal.ctx, reached);
        let checks = ref(0);
        let search =
          run(
            ~continue_=
              () => {
                incr(checks);
                checks^ <= max_checks;
              },
            ~algebra,
            from,
            start(from, ~wanted=goal.written),
          );
        switch (search.status) {
        | Found(more) => steps @ more
        | Searching
        | NotFound => steps @ [Proof.fresh(EmptyHole)]
        };
      | None => steps @ [Proof.fresh(EmptyHole)]
      };
    | (None, _)
    | (_, None) => steps
    };
  };
};
