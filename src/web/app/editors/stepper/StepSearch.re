open Util;
open Language;

/* The write box's quick search: the steps that take a row's goal to the
 * goal the user typed, found by breadth-first search, so the fewest steps
 * win.
 *
 * Its moves are steps a user could write: `eval`, at the goal's root or
 * through a focus on a part that can step; reflexivity; each fact, theorem
 * and axiom in either direction, at each part it rewrites; and `rewrite`
 * of a part with a variable in it to the typed part, when the algebra
 * check says they're equal. The checker trusts the algebra check rather
 * than verify it, so arithmetic with no variables is left to eval.
 * Each candidate is checked by the checker against the goal it starts
 * from, not by checking the theorem again, so a step found is a step the
 * checker accepts.
 *
 * Moves act only inside the smallest part of the goal that differs from
 * the typed goal: a step elsewhere would change something already right.
 * A rule can rewrite most goals somehow (`axiomrev not_not` turns any
 * boolean `b` into `!!b`), so a rule's step is kept only if it shrinks
 * that part; an eval step, which may unfold a definition first, is kept
 * either way.
 *
 * The search goes `max_depth` steps deep and checks at most `max_checks`
 * candidates. It runs in slices (`run`), so a slow check never holds up
 * typing for longer than one candidate takes. */

module Picker = StepperPicker;

let max_depth = 3;
let max_checks = 300;
/* The parts a step may act on, nearest the differing part first. A goal
 * an eval step has unfolded a definition into has thousands. */
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
  /* The typed goal, with the scope's definitions substituted in, as every
   * goal is compared: it names a function an eval step may have inlined. */
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
};

/* Goals compared, as substituted: an eval step inlines a definition, and
 * substitution renames the binders of what it inlines to avoid capture,
 * differently in different goals. */
let same = Equality.ignoring_ascriptions.exp;

let comparable = (goal: Picker.Goal.t, e: Exp.t): Exp.t =>
  e |> Substitution.in_exp(Picker.Goal.env(goal)) |> DHExp.strip_ascriptions;

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
      ? Some(List.combine(gs, ws)) : None;
  switch (
    same(g, w),
    Option.map(List.filter(((a, b)) => !same(a, b)), pairs),
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
  List.fold_left((n, child) => n + size(child), 1, Occurrence.children(e));

let differs = (compared: Exp.t, wanted: Exp.t): int =>
  differing(compared, wanted)
  |> Option.fold(~none=0, ~some=((p, _)) => size(p));

let start = (goal: Picker.Goal.t, ~wanted: Exp.t): t => {
  let wanted = comparable(goal, wanted);
  let here = comparable(goal, goal.written);
  {
    wanted,
    status: same(here, wanted) ? Found([]) : Searching,
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
  };
};

/* What can be checked as `rewrite`: arithmetic, with no booleans in it. */
let rec arithmetic = (e: Exp.t): bool =>
  switch (Exp.term_of(e)) {
  | Atom(Int(_))
  | Var(_) => true
  | Parens(e)
  | UnOp(Int(Minus), e) => arithmetic(e)
  | BinOp(Int(Plus | Minus | Times | Divide | Power), a, b) =>
    arithmetic(a) && arithmetic(b)
  | _ => false
  };

/* The steps to try from `from`, acting inside `part`, a part of it whose
 * typed replacement is `wanted`. */
/* A goal prepared for proposing steps on. Substituting is slow where the
 * scope's functions are large, so it is done once, and a part's
 * substituted form is found in the result by its id. */
module Prepared = {
  type t = {
    written: Exp.t,
    substituted: Exp.t,
    compared: Exp.t,
  };

  let mk = (goal: Picker.Goal.t, written: Exp.t): t => {
    let substituted = Substitution.in_exp(Picker.Goal.env(goal), written);
    {
      written,
      substituted,
      compared: DHExp.strip_ascriptions(substituted),
    };
  };

  /* The parts of `root` a step may act on, nearest it first, each with
   * its substituted form. A name the substitution replaced, a
   * function's, is left out: no rule rewrites a function's name, and its
   * definition is slow to match. */
  let parts = (p: t, root: Exp.t): list((Exp.t, Exp.t)) => {
    let rec breadth_first = (found, n, queue) =>
      switch (queue) {
      | [] => List.rev(found)
      | _ when n >= max_parts => List.rev(found)
      | [e, ...rest] =>
        breadth_first([e, ...found], n + 1, rest @ Occurrence.children(e))
      };
    breadth_first([], 0, [root])
    |> List.filter_map(node =>
         Exp.find_by_id(Exp.rep_id(node), p.compared)
         |> Option.map(substituted => (node, substituted))
       );
  };

  /* `step` at `node`: through a focus, unless `node` is the whole goal. */
  let at =
      (goal: Picker.Goal.t, p: t, node: Exp.t, step: TermBase.Proof.term)
      : option(Proof.t) =>
    Exp.rep_id(node) == Exp.rep_id(p.written)
      ? Some(Proof.fresh(step))
      : Picker.EvalSteps.checker_idx(
          ~env=Picker.Goal.env(goal),
          ~substituted=p.substituted,
          node,
        )
        |> Option.map(index =>
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
  let info_map = Picker.Goal.info_map(goal);
  let (scope, axioms) = Picker.scope_and_axioms(goal.ctx);
  scope
  @ axioms
  |> List.filter((entry: ProofCtx.entry) =>
       !entry.is_captured
       && !entry.is_shadowed
       && entry.name != "refl_eq"
       && Option.fold(~none=true, ~some=(==)(entry.name), only)
     )
  |> List.concat_map((entry: ProofCtx.entry) => {
       let rule =
         ProofRule.BoolReading.apply(~info_map=Some(info_map), entry.rule);
       /* A side that is just one of the rule's variables matches every
        * part, as `axiomrev and_true` rewrites any `a` to `a && true`.
        * Such a step only wraps the goal, so it isn't proposed. */
       let rec is_variable = (side: Exp.t) =>
         switch (Exp.term_of(side)) {
         | Parens(inner)
         | Asc(inner, _) => is_variable(inner)
         | Var(x) =>
           List.exists(
             (binding: Ctx.entry) =>
               switch (binding) {
               | VarEntry({name, _}) => name == x
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
       /* Matching is slow on a part holding the scope's definitions, so a
        * part whose head can't match is passed over first. */
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
           ((node, substituted)) =>
             applies(side, substituted)
               /* The part is substituted already, and matching would
                * substitute it again. */
               ? {
                 let (l, r) =
                   ProofRule.Rewrite.apply(
                     ~info_map,
                     ~env=Environment.empty,
                     rule,
                     substituted,
                   );
                 switch (direction == Direction.Right ? r : l) {
                 | Some(result) when keep(node, result) =>
                   Prepared.at(
                     goal,
                     p,
                     node,
                     Picker.Terms.axiom_step(
                       ~direction,
                       ~equality=entry.name,
                     ),
                   )
                   |> Option.map(step => (node, step))
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
  let info_map = Picker.Goal.info_map(goal);
  let p = Prepared.mk(goal, from.goal);
  let within = Prepared.parts(p, part);
  let part_substituted =
    switch (within) {
    | [(_, substituted), ..._] => substituted
    | [] => part |> Substitution.in_exp(env) |> DHExp.strip_ascriptions
    };
  /* Whether rewriting `node` to `result` shrinks the part that differs
   * from the typed goal. Only the path down to `node` is rebuilt, its ids
   * the same in the written goal, which is small, as in the substituted
   * one, which holds the definitions. */
  let shrinks = (node: Exp.t, result: Exp.t) => {
    let path =
      Picker.self_and_ancestors(Exp.rep_id(node), from.goal)
      |> Option.value(~default=[])
      |> List.map(Exp.rep_id);
    let rewritten =
      Exp.map_term(
        ~f_exp=
          (cont, e) =>
            switch (Exp.rep_id(e)) {
            | id when id == Exp.rep_id(node) => result
            | id when List.mem(id, path) => cont(e)
            | _ => e
            },
        p.compared,
      );
    differs(rewritten, wanted) < from.differs;
  };
  let at = Prepared.at(goal, p);
  let evals =
    Picker.EvalSteps.offered(~settings=goal.settings, ~env, from.goal)
    |> List.filter_map(((_, source)) =>
         Exp.find_by_id(source, part)
         |> Option.bind(_, node => at(node, EvalStep))
       );
  /* Found with the definitions substituted in, as the checker matches;
   * substitution keeps the ids of the equalities themselves. */
  let refls =
    MatchExp.find_refls(~info_map, ~env, part_substituted)
    |> List.filter_map(e => Exp.find_by_id(Exp.rep_id(e), part))
    |> List.filter_map(node =>
         at(
           node,
           Picker.Terms.axiom_step(~direction=Right, ~equality="refl_eq"),
         )
       );
  let rec has_variable = (e: Exp.t) =>
    switch (Exp.term_of(e)) {
    | Var(_) => true
    | _ => List.exists(has_variable, Occurrence.children(e))
    };
  let algebraic =
    arithmetic(part)
    && has_variable(part)
    && arithmetic(wanted_part)
    && algebra(part_substituted, wanted_part)
      ? Option.to_list(
          at(part, Picker.Terms.algebrite_step(~replacement=wanted_part)),
        )
      : [];
  let rules = rule_uses(~keep=shrinks, goal, p, within) |> List.map(snd);
  List.map(step => (Eval, step), evals)
  @ List.map(step => (Rule, step), refls @ algebraic @ rules);
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
        |> Option.map(e => (e, comparable(goal, e)));
      let steps = node.steps @ [step];
      switch (reached) {
      | Some((e, compared)) when !List.exists(same(compared), t.seen) =>
        let differs = differs(compared, t.wanted);
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
      /* The differing part is found in the substituted goal, which keeps
       * the ids of all but what it inlined. */
      let compared = comparable(goal, node.goal);
      let written = (part: Exp.t) =>
        Picker.self_and_ancestors(Exp.rep_id(part), compared)
        |> Option.value(~default=[])
        |> List.find_map(e => Exp.find_by_id(Exp.rep_id(e), node.goal))
        |> Option.value(~default=node.goal);
      let steps =
        List.length(node.steps) >= max_depth
          ? []
          : differing(compared, t.wanted)
            |> Option.map(((part, wanted)) => (written(part), wanted))
            |> Option.fold(
                 ~none=[],
                 ~some=candidates(~algebra, goal, ~wanted=t.wanted, node),
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
        let p = Prepared.mk(goal, goal.written);
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
                  Option.bind(t.at, id => Exp.find_by_id(id, goal.written))
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
    |> List.filter((entry: ProofCtx.entry) =>
         !entry.is_captured && !entry.is_shadowed
       )
    |> List.map((entry: ProofCtx.entry) => (entry.name, kind));
  named(`Scope, scope) @ named(`Axiom, axioms);
};

/* What the box lists under itself. Typing the start of a name lists the
 * names it completes to; a name in full, or any other text, lists the
 * rules' uses. */
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
    | Some(start) =>
      names(goal)
      |> List.map(fst)
      |> List.filter(name =>
           name != start
           && String.length(name) > String.length(start)
           && String.sub(name, 0, String.length(start)) == start
         )
      |> ListUtil.dedup_f((==))
    | None => []
    };
  switch (completions, suggestions) {
  | ([_, ..._], _) => List.map(name => Complete(name), completions)
  | ([], Some(s)) => List.map(use => Use(use), s.found)
  | ([], None) => []
  };
};
