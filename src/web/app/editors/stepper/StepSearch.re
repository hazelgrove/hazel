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
    |> List.filter_map(node =>
         Exp.find_by_id(Exp.rep_id(node), p.compared)
         |> Option.map(compared => (node, compared))
       );
  };

  /* `step` at `node`: through a focus, unless `node` is the whole goal. */
  let at = (p: t, node: Exp.t, step: TermBase.Proof.term): option(Proof.t) =>
    Exp.rep_id(node) == Exp.rep_id(p.written)
      ? Some(Proof.fresh(step))
      : Picker.EvalSteps.checker_idx(p.written, node)
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
           ((node, compared)) =>
             applies(side, compared)
               ? {
                 let (l, r) =
                   ProofRule.Rewrite.apply(
                     ~info_map,
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
  let at = Prepared.at(p);
  let evals =
    Picker.EvalSteps.offered(~settings=goal.settings, ~env, from.goal)
    |> List.filter_map(((_, source)) =>
         Exp.find_by_id(source, part)
         |> Option.bind(_, node => at(node, EvalStep))
       );
  let refls =
    MatchExp.find_refls(~info_map, ~defs=env, part_compared)
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
    && algebra(part_compared, wanted_part)
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
        |> Option.map(e => (e, comparable(e)));
      let steps = node.steps @ [step];
      switch (reached) {
      | Some((e, compared)) when !List.exists(same(compared), t.seen) =>
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
