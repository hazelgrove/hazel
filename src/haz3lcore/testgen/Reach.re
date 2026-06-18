open Util;
open Language;

/* Reachability analysis for test input generation (Phase 2).
 *
 * Given a marked "reach point" (a node id), compute the PATH CONDITION that
 * must hold for execution to reach it: the conjunction of the enclosing `if`
 * guards from the program root down to the node (positive in a then-branch,
 * negated in an else-branch), together with the `let`-bindings in scope.
 *
 * Inputs are the variables in scope at the node (function parameters and
 * program-level free variables), treated as free SMT variables — i.e. this is
 * a per-function / intraprocedural analysis; call sites are ignored. A
 * satisfying assignment is an input that reaches the point; UNSAT means the
 * point is unreachable for ANY input, i.e. dead code (sound regardless of how
 * an enclosing function is actually called).
 *
 * Web-free and solver-agnostic: builds an SMT-LIB2 script (reusing
 * ConstraintGen + TestGen) for the same backends as the predicate-local
 * TestGen feature. The marked node's reach data is computed in
 * ProjectorInfo.mk_info (which has the whole-program statics map) and stashed
 * in ProjectorBase.info; the view solves it on demand. */

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  /* Signed boolean conditions along the path (then ⇒ cond, else ⇒ !cond),
   * conjoined to form the path condition. */
  guards: list(Exp.t),
  /* `let`-bindings in scope at the reach point, as (var, definition). */
  lets: list((string, Exp.t)),
  /* Base type of every variable referenced (inputs + let-bound), for SMT
   * declarations. Variables whose type isn't a base type are omitted (and
   * any guard/def referencing them is dropped, marking the result
   * incomplete). */
  var_sorts: list((string, Atom.cls)),
  /* The variables to report as the answer: in-scope inputs (parameters /
   * program-free vars), i.e. var_sorts minus the let-bound names. */
  inputs: list(string),
  /* False if an unsupported construct (e.g. `match`) sat on the path, so a
   * SAT result can't be trusted as "reachable" (but UNSAT is still sound). */
  complete: bool,
};

/* ===================== path-condition extraction ===================== */

let contains = (id: Id.t, e: Exp.t): bool =>
  Option.is_some(Exp.find_by_id(id, e));

let rec simple_binder = (p: Pat.t): option(string) =>
  switch (p.term) {
  | Var(x) => Some(x)
  | Parens(inner) => simple_binder(inner)
  | _ => None
  };

let negate = (c: Exp.t): Exp.t => Exp.fresh(UnOp(Bool(Not), c));

let false_exp = (): Exp.t => Exp.fresh(Atom(Bool(false)));

/* How a scrutinee relates to a (literal/wildcard) match pattern. */
type pmatch =
  | PAlways /* wildcard: matches anything */
  | PCond(Exp.t) /* literal: matches iff scrut == lit */
  | PUnknown; /* pattern we can't express (constructor/tuple/var-binding) */

let rec pat_matches = (scrut: Exp.t, p: Pat.t): pmatch =>
  switch (p.term) {
  | Wild => PAlways
  | Atom(a) =>
    PCond(Exp.fresh(BinOp(Poly(Equals), scrut, Exp.fresh(Atom(a)))))
  | Parens(inner) => pat_matches(scrut, inner)
  | _ => PUnknown
  };

/* Process one (ancestor, child-on-path) step, threading the accumulator. */
let step =
    (
      ~map: Statics.Map.t,
      a_id: Id.t,
      child_id: Id.t,
      (guards, lets, complete): (
        list(Exp.t),
        list((string, Exp.t)),
        bool,
      ),
    )
    : (list(Exp.t), list((string, Exp.t)), bool) =>
  switch (Statics.Map.lookup_exp(a_id, map)) {
  | None => (guards, lets, complete)
  | Some({user_term, _}) =>
    switch (user_term.term) {
    | If(c, t, f) =>
      if (contains(child_id, t)) {
        (guards @ [c], lets, complete);
      } else if (contains(child_id, f)) {
        (guards @ [negate(c)], lets, complete);
      } else {
        (
          guards,
          lets,
          complete /* child is in the condition itself */
        );
      }
    | Let(p, def, body) when contains(child_id, body) =>
      switch (simple_binder(p)) {
      | Some(v) => (guards, lets @ [(v, def)], complete)
      | None => (guards, lets, false)
      }
    | Let(_) => (guards, lets, complete) /* child is in the definition */
    | Match(scrut, rules) when !contains(child_id, scrut) =>
      /* The reach point is in some arm i: it's reached when scrut didn't match
       * arms 0..i-1 and does match arm i. An earlier wildcard makes arm i dead
       * (false guard); a pattern we can't express drops its guard (incomplete,
       * but UNSAT stays sound). */
      let rec find = (i, rs) =>
        switch (rs) {
        | [] => None
        | [(_p, body), ...rest] =>
          contains(child_id, body) ? Some(i) : find(i + 1, rest)
        };
      switch (find(0, rules)) {
      | None => (guards, lets, complete)
      | Some(target_i) =>
        List.fold_left(
          ((gs, comp), (j, (p, _body))) =>
            if (j > target_i) {
              (gs, comp);
            } else if (j == target_i) {
              switch (pat_matches(scrut, p)) {
              | PAlways => (gs, comp)
              | PCond(c) => (gs @ [c], comp)
              | PUnknown => (gs, false)
              };
            } else {
              switch (pat_matches(scrut, p)) {
              | PAlways => (gs @ [false_exp()], comp)
              | PCond(c) => (gs @ [negate(c)], comp)
              | PUnknown => (gs, false)
              };
            },
          (guards, complete),
          List.mapi((j, r) => (j, r), rules),
        )
        |> (((gs, comp)) => (gs, lets, comp))
      };
    | Match(_) => (guards, lets, complete) /* child is in the scrutinee */
    | Parens(_)
    | BinOp(_)
    | UnOp(_)
    | Ap(_)
    | Fun(_)
    | Seq(_)
    | Tuple(_) => (guards, lets, complete) /* no guard contributed */
    | _ => (guards, lets, false) /* unmodeled control flow */
    }
  };

/* Collect (name, id) of every variable reference within an expression. */
let vars_of = (e: Exp.t): list((string, Id.t)) => {
  let acc = ref([]);
  ignore(
    Exp.map_term(
      ~f_exp=
        (cont, e) => {
          switch (e.term) {
          | Var(x) => acc := [(x, Exp.rep_id(e)), ...acc^]
          | _ => ()
          };
          cont(e);
        },
      e,
    ),
  );
  List.rev(acc^);
};

let sort_of_var =
    (~ctx: Ctx.t, ~map: Statics.Map.t, id: Id.t): option(Atom.cls) =>
  switch (Statics.Map.lookup_exp(id, map)) {
  | Some({ty, _}) => Typ.is_ana_atom(Typ.weak_head_normalize(ctx, ty))
  | None => None
  };

let analyze = (target_id: Id.t, map: Statics.Map.t): option(t) =>
  switch (Statics.Map.lookup_exp(target_id, map)) {
  | None => None
  | Some(target) =>
    /* ancestors is [parent, …, root]; path top→down is [root, …, parent, target] */
    let path = List.rev(target.ancestors) @ [target_id];
    let rec walk = (nodes, acc) =>
      switch (nodes) {
      | []
      | [_] => acc
      | [a_id, child_id, ..._] =>
        walk(List.tl(nodes), step(~map, a_id, child_id, acc))
      };
    let (guards, all_lets, complete) = walk(path, ([], [], true));
    /* Keep only the lets actually needed: start from the guards' variables and
       close over let-definitions, dropping in-scope lets irrelevant to this
       reach point (otherwise they'd be declared/asserted, and an unreferenced
       one would be dropped at solve time and spuriously mark incompleteness). */
    let names = (es: list(Exp.t)): list(string) =>
      List.concat_map(vars_of, es)
      |> List.map(fst)
      |> List.sort_uniq(compare);
    let rec close = (needed: list(string)): list(string) => {
      let next =
        List.fold_left(
          (acc, (v, def)) => List.mem(v, acc) ? acc @ names([def]) : acc,
          needed,
          all_lets,
        )
        |> List.sort_uniq(compare);
      List.length(next) == List.length(needed) ? needed : close(next);
    };
    let needed = close(names(guards));
    let lets = List.filter(((v, _)) => List.mem(v, needed), all_lets);
    /* Resolve sorts for every variable referenced in guards and kept let-defs. */
    let referenced =
      List.concat_map(vars_of, guards)
      @ List.concat_map(((_, def)) => vars_of(def), lets);
    let var_sorts =
      List.fold_left(
        (acc, (name, id)) =>
          List.mem_assoc(name, acc)
            ? acc
            : (
              switch (sort_of_var(~ctx=target.ctx, ~map, id)) {
              | Some(cls) => acc @ [(name, cls)]
              | None => acc
              }
            ),
        [],
        referenced,
      );
    let let_names = List.map(fst, lets);
    let inputs =
      List.filter_map(
        ((name, _)) => List.mem(name, let_names) ? None : Some(name),
        var_sorts,
      );
    Some({
      guards,
      lets,
      var_sorts,
      inputs,
      complete,
    });
  };

/* Merge several reach conditions into one (their conjunction): "find one
 * input reaching all of them in a single execution". Used for group/merge.
 * Variable declarations are deduped by name (a repeated declare-const is a z3
 * error); let-bindings likewise. NOTE: variables are merged by NAME, so reach
 * points in different scopes that share a local/parameter name are conflated —
 * fine within one scope (the common case), approximate across scopes. */
let dedup_assoc = (xs: list((string, 'a))): list((string, 'a)) =>
  List.fold_left(
    (acc, (k, v)) => List.mem_assoc(k, acc) ? acc : acc @ [(k, v)],
    [],
    xs,
  );

let merge = (rs: list(t)): t => {
  guards: List.concat_map(r => r.guards, rs),
  lets: dedup_assoc(List.concat_map(r => r.lets, rs)),
  var_sorts: dedup_assoc(List.concat_map(r => r.var_sorts, rs)),
  inputs: List.concat_map(r => r.inputs, rs) |> List.sort_uniq(compare),
  complete: List.for_all(r => r.complete, rs),
};

/* ===================== SMT-LIB2 assembly ===================== */

/* Returns (script, complete): complete is false if any guard or let-definition
 * had to be dropped (unsupported construct or undeclarable variable). Dropping
 * only weakens the constraint, so UNSAT of the result is still sound ("dead
 * code"); a SAT result with complete=false must be reported as unknown. */
let smtlib2 = (r: t): (string, bool) => {
  let declared = List.map(fst, r.var_sorts);
  let vars_declared = (e: Exp.t): bool =>
    List.for_all(((n, _)) => List.mem(n, declared), vars_of(e));
  let translate = (e: Exp.t): option(string) =>
    vars_declared(e)
      ? switch (ConstraintGen.smt_of_exp(e)) {
        | s => Some(s)
        | exception (ConstraintGen.Unsupported(_)) => None
        }
      : None;
  let decls =
    List.map(
      ((name, cls)) =>
        Printf.sprintf(
          "(declare-const %s %s)",
          name,
          TestGen.sort_of_cls(cls),
        ),
      r.var_sorts,
    );
  let nat_constraints =
    List.filter_map(
      ((name, cls: Atom.cls)) =>
        switch (cls) {
        | Nat => Some(Printf.sprintf("(assert (>= %s 0))", name))
        | _ => None
        },
      r.var_sorts,
    );
  let dropped = ref(false);
  let asserts_of = (translate_one, items) =>
    List.filter_map(
      item =>
        switch (translate_one(item)) {
        | Some(line) => Some(line)
        | None =>
          dropped := true;
          None;
        },
      items,
    );
  let let_asserts =
    asserts_of(
      ((v, def)) =>
        switch (translate(def)) {
        | Some(s) when List.mem(v, declared) =>
          Some(Printf.sprintf("(assert (= %s %s))", v, s))
        | _ => None
        },
      r.lets,
    );
  let guard_asserts =
    asserts_of(
      g => Option.map(s => "(assert " ++ s ++ ")", translate(g)),
      r.guards,
    );
  let lines =
    ["(set-logic ALL)", "(set-option :produce-models true)"]
    @ decls
    @ nat_constraints
    @ let_asserts
    @ guard_asserts
    @ ["(check-sat)", "(get-model)"];
  (String.concat("\n", lines), r.complete && ! dropped^);
};

/* Map a raw solver outcome to the reachability reading:
 *   Unsat  → unreachable (dead code) — sound even if guards were dropped
 *   Sat    → reachable, keeping only the input variables — but if anything was
 *            dropped (complete=false) we can't trust it, so report Unknown
 *   else   → unchanged. */
let interpret =
    (~complete: bool, ~inputs: list(string), outcome: TestGen.outcome)
    : TestGen.outcome =>
  switch (outcome) {
  | Sat(assignments) when complete =>
    Sat(
      List.filter(
        (a: TestGen.assignment) => List.mem(a.name, inputs),
        assignments,
      ),
    )
  | Sat(_) => Unknown
  | other => other
  };
