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

/* Which binder a declared variable refers to. Same-named variables from
 * different scopes must never share an SMT symbol (a leaked constraint could
 * turn a reachable point UNSAT, i.e. falsely dead), so names are renamed
 * apart (`x!1`, `x!2`, …) and `merge` unifies variables by this identity
 * rather than by name. */
[@deriving (show({with_path: false}), sexp, yojson)]
type origin =
  | Bound(Id.t) /* the binder whose pattern-variable node has this id */
  | Free(string); /* a program-level free variable, identified by name */

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
  /* Binder identity of every declared name, for identity-aware `merge`. */
  origins: list((string, origin)),
  /* False if an unsupported construct (e.g. `match`) sat on the path, so a
   * SAT result can't be trusted as "reachable" (but UNSAT is still sound). */
  complete: bool,
};

/* ===================== path-condition extraction ===================== */

let contains = (id: Id.t, e: Exp.t): bool =>
  Option.is_some(Exp.find_by_id(id, e));

/* Bindings introduced by a pattern matched against a value. A variable binds
 * the whole value; a tuple pattern against a tuple expression binds each
 * component. None for anything we can't decompose (e.g. a tuple pattern bound
 * to a non-tuple expression). Used for `let`, function parameters, and
 * function inlining. */
let rec bind_pattern =
        (pat: Pat.t, value: Exp.t): option(list((string, Exp.t))) =>
  switch (pat.term) {
  | Var(x) => Some([(x, value)])
  | Parens(inner)
  | Asc(inner, _) => bind_pattern(inner, value)
  | Tuple(pats) =>
    switch (ConstraintGen.tuple_elems(value)) {
    | Some(vals) when List.length(vals) == List.length(pats) =>
      List.fold_left2(
        (acc, p, v) =>
          switch (acc, bind_pattern(p, v)) {
          | (Some(a), Some(b)) => Some(a @ b)
          | _ => None
          },
        Some([]),
        pats,
        vals,
      )
    | _ => None
    }
  | _ => None
  };

let negate = (c: Exp.t): Exp.t => Exp.fresh(UnOp(Bool(Not), c));

let false_exp = (): Exp.t => Exp.fresh(Atom(Bool(false)));

/* ===================== scope-aware naming ===================== */

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

/* All variable names occurring in `es`. */
let names_of = (es: list(Exp.t)): list(string) =>
  List.concat_map(vars_of, es) |> List.map(fst) |> List.sort_uniq(compare);

/* Variable names a pattern binds, with the id of each binding occurrence
 * (the identity behind `Bound`). */
let rec pat_var_ids = (p: Pat.t): list((string, Id.t)) =>
  switch (p.term) {
  | Var(x) => [(x, Pat.rep_id(p))]
  | Parens(q)
  | Asc(q, _)
  | Projector(_, q) => pat_var_ids(q)
  | Tuple(ps)
  | ListLit(ps) => List.concat_map(pat_var_ids, ps)
  | Cons(a, b) => pat_var_ids(a) @ pat_var_ids(b)
  | TupLabel(_, q) => pat_var_ids(q)
  | Ap(_, q) => pat_var_ids(q)
  | _ => []
  };

let pat_vars = (p: Pat.t): list(string) => List.map(fst, pat_var_ids(p));

/* A name for a variable renamed apart from a shadowing binder: `x!1`, `x!2`,
 * … `!` cannot appear in a Hazel identifier but is a legal SMT-LIB simple-
 * symbol character, so fresh names never collide with source names (and can
 * never be captured by a source binder). */
let fresh_name = (base: string, used: list(string)): string => {
  let rec go = k => {
    let cand = Printf.sprintf("%s!%d", base, k);
    List.mem(cand, used) ? go(k + 1) : cand;
  };
  go(1);
};

/* Simultaneously rename free variable occurrences per `mapping`. Var nodes
 * are rebuilt in place (ids preserved) so statics lookups keep resolving;
 * entries are dropped under binders that rebind their source name. Only the
 * fragment ConstraintGen can translate is traversed — an occurrence inside
 * anything else is dropped at translation time anyway. */
let rec rename_many = (mapping: list((string, string)), e: Exp.t): Exp.t =>
  switch (mapping) {
  | [] => e
  | _ =>
    let go = rename_many(mapping);
    let under = (pat, body) =>
      rename_many(
        List.filter(((v, _)) => !List.mem(v, pat_vars(pat)), mapping),
        body,
      );
    switch (e.term) {
    | Var(x) =>
      switch (List.assoc_opt(x, mapping)) {
      | Some(x') => {
          ...e,
          term: Var(x'),
        }
      | None => e
      }
    | Atom(_) => e
    | Parens(x) => {
        ...e,
        term: Parens(go(x)),
      }
    | UnOp(op, x) => {
        ...e,
        term: UnOp(op, go(x)),
      }
    | BinOp(op, l, r) => {
        ...e,
        term: BinOp(op, go(l), go(r)),
      }
    | If(c, t, f) => {
        ...e,
        term: If(go(c), go(t), go(f)),
      }
    | Seq(a, b) => {
        ...e,
        term: Seq(go(a), go(b)),
      }
    | Tuple(es) => {
        ...e,
        term: Tuple(List.map(go, es)),
      }
    | Ap(d, fn, a) => {
        ...e,
        term: Ap(d, go(fn), go(a)),
      }
    | Let(pat, def, body) => {
        ...e,
        term: Let(pat, go(def), under(pat, body)),
      }
    | Fun(pat, body, t, n) => {
        ...e,
        term: Fun(pat, under(pat, body), t, n),
      }
    | Match(s, rules) => {
        ...e,
        term:
          Match(go(s), List.map(((p, b)) => (p, under(p, b)), rules)),
      }
    | _ => e
    };
  };

let rename = (from: string, to_: string, e: Exp.t): Exp.t =>
  rename_many([(from, to_)], e);

/* Rename a binding occurrence in a pattern (for capture-avoiding `subst`). */
let rec rename_pat = (from: string, to_: string, p: Pat.t): Pat.t => {
  let go = rename_pat(from, to_);
  switch (p.term) {
  | Var(x) when x == from => {
      ...p,
      term: Var(to_),
    }
  | Var(_) => p
  | Parens(q) => {
      ...p,
      term: Parens(go(q)),
    }
  | Asc(q, t) => {
      ...p,
      term: Asc(go(q), t),
    }
  | Projector(d, q) => {
      ...p,
      term: Projector(d, go(q)),
    }
  | Tuple(ps) => {
      ...p,
      term: Tuple(List.map(go, ps)),
    }
  | ListLit(ps) => {
      ...p,
      term: ListLit(List.map(go, ps)),
    }
  | Cons(a, b) => {
      ...p,
      term: Cons(go(a), go(b)),
    }
  | TupLabel(l, q) => {
      ...p,
      term: TupLabel(l, go(q)),
    }
  | Ap(f, q) => {
      ...p,
      term: Ap(f, go(q)),
    }
  | _ => p
  };
};

/* Cross a binder on the walk toward the reach point: from here inward its
 * pattern's names refer to the new binder, so every accumulated occurrence
 * of a re-bound name — in guards, let definitions, let keys, and origin keys
 * — is renamed apart to a fresh `v!k`. `also` holds expressions evaluated in
 * the OUTER scope (e.g. the incoming let definition): they force a rename
 * too, and the caller applies the returned renaming to them. */
let shadow =
    (
      ~also: list(Exp.t)=[],
      pat: Pat.t,
      guards: list(Exp.t),
      lets: list((string, Exp.t)),
      origins: list((string, origin)),
    )
    : (
        list(Exp.t),
        list((string, Exp.t)),
        list((string, origin)),
        list((string, string)),
      ) => {
  let occurring =
    List.map(fst, lets) @ names_of(guards @ List.map(snd, lets) @ also);
  let (renaming, _) =
    List.fold_left(
      ((ren, used), v) =>
        List.mem(v, occurring) && !List.mem_assoc(v, ren)
          ? {
            let v' = fresh_name(v, used);
            ([(v, v'), ...ren], [v', ...used]);
          }
          : (ren, used),
      ([], occurring),
      pat_vars(pat),
    );
  let ren_key = k =>
    switch (List.assoc_opt(k, renaming)) {
    | Some(k') => k'
    | None => k
    };
  let guards = List.map(rename_many(renaming), guards);
  let lets =
    List.map(
      ((k, def)) => (ren_key(k), rename_many(renaming, def)),
      lets,
    );
  let origins =
    List.map(((k, org)) => (ren_key(k), org), origins)
    @ List.filter_map(
        /* a renamed name with no recorded origin was a free variable */
        ((v, v')) =>
          List.mem_assoc(v, origins) ? None : Some((v', Free(v))),
        renaming,
      );
  (guards, lets, origins, renaming);
};

/* Record the identity of a crossed binder's names (replacing any stale entry
 * for an unreferenced — hence unrenamed — outer binder of the same name). */
let bind_origins =
    (pat: Pat.t, origins: list((string, origin))): list((string, origin)) =>
  List.fold_left(
    (acc, (v, id)) => [(v, Bound(id)), ...List.remove_assoc(v, acc)],
    origins,
    pat_var_ids(pat),
  );

/* How a scrutinee relates to a (literal/wildcard) match pattern. */
type pmatch =
  | PAlways /* wildcard: matches anything */
  | PCond(Exp.t) /* literal: matches iff scrut == lit */
  | PUnknown; /* pattern we can't express (constructor/tuple/var-binding) */

let is_unknown = (m: pmatch): bool =>
  switch (m) {
  | PUnknown => true
  | _ => false
  };

let cond_of = (m: pmatch): option(Exp.t) =>
  switch (m) {
  | PCond(c) => Some(c)
  | _ => None
  };

let conjoin = (cs: list(Exp.t)): Exp.t =>
  switch (cs) {
  | [] => Exp.fresh(Atom(Bool(true)))
  | [c, ...rest] =>
    List.fold_left(
      (acc, c) => Exp.fresh(BinOp(Bool(And), acc, c)),
      c,
      rest,
    )
  };

let rec pat_matches = (scrut: Exp.t, p: Pat.t): pmatch =>
  switch (p.term) {
  | Wild => PAlways
  | Atom(a) =>
    PCond(Exp.fresh(BinOp(Poly(Equals), scrut, Exp.fresh(Atom(a)))))
  | Parens(inner) => pat_matches(scrut, inner)
  | Tuple(pats) =>
    /* Match component-wise against a tuple scrutinee. */
    switch (ConstraintGen.tuple_elems(scrut)) {
    | Some(scruts) when List.length(scruts) == List.length(pats) =>
      let subs = List.map2(pat_matches, scruts, pats);
      if (List.exists(is_unknown, subs)) {
        PUnknown;
      } else {
        switch (List.filter_map(cond_of, subs)) {
        | [] => PAlways
        | conds => PCond(conjoin(conds))
        };
      };
    | _ => PUnknown
    }
  | _ => PUnknown
  };

/* The walk accumulator: guards, in-scope let-bindings, binder identities of
 * the names in play, and whether the path condition is complete. */
type acc = (
  list(Exp.t),
  list((string, Exp.t)),
  list((string, origin)),
  bool,
);

/* Process one (ancestor, child-on-path) step, threading the accumulator. */
let step = (~map: Statics.Map.t, a_id: Id.t, child_id: Id.t, acc: acc): acc => {
  let (guards, lets, origins, complete) = acc;
  switch (Statics.Map.lookup_exp(a_id, map)) {
  | None => acc
  | Some({user_term, _}) =>
    switch (user_term.term) {
    | If(c, t, f) =>
      if (contains(child_id, t)) {
        (guards @ [c], lets, origins, complete);
      } else if (contains(child_id, f)) {
        (guards @ [negate(c)], lets, origins, complete);
      } else {
        acc; /* child is in the condition itself */
      }
    | Let(p, def, body) when contains(child_id, body) =>
      /* Bind first — the definition references the OUTER scope — then rename
       * shadowed outer names apart (in the incoming defs too). */
      let binds = bind_pattern(p, def);
      let (guards, lets, origins, renaming) =
        shadow(~also=[def], p, guards, lets, origins);
      let origins = bind_origins(p, origins);
      switch (binds) {
      | Some(binds) =>
        let binds =
          List.map(((v, d)) => (v, rename_many(renaming, d)), binds);
        (guards, lets @ binds, origins, complete);
      | None => (guards, lets, origins, false) /* pattern still shadows */
      };
    | Let(_) => acc /* child is in the definition */
    | Fun(p, body, _, _) when contains(child_id, body) =>
      /* The parameter is a fresh free input from here inward; outer bindings
       * of the same name are renamed apart rather than leaking onto it. */
      let (guards, lets, origins, _) = shadow(p, guards, lets, origins);
      (guards, lets, bind_origins(p, origins), complete);
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
      | None => acc
      | Some(target_i) =>
        let (gs, comp) =
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
          );
        /* The arm guards reference the outer-scope scrutinee and were added
         * above, so they are correctly renamed here alongside everything
         * else the target arm's pattern shadows. */
        let target_pat = fst(List.nth(rules, target_i));
        let (gs, lets, origins, _) = shadow(target_pat, gs, lets, origins);
        (gs, lets, bind_origins(target_pat, origins), comp);
      };
    | Match(_) => acc /* child is in the scrutinee */
    /* Binder constructs we don't model: their pattern still shadows, and a
     * leaked outer constraint would break UNSAT-soundness even though the
     * result is already marked incomplete. */
    | FixF(p, body, _) when contains(child_id, body) =>
      let (guards, lets, origins, _) = shadow(p, guards, lets, origins);
      (guards, lets, bind_origins(p, origins), false);
    | Theorem(p, _, body) when contains(child_id, body) =>
      let (guards, lets, origins, _) = shadow(p, guards, lets, origins);
      (guards, lets, bind_origins(p, origins), false);
    | Forall(p, body) when contains(child_id, body) =>
      let (guards, lets, origins, _) = shadow(p, guards, lets, origins);
      (guards, lets, bind_origins(p, origins), false);
    | Parens(_)
    | BinOp(_)
    | UnOp(_)
    | Ap(_)
    | Fun(_)
    | Seq(_)
    | Tuple(_) => acc /* no guard contributed */
    | _ => (guards, lets, origins, false) /* unmodeled control flow */
    }
  };
};

let sort_of_var =
    (~ctx: Ctx.t, ~map: Statics.Map.t, id: Id.t): option(Atom.cls) =>
  switch (Statics.Map.lookup_exp(id, map)) {
  | Some({ty, ana, _}) =>
    /* Prefer the synthesized type; for a free/unbound variable that is Unknown,
       fall back to the type expected at the use site (e.g. an operand of `>`
       or a function argument). */
    switch (Typ.is_ana_atom(Typ.weak_head_normalize(ctx, ty))) {
    | Some(_) as s => s
    | None => Typ.is_ana_atom(Typ.weak_head_normalize(ctx, ana))
    }
  | None => None
  };

/* Infer a variable's base type from how it's used. After inlining, an
 * argument's statics type can be Unknown (e.g. an unannotated function
 * parameter), so we read the sort off the operators it flows into:
 * `a * 0` ⇒ a is Int, `b && c` ⇒ Bool, `n == 0` ⇒ n matches the literal, etc. */
let rec is_var = (name: string, e: Exp.t): bool =>
  switch (e.term) {
  | Var(x) => x == name
  | Parens(inner) => is_var(name, inner)
  | _ => false
  };

let rec lit_sort = (e: Exp.t): option(Atom.cls) =>
  switch (e.term) {
  | Atom(a) => Some(Atom.cls_of_t(a))
  | Parens(inner) => lit_sort(inner)
  | _ => None
  };

let binop_operand_sort = (op: Operators.op_bin): option(Atom.cls) =>
  switch (op) {
  | Int(_) => Some(Int)
  | SInt(_) => Some(SInt)
  | Nat(_) => Some(Nat)
  | Float(_) => Some(Float)
  | Bool(_) => Some(Bool)
  | String(_) => Some(String)
  | Poly(_) => None
  };

let unop_operand_sort = (op: Operators.op_un): option(Atom.cls) =>
  switch (op) {
  | Int(_) => Some(Int)
  | SInt(_) => Some(SInt)
  | Nat(_) => Some(Nat)
  | Float(_) => Some(Float)
  | Bool(_) => Some(Bool)
  };

let infer_sort = (name: string, exps: list(Exp.t)): option(Atom.cls) => {
  let orElse = (a, b) =>
    switch (a) {
    | Some(_) => a
    | None => b()
    };
  let rec scan = (e: Exp.t): option(Atom.cls) =>
    switch (e.term) {
    | BinOp(op, l, r) =>
      let direct =
        if (is_var(name, l) || is_var(name, r)) {
          switch (binop_operand_sort(op)) {
          | Some(_) as s => s
          | None => is_var(name, l) ? lit_sort(r) : lit_sort(l) /* Poly(=) */
          };
        } else {
          None;
        };
      orElse(direct, () => orElse(scan(l), () => scan(r)));
    | UnOp(op, x) => is_var(name, x) ? unop_operand_sort(op) : scan(x)
    | If(c, t, f) => orElse(scan(c), () => orElse(scan(t), () => scan(f)))
    | Parens(x) => scan(x)
    | Ap(_, f, a) => orElse(scan(f), () => scan(a))
    | Seq(a, b) => orElse(scan(a), () => scan(b))
    | Tuple(es) => List.find_map(scan, es)
    | Let(_, def, body) => orElse(scan(def), () => scan(body))
    | Match(s, rules) =>
      orElse(scan(s), () => List.find_map(((_, b)) => scan(b), rules))
    | _ => None
    };
  List.find_map(scan, exps);
};

/* ===================== function inlining (Ap support) ===================== */

/* Capture-avoiding substitution of `repl` for free `Var(name)` in `e`, over
 * the fragment we analyze (other constructs are left intact — they'd be
 * rejected by ConstraintGen anyway). A binder that would capture one of
 * `repl`'s variables is renamed apart first (`avoid` holds names the fresh
 * ones must additionally steer clear of, e.g. the analyze-level declared
 * names). */
let subst =
    (~avoid: list(string)=[], name: string, repl: Exp.t, e: Exp.t): Exp.t => {
  let repl_names = names_of([repl]);
  let unshadow = (pat: Pat.t, body: Exp.t): (Pat.t, Exp.t) =>
    List.fold_left(
      ((pat, body), v) =>
        if (List.mem(v, repl_names)) {
          let used = repl_names @ pat_vars(pat) @ names_of([body]) @ avoid;
          let v' = fresh_name(v, used);
          (rename_pat(v, v', pat), rename(v, v', body));
        } else {
          (pat, body);
        },
      (pat, body),
      pat_vars(pat),
    );
  let rec go = (e: Exp.t): Exp.t =>
    switch (e.term) {
    | Var(x) => x == name ? repl : e
    | Atom(_) => e
    | Parens(x) => {
        ...e,
        term: Parens(go(x)),
      }
    | UnOp(op, x) => {
        ...e,
        term: UnOp(op, go(x)),
      }
    | BinOp(op, l, r) => {
        ...e,
        term: BinOp(op, go(l), go(r)),
      }
    | If(c, t, f) => {
        ...e,
        term: If(go(c), go(t), go(f)),
      }
    | Seq(a, b) => {
        ...e,
        term: Seq(go(a), go(b)),
      }
    | Tuple(es) => {
        ...e,
        term: Tuple(List.map(go, es)),
      }
    | Ap(d, fn, a) => {
        ...e,
        term: Ap(d, go(fn), go(a)),
      }
    | Let(pat, def, body) =>
      let def = go(def);
      if (List.mem(name, pat_vars(pat))) {
        {
          ...e,
          term: Let(pat, def, body),
        };
      } else {
        let (pat, body) = unshadow(pat, body);
        {
          ...e,
          term: Let(pat, def, go(body)),
        };
      };
    | Fun(pat, body, t, n) =>
      if (List.mem(name, pat_vars(pat))) {
        e;
      } else {
        let (pat, body) = unshadow(pat, body);
        {
          ...e,
          term: Fun(pat, go(body), t, n),
        };
      }
    | Match(s, rules) => {
        ...e,
        term:
          Match(
            go(s),
            List.map(
              ((p, b)) =>
                if (List.mem(name, pat_vars(p))) {
                  (p, b);
                } else {
                  let (p, b) = unshadow(p, b);
                  (p, go(b));
                },
              rules,
            ),
          ),
      }
    | _ => e
    };
  go(e);
};

let rec fn_name = (e: Exp.t): option(string) =>
  switch (e.term) {
  | Var(f) => Some(f)
  | Parens(inner) => fn_name(inner)
  | _ => None
  };

let is_fun = (e: Exp.t): bool =>
  switch (e.term) {
  | Fun(_) => true
  | Parens(inner) =>
    switch (inner.term) {
    | Fun(_) => true
    | _ => false
    }
  | _ => false
  };

/* Beta-reduce applications of let-bound functions in `env` (name → Fun def):
 * `f(arg)` with `f = fun p -> body` becomes `body[p := arg]`. Single-variable
 * parameters only; the applied function is removed from `env` while inlining
 * its body, so recursive calls are left as-is (and later rejected, marking the
 * result incomplete). */
let rec inline_aps =
        (~avoid: list(string)=[], env: list((string, Exp.t)), e: Exp.t)
        : Exp.t => {
  let go = inline_aps(~avoid, env);
  switch (e.term) {
  | Ap(Forward, fn, arg) =>
    let arg = go(arg);
    switch (fn_name(fn)) {
    | Some(f) when List.mem_assoc(f, env) =>
      let def = List.assoc(f, env);
      let def =
        switch (def.term) {
        | Parens(inner) => inner
        | _ => def
        };
      switch (def.term) {
      | Fun(pat, body, _, _) =>
        switch (bind_pattern(pat, arg)) {
        | Some(binds) =>
          let reduced =
            List.fold_left(
              (b, (p, a)) => subst(~avoid, p, a, b),
              body,
              binds,
            );
          inline_aps(~avoid, List.remove_assoc(f, env), reduced);
        | None => {
            ...e,
            term: Ap(Forward, fn, arg),
          }
        }
      | _ => {
          ...e,
          term: Ap(Forward, fn, arg),
        }
      };
    | _ => {
        ...e,
        term: Ap(Forward, go(fn), arg),
      }
    };
  | Parens(x) => {
      ...e,
      term: Parens(go(x)),
    }
  | UnOp(op, x) => {
      ...e,
      term: UnOp(op, go(x)),
    }
  | BinOp(op, l, r) => {
      ...e,
      term: BinOp(op, go(l), go(r)),
    }
  | If(c, t, f) => {
      ...e,
      term: If(go(c), go(t), go(f)),
    }
  | Seq(a, b) => {
      ...e,
      term: Seq(go(a), go(b)),
    }
  | Tuple(es) => {
      ...e,
      term: Tuple(List.map(go, es)),
    }
  | Let(pat, def, body) => {
      ...e,
      term: Let(pat, go(def), go(body)),
    }
  | Match(s, rules) => {
      ...e,
      term: Match(go(s), List.map(((p, b)) => (p, go(b)), rules)),
    }
  | _ => e
  };
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
    let (guards, walked_lets, walk_origins, complete) =
      walk(path, ([], [], [], true));
    /* Inline applications of in-scope let-bound functions into the guards (and
       remaining value-let definitions), then set those function bindings
       aside — what's left are value bindings the path condition refers to.
       Fresh names minted while inlining must not collide with walk names. */
    let avoid =
      List.map(fst, walked_lets)
      @ names_of(guards @ List.map(snd, walked_lets));
    let (fn_lets, value_lets) =
      List.partition(((_, def)) => is_fun(def), walked_lets);
    let inline = inline_aps(~avoid, fn_lets);
    let guards = List.map(inline, guards);
    let all_lets = List.map(((v, def)) => (v, inline(def)), value_lets);
    /* Keep only the lets actually needed: start from the guards' variables and
       close over let-definitions, dropping in-scope lets irrelevant to this
       reach point (otherwise they'd be declared/asserted, and an unreferenced
       one would be dropped at solve time and spuriously mark incompleteness). */
    let rec close = (needed: list(string)): list(string) => {
      let next =
        List.fold_left(
          (acc, (v, def)) =>
            List.mem(v, acc) ? acc @ names_of([def]) : acc,
          needed,
          all_lets,
        )
        |> List.sort_uniq(compare);
      List.length(next) == List.length(needed) ? needed : close(next);
    };
    let needed = close(names_of(guards));
    let lets = List.filter(((v, _)) => List.mem(v, needed), all_lets);
    /* Resolve sorts for every variable referenced in guards and kept let-defs:
       prefer the statics type, then infer from operator usage in those exprs. */
    let exprs = guards @ List.map(snd, lets);
    let referenced =
      List.concat_map(vars_of, guards)
      @ List.concat_map(((_, def)) => vars_of(def), lets);
    let var_sorts =
      List.fold_left(
        (acc, (name, id)) =>
          if (List.mem_assoc(name, acc)) {
            acc;
          } else {
            let cls =
              switch (sort_of_var(~ctx=target.ctx, ~map, id)) {
              | Some(_) as s => s
              | None => infer_sort(name, exprs)
              };
            switch (cls) {
            | Some(cls) => acc @ [(name, cls)]
            | None => acc
            };
          },
        [],
        referenced,
      );
    let let_names = List.map(fst, lets);
    let inputs =
      List.filter_map(
        ((name, _)) => List.mem(name, let_names) ? None : Some(name),
        var_sorts,
      );
    /* Identity of every declared name; anything the walk didn't record is a
       free variable (or an expression-internal binder name introduced by
       inlining — a junk constant the SMT-level `let` shadows anyway). */
    let origins =
      List.map(
        ((name, _)) =>
          switch (List.assoc_opt(name, walk_origins)) {
          | Some(org) => (name, org)
          | None => (name, Free(name))
          },
        var_sorts,
      );
    Some({
      guards,
      lets,
      var_sorts,
      inputs,
      origins,
      complete,
    });
  };

/* Merge several reach conditions into one (their conjunction): "find one
 * input reaching all of them in a single execution". Used for group/merge.
 * Variables are unified by binder identity (`origins`), not by name: reach
 * points that see the same binder share its variable, while same-named
 * variables from different scopes are renamed apart (`v!k`) rather than
 * conflated. Declarations can then be deduped by name (a repeated
 * declare-const is a z3 error); let-bindings likewise. */
let dedup_assoc = (xs: list((string, 'a))): list((string, 'a)) =>
  List.fold_left(
    (acc, (k, v)) => List.mem_assoc(k, acc) ? acc : acc @ [(k, v)],
    [],
    xs,
  );

/* Extend the identity → canonical-name assignment (`canon`, with `taken` its
 * used names) by condition `r`'s variables, returning the renaming `r` needs:
 * a seen identity maps to its canonical name; a new identity keeps its name
 * unless another identity already took it, in which case it gets a fresh one. */
let merge_renaming =
    (canon: list((origin, string)), taken: list(string), r: t)
    : (list((string, string)), list((origin, string)), list(string)) =>
  List.fold_left(
    ((ren, canon, taken), (n, org)) =>
      switch (List.assoc_opt(org, canon)) {
      | Some(cname) when cname == n => (ren, canon, taken)
      | Some(cname) => ([(n, cname), ...ren], canon, taken)
      | None when List.mem(n, taken) =>
        let n' = fresh_name(n, taken @ List.map(fst, r.origins));
        ([(n, n'), ...ren], [(org, n'), ...canon], [n', ...taken]);
      | None => (ren, [(org, n), ...canon], [n, ...taken])
      },
    ([], canon, taken),
    r.origins,
  );

let merge = (rs: list(t)): t => {
  /* Assign one canonical name per identity, in order: the first condition to
   * mention an identity names it. Each condition's renaming is applied
   * simultaneously (a name can both be given away and received, e.g. swaps). */
  let (rs, _, _) =
    List.fold_left(
      ((acc, canon, taken), r) => {
        let (renaming, canon, taken) = merge_renaming(canon, taken, r);
        let ren_name = n =>
          switch (List.assoc_opt(n, renaming)) {
          | Some(n') => n'
          | None => n
          };
        let r =
          renaming == []
            ? r
            : {
              ...r,
              guards: List.map(rename_many(renaming), r.guards),
              lets:
                List.map(
                  ((k, d)) => (ren_name(k), rename_many(renaming, d)),
                  r.lets,
                ),
              var_sorts:
                List.map(((n, c)) => (ren_name(n), c), r.var_sorts),
              inputs: List.map(ren_name, r.inputs),
              origins: List.map(((n, o)) => (ren_name(n), o), r.origins),
            };
        ([r, ...acc], canon, taken);
      },
      ([], [], []),
      rs,
    );
  let rs = List.rev(rs);
  {
    guards: List.concat_map(r => r.guards, rs),
    lets: dedup_assoc(List.concat_map(r => r.lets, rs)),
    var_sorts: dedup_assoc(List.concat_map(r => r.var_sorts, rs)),
    inputs: List.concat_map(r => r.inputs, rs) |> List.sort_uniq(compare),
    origins: dedup_assoc(List.concat_map(r => r.origins, rs)),
    complete: List.for_all(r => r.complete, rs),
  };
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
