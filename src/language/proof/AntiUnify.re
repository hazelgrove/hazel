/* Anti-unification and minimal discriminating patterns.
 *
 * A pure library over the pattern language introduced in MetaVar: a
 * "pattern" is just an Exp that may contain `$e`/`$v`/`$x`
 * metavariables, so everything produced here can be dropped straight
 * into a proof step's `on`/`at` slot, printed back as ordinary Hazel
 * syntax, and matched with MatchExp.match_pattern.
 *
 * Two operations:
 *
 *  - `msg e1 e2` — the most specific generalization of two expressions
 *    (first-order anti-unification / least general generalization).
 *    Where the two agree, the result keeps the shared structure; where
 *    they disagree, it puts a metavariable, and *the same* metavariable
 *    for disagreements that recur, which is what makes the result least
 *    general rather than merely a generalization.
 *
 *  - `discriminating_pattern ~goal id` — the most *general* pattern that
 *    still pins down one particular occurrence in a goal, paired with
 *    the index that occurrence has among the pattern's matches. This is
 *    the inverse problem: instead of quoting a subterm verbatim, say the
 *    least about it that still identifies it.
 *
 * Implementation note. Rather than an exhaustive match over the ~50 Exp
 * constructors (which MatchExp already pays for once), this module works
 * generically through three primitives derived from `Exp.map_term`:
 * immediate children, the "shell" of a node (the node with its
 * children blanked out), and id-targeted replacement
 * (ProofHacks.replace_exp_id). Every pattern returned is therefore
 * built out of the caller's own nodes, so it prints as real syntax and
 * no constructor can be silently mishandled as the grammar grows.
 */

open Util;

/* A pattern is an ordinary expression that may contain metavariables. */
type pattern = Exp.t;

module Fresh = IdTagged.FreshGrammar;

let metavar = (name: string): pattern => Fresh.Exp.var(name);
let any: pattern = metavar(MetaVar.any_name);

/* Named metavariables are generated as `$x1`, `$x2`, ...: linear (so
 * repeated disagreements are tied together) and, unlike `$1`, accepted
 * by Token.var_regexp, so a generated pattern round-trips through the
 * parser. */
let nth_name = (i: int): string => "$x" ++ string_of_int(i + 1);

/* ---- Generic structural primitives --------------------------------- */

/* The immediate *expression* children of a node, left to right.
 *
 * `Exp.map_term` applies `f_exp` at every node top-down, so we let the
 * traversal descend through the root and then stop at each node below
 * it: those are exactly the immediate children. Children reached
 * through a pattern or type (a `let`'s binder, an ascription's type)
 * are not expressions and so are not returned; they stay part of the
 * node's shell and are compared there instead.
 *
 * `map_term` rebuilds a node by mapping its fields, and OCaml evaluates
 * those right to left, so children arrive in reverse source order; we
 * prepend to undo that. Nothing about matching depends on the order —
 * two nodes with the same head are decomposed by the same code and so
 * pair up consistently either way — but it does decide which
 * disagreement `msg` calls `$x1`, and left to right is what a reader
 * expects. */
let children = (e: Exp.t): list(Exp.t) => {
  let root = Exp.rep_id(e);
  let acc = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, x) =>
          if (Exp.rep_id(x) == root) {
            cont(x);
          } else {
            acc := [x, ...acc^];
            x;
          },
      e,
    );
  acc^;
};

/* Unwrap the wrappers that matching already ignores, so that they never
 * show up as spurious structure. */
let rec unwrap = (e: Exp.t): Exp.t =>
  switch (e |> Exp.term_of) {
  | Parens(e1)
  | Projector(_, e1)
  | Asc(e1, _) => unwrap(e1)
  | _ => e
  };

/* Replace each listed subterm of `e` by `to_`. */
let blank_out = (~to_: pattern, targets: list(Exp.t), e: Exp.t): Exp.t =>
  List.fold_left(
    (acc, child) => ProofHacks.replace_exp_id(Exp.rep_id(child), acc, to_),
    e,
    targets,
  );

/* A node with all of its immediate expression children blanked out.
 *
 * Comparing shells is how we ask "do these two nodes have the same
 * head?" without enumerating heads: the shell still carries the
 * operator, any literal payload, the binder patterns, the type
 * annotations and the arity, so shell equality is exactly head
 * agreement. */
let shell = (e: Exp.t): Exp.t => blank_out(~to_=any, children(e), e);

/* Alpha-aware structural equality, "to the degree MatchExp is": an
 * empty match context means no metavariable may bind, so this succeeds
 * only when the two terms genuinely correspond, with MatchExp's own
 * treatment of binders. */
let alpha_equal = (~alphas=[], e1: Exp.t, e2: Exp.t): bool =>
  MatchExp.match_exp'(~info_map=Statics.Map.empty, alphas, [], e1, e2)
  == Some([]);

let same_head = (~alphas=[], e1: Exp.t, e2: Exp.t): bool =>
  alpha_equal(~alphas, shell(e1), shell(e2));

/* The immediate *pattern* children of a node, used to extend the alpha
 * correspondence when descending through a binder. Obtained with the
 * same trick as `children`, via `f_pat`. */
let pat_children = (e: Exp.t): list(Pat.t) => {
  let root = Exp.rep_id(e);
  let acc = ref([]);
  let _ =
    Exp.map_term(
      /* Stop the expression traversal below the root, so `f_pat` only
       * fires for patterns belonging to this node — not for binders
       * nested deeper, which are this node's children's business. */
      ~f_exp=(cont, x) => Exp.rep_id(x) == root ? cont(x) : x,
      ~f_pat=
        (_cont, p) => {
          acc := acc^ @ [p];
          p;
        },
      e,
    );
  acc^;
};

/* Binder correspondence contributed by a node: pair up its pattern
 * children positionally. Over-approximates scope (a `let`'s binder is
 * not really in scope in its own definiens), which is why `msg`
 * verifies its result below and coarsens if the correspondence turned
 * out to be wrong. */
let binder_alphas = (e1: Exp.t, e2: Exp.t): MatchExp.alphas => {
  let (p1s, p2s) = (pat_children(e1), pat_children(e2));
  if (List.length(p1s) != List.length(p2s)) {
    [];
  } else {
    List.fold_left2(
      (acc, p1, p2) =>
        switch (MatchExp.match_pat(p1, p2)) {
        | Some(alphas) => alphas @ acc
        | None => acc
        },
      [],
      p1s,
      p2s,
    );
  };
};

/* ---- Most specific generalization (anti-unification) --------------- */

/* Disagreement pairs, in the order they are met top-down. Each is a
 * maximal pair of aligned subterms whose heads differ; those are the
 * positions a generalization has to abstract. */
let disagreements =
    (~use_alphas: bool, e1: Exp.t, e2: Exp.t): list((Exp.t, Exp.t)) => {
  let rec go = (alphas, e1, e2, acc) => {
    let (e1, e2) = (unwrap(e1), unwrap(e2));
    if (alpha_equal(~alphas, e1, e2)) {
      acc;
    } else {
      let (c1, c2) = (children(e1), children(e2));
      if (c1 != []
          && List.length(c1) == List.length(c2)
          && same_head(~alphas, e1, e2)) {
        let alphas = use_alphas ? binder_alphas(e1, e2) @ alphas : alphas;
        List.fold_left2((acc, x, y) => go(alphas, x, y, acc), acc, c1, c2);
      } else {
        acc @ [(e1, e2)];
      };
    };
  };
  go([], e1, e2, []);
};

/* Assign a metavariable to each disagreement, sharing one name between
 * disagreements that are the same pair. That sharing is the difference
 * between *a* generalization and the *least general* one: `msg (f a a)
 * (f b b)` is `f $x1 $x1`, not `f $x1 $x2`. */
let assign_metavars =
    (pairs: list((Exp.t, Exp.t))): list(((Exp.t, Exp.t), pattern)) => {
  let slots: ref(list((Exp.t, Exp.t))) = ref([]);
  let index_of = ((a, b)) => {
    let rec find = (i, l) =>
      switch (l) {
      | [] =>
        slots := slots^ @ [(a, b)];
        List.length(slots^) - 1;
      | [(x, y), ...rest] =>
        alpha_equal(x, a) && alpha_equal(y, b) ? i : find(i + 1, rest)
      };
    find(0, slots^);
  };
  List.map(pair => (pair, metavar(nth_name(index_of(pair)))), pairs);
};

let generalize_at = (~use_alphas: bool, e1: Exp.t, e2: Exp.t): pattern => {
  let assigned = assign_metavars(disagreements(~use_alphas, e1, e2));
  List.fold_left(
    (acc, ((d1, _), mv)) =>
      ProofHacks.replace_exp_id(Exp.rep_id(d1), acc, mv),
    unwrap(e1),
    assigned,
  );
};

/* Does this pattern actually generalize both inputs? A generalization
 * that fails to match one of them is not a generalization at all, so
 * this is the correctness condition `msg` checks. */
let generalizes = (pat: pattern, e1: Exp.t, e2: Exp.t): bool =>
  MatchExp.matches_pattern(pat, e1) && MatchExp.matches_pattern(pat, e2);

/* The most specific generalization of two expressions.
 *
 * We first try with binder correspondence switched on, which lets a
 * variable bound on both sides generalize to itself rather than to a
 * metavariable (`fun u -> u + 1` and `fun v -> v + 2` give
 * `fun u -> u + $x1`, not `fun u -> $x1 + $x2`). Because that
 * correspondence over-approximates scope, we verify the result and, if
 * it does not in fact match both inputs, fall back to the version with
 * no binder correspondence, where every disagreement becomes a
 * metavariable and matching both inputs is guaranteed. */
let msg = (e1: Exp.t, e2: Exp.t): pattern => {
  let candidate = generalize_at(~use_alphas=true, e1, e2);
  generalizes(candidate, e1, e2)
    ? candidate : generalize_at(~use_alphas=false, e1, e2);
};

/* ---- Minimal discriminating patterns ------------------------------- */

/* Every subterm of `e` at exactly `depth` steps below the root. */
let at_depth = (depth: int, e: Exp.t): list(Exp.t) => {
  let rec go = (d, level) =>
    d >= depth ? level : go(d + 1, List.concat_map(children, level));
  go(0, [e]);
};

let max_depth = (e: Exp.t): int => {
  let rec go = (d, level) =>
    level == [] ? d : go(d + 1, List.concat_map(children, level));
  go(0, [e]);
};

/* The generalization ladder for a term: rung `d` keeps the top `d`
 * levels of structure and replaces everything below with `$e`. Ordered
 * most general first, ending at the ground term itself. `$e` alone is
 * rung 0 and is deliberately included: it is the right answer when the
 * goal is a single node. */
let generalizations = (e: Exp.t): list(pattern) => {
  let e = unwrap(e);
  let depth = max_depth(e);
  List.init(depth, d => blank_out(~to_=any, at_depth(d, e), e)) @ [e];
};

/* Every match of `pattern` in `goal`, in the traversal order that
 * `at <idx>` counts (ProofHacks.nth_exp_pat's order — matches are not
 * searched inside other matches). */
let matches_in = (~goal: Exp.t, pat: pattern): list(Exp.t) => {
  let acc = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e) =>
          if (MatchExp.matches_pattern(pat, e)) {
            acc := acc^ @ [e];
            e;
          } else {
            cont(e);
          },
      goal,
    );
  acc^;
};

/* Index of the subterm with id `id` among a pattern's matches. */
let index_of_id = (~goal: Exp.t, pat: pattern, id: Id.t): option(int) => {
  let rec find = (i, l) =>
    switch (l) {
    | [] => None
    | [e, ...rest] => Exp.rep_id(e) == id ? Some(i) : find(i + 1, rest)
    };
  find(0, matches_in(~goal, pat));
};

/* The most general pattern that still identifies the occurrence `id` in
 * `goal`, with that occurrence's index among the pattern's matches.
 *
 * We walk the generalization ladder from the most general rung down.
 * The first rung that matches the target *and nothing else* wins, and
 * its index is 0 — that is the "generalize until ambiguity, then back
 * off one step" rule. If no rung is unambiguous (the goal repeats the
 * term verbatim, say), we keep the ground term and disambiguate with
 * its occurrence index instead. */
let find = (~goal: Exp.t, id: Id.t): option((pattern, int)) =>
  switch (ProofHacks.find_exp_id(id, goal)) {
  | None => None
  | Some(target) =>
    let rungs = generalizations(target);
    let unambiguous =
      List.find_opt(
        pat =>
          switch (matches_in(~goal, pat)) {
          | [only] => Exp.rep_id(only) == id
          | _ => false
          },
        rungs,
      );
    switch (unambiguous) {
    | Some(pat) => Some((pat, 0))
    | None =>
      /* Fall back to the ground term plus an index. */
      let ground = unwrap(target);
      switch (index_of_id(~goal, ground, id)) {
      | Some(idx) => Some((ground, idx))
      | None => Some((ground, 0))
      };
    };
  };

/* Total version. Raises if `id` names no subterm of `goal`; every
 * caller obtains the id *from* the goal, so that is a programming
 * error rather than a case to handle. */
let discriminating_pattern = (~goal: Exp.t, id: Id.t): (pattern, int) =>
  switch (find(~goal, id)) {
  | Some(result) => result
  | None =>
    raise(
      Invalid_argument("AntiUnify.discriminating_pattern: id not in goal"),
    )
  };
