open Util;
open OptUtil.Syntax;

/* Locating the subterm a proof step targets.
 *
 * A step names its target as `at <n> on <exp>`: the (n+1)th subterm of
 * the goal that equals `<exp>`, counting in `map_term` order and not
 * descending into a counted match. The stepper writes `n` with `index`;
 * the checker reads it back with `nth_env_target`. */

/* How many subterms equal to `e1` come before the node with `e1`'s id in
 * `e2`. `None` if that id is not in `e2`. */
let index = (e1: Exp.t, e2: Exp.t): option(int) => {
  let count = ref(0);
  let found = ref(false);
  let _: Exp.t =
    Exp.map_term(
      ~f_exp=
        (cont, exp) =>
          if (found^) {
            exp;
          } else if (Exp.rep_id(exp) == Exp.rep_id(e1)) {
            found := true;
            exp;
          } else if (Equality.ignoring_ascriptions.exp(exp, e1)) {
            incr(count);
            exp;
          } else {
            cont(exp);
          },
      e2,
    );
  found^ ? Some(count^) : None;
};

/* The (n+1)th subterm of `e` that `matches`. */
let nth_matching = (matches: Exp.t => bool, n: int, e: Exp.t): option(Exp.t) => {
  let count = ref(0);
  let found = ref(None);
  let _: Exp.t =
    Exp.map_term(
      ~f_exp=
        (cont, exp) =>
          switch (found^) {
          | Some(_) => exp
          | None when matches(exp) =>
            if (count^ == n) {
              found := Some(exp);
            } else {
              incr(count);
            };
            exp;
          | None => cont(exp)
          },
      e,
    );
  found^;
};

/* The (n+1)th occurrence of `e1` in `e2`. */
let nth = (e1: Exp.t, n: int, e2: Exp.t): option(Exp.t) =>
  nth_matching(exp => Equality.ignoring_ascriptions.exp(exp, e1), n, e2);

/* The (n+1)th subterm of `e2` that matches `pat`, an expression that may
 * contain `$e`/`$v`/`$x` metavariables (see MetaVar). A metavariable-free
 * pattern matches exactly the terms it is structurally equal to, so
 * indexing among matches generalizes indexing among occurrences. */
let nth_pattern =
    (
      ~info_map: Statics.Map.t=Statics.Map.empty,
      ~env: Environment.t(Exp.t)=Environment.empty,
      pat: Exp.t,
      n: int,
      e2: Exp.t,
    )
    : option(Exp.t) =>
  nth_matching(MatchExp.matches_pattern(~info_map, ~env, pat), n, e2);

/* The subterm an `on`/`at` target slot names. Pattern matching
 * (MatchExp) and the equality `nth` uses (Equality) are different
 * relations, so only a target that uses the pattern language goes
 * through the matcher; a ground target is compared with Equality. */
let nth_target =
    (
      ~info_map: Statics.Map.t=Statics.Map.empty,
      ~env: Environment.t(Exp.t)=Environment.empty,
      at_exp: Exp.t,
      n: int,
      e2: Exp.t,
    )
    : option(Exp.t) =>
  MetaVar.has_meta(at_exp)
    ? nth_pattern(~info_map, ~env, at_exp, n, e2) : nth(at_exp, n, e2);

/* `nth_target` with both sides env-substituted first.
 *
 * Proof-level `eval` steps run the evaluator, which substitutes the
 * runtime env into branch bodies and so inlines recursive bindings like
 * `rev` as `FixF(Var(name), ...)`. Targets written in later steps still
 * say `Var(name)`, so both sides are substituted before matching, and
 * the match is mapped back to the unsubstituted `e2` by `Exp.rep_id`,
 * so callers see the goal as the user wrote it. `Substitution.in_exp`
 * preserves the ids of wrapping nodes (`Ap`, `Tuple`, `BinOp`, ...), so
 * the id is present in `e2`. It leaves metavariables alone, since no
 * binding can be named `$...`.
 *
 * Equality compares up to parens/projectors, ascriptions,
 * alpha-renaming (expression and type), function names, filters and
 * hole provenance, but not up to elaboration-supplied type information.
 * Two same-named constructors at different types are different terms,
 * and a step must not retarget across them. So both sides must be
 * elaborated: an unelaborated `Constructor(c, None)` never equals its
 * elaborated `Constructor(c, Some(Some(ty)))`, and the mismatch reads as
 * "target not found". This is why the theorem context records
 * elaborated statements (`Statics`' Theorem arm):
 * `ProofCtx.of_theorem_ctx` splices a cited rule's right-hand side into
 * elaborated goals. */
let nth_env_target =
    (
      ~info_map: Statics.Map.t,
      ~env: Environment.t(Exp.t),
      at_exp: Exp.t,
      n: int,
      e2: Exp.t,
    )
    : option(Exp.t) => {
  let* matched =
    nth_target(
      ~info_map,
      ~env,
      Substitution.in_exp(env, at_exp),
      n,
      Substitution.in_exp(env, e2),
    );
  Exp.find_by_id(Exp.rep_id(matched), e2);
};

/* `exp` with the node whose id is `id` replaced by a fresh copy of
 * `new_exp`. */
let replace_by_id = (id: Id.t, exp: Exp.t, new_exp: Exp.t) =>
  Exp.map_term(
    ~f_exp=
      (cont, exp) =>
        if (Exp.rep_id(exp) == id) {
          new_exp |> Exp.replace_all_ids;
        } else {
          cont(exp);
        },
    exp,
  );

/* Immediate expression children, in `map_term` order. Used to walk two
 * trees in lockstep when one is the env-substituted display of the other:
 * wrapping nodes (`Ap`, `BinOp`, …) keep their ids, but a `Var` may
 * become a `Fun`/`FixF` with a fresh id. */
let children = (e: Exp.t): list(Exp.t) => {
  let kids = ref([]);
  let root_id = Exp.rep_id(e);
  let _: Exp.t =
    Exp.map_term(
      ~f_exp=
        (cont, inner) =>
          if (Exp.rep_id(inner) == root_id) {
            cont(inner);
          } else {
            kids := [inner, ...kids^];
            inner;
          },
      e,
    );
  List.rev(kids^);
};

/* The id of the node in `onto` that occupies the same position as `id`
 * in `from`. `Some(id)` when the id survived substitution; the inlined
 * function when a `Var` was replaced by a `Fun`/`FixF`/`Closure`. */
let rec map_rep_id = (~from: Exp.t, ~onto: Exp.t, id: Id.t): option(Id.t) =>
  if (Exp.rep_id(from) == id) {
    Some(Exp.rep_id(onto));
  } else {
    let from_kids = children(from);
    let onto_kids = children(onto);
    List.length(from_kids) == List.length(onto_kids)
      ? List.fold_left2(
          (acc, f, o) =>
            switch (acc) {
            | Some(_) as s => s
            | None => map_rep_id(~from=f, ~onto=o, id)
            },
          None,
          from_kids,
          onto_kids,
        )
      : None;
  };
