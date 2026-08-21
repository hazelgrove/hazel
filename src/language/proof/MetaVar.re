/* Pattern metavariables.
 *
 * Hazel's stepper filters already have a metavariable convention: inside
 * a `pause`/`eval`/`hide` filter pattern, `$e` stands for any
 * expression and `$v` for any value (see FilterMatcher.matches_exp,
 * which drives Equality's `use_expr_wildcards`, and Statics'
 * `is_in_filter` case that keeps them from reading as free variables).
 *
 * Proof-step target slots (`axiom ... at <idx> on <target>`,
 * `eval <target> at <idx>`) reuse that convention verbatim rather than
 * inventing a second one, so a user who has written a filter already
 * knows the syntax. Nothing new needs to parse: Token.var_regexp
 * already admits a `$`-prefixed name, so `$e` arrives as `Var("$e")`.
 *
 * This module is deliberately the dependency-light core of the feature:
 * classification and collection only. It is shared by the statics (to
 * suppress free-variable marks), the proof checker (to select a rewrite
 * site) and, later, the GUI generalization / extract-lemma tools.
 *
 * Deciding whether an expression is a *value* is not a syntactic
 * question, so, exactly as Equality does with `use_expr_wildcards`, we
 * take it as an injected predicate instead of depending on the
 * evaluator here. That keeps the notion of value-ness the stepper's own
 * rather than a proof-layer copy of it.
 */

open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  /* `$e`: any expression. Non-linear — each occurrence matches
   * independently, matching filter semantics (Equality returns `true`
   * unconditionally for `$e`). */
  | Any
  /* `$v`: any value, per the injected predicate. Also non-linear. */
  | Value
  /* `$x`, `$acc`, ...: a *named* metavariable. Unlike `$e`/`$v` these
   * are linear: every occurrence of the same name must match the same
   * expression. Filters have no named metavariables, so this is the one
   * genuine extension, and it rides on MatchExp's existing match_ctx
   * machinery rather than new code. */
  | Named(string);

/* What a matcher must supply to decide `$v`. */
type env = {is_value: Exp.t => bool};

let sigil = '$';
let any_name = "$e";
let value_name = "$v";

/* A `$`-prefixed name. `$` alone is not a metavariable. */
let is_meta_name = (name: string): bool =>
  String.length(name) > 1 && name.[0] == sigil;

let of_name = (name: string): option(t) =>
  if (!is_meta_name(name)) {
    None;
  } else if (name == any_name) {
    Some(Any);
  } else if (name == value_name) {
    Some(Value);
  } else {
    Some(Named(name));
  };

let name_of = (m: t): string =>
  switch (m) {
  | Any => any_name
  | Value => value_name
  | Named(name) => name
  };

/* Recognize a metavariable *node*. `Var` is the surface form; the
 * `Constructor` case mirrors Equality's wildcard cases, which see
 * metavariables in that shape on some post-elaboration filter paths. */
let of_exp = (e: Exp.t): option(t) =>
  switch (e |> Exp.term_of) {
  | Var(name)
  | Constructor(name, _) => of_name(name)
  | _ => None
  };

/* Every metavariable occurring in a pattern, first-occurrence order. */
let collect = (e: Exp.t): list(t) => {
  let acc = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e) => {
          switch (of_exp(e)) {
          | Some(m) when !List.mem(m, acc^) => acc := acc^ @ [m]
          | _ => ()
          };
          cont(e);
        },
      e,
    );
  acc^;
};

/* The names of the linear (named) metavariables in a pattern. */
let named_names = (e: Exp.t): list(string) =>
  e
  |> collect
  |> List.filter_map(
       fun
       | Named(name) => Some(name)
       | Any
       | Value => None,
     );

/* Does this expression use the pattern language at all? The checker
 * uses this to keep ground targets on their original, byte-identical
 * code path (see ProofHacks.nth_exp_target). */
let has_meta = (e: Exp.t): bool => collect(e) != [];

/* Does a metavariable match `exp`? Only meaningful for the non-linear
 * kinds; `Named` is resolved by the caller's match context. */
let matches = (~env: env, m: t, exp: Exp.t): bool =>
  switch (m) {
  | Any => true
  | Value => env.is_value(exp)
  | Named(_) => true
  };
