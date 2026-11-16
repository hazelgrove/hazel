/*

  _____ ____  _____ _____ __     __    _     ____   ____
 |  ___|  _ \| ____| ____|\ \   / /   / \   |  _ \ / ___|
 | |_  | |_) |  _| |  _|   \ \ / /   / _ \  | |_) |\___ \
 |  _| |  _ <| |___| |___   \ V /   / ___ \ |  _ <  ___) |
 |_|   |_| \_\_____|_____|   \_/   /_/   \_\|_| \_\|____/


 Capture avoidance: the rules FreeVars and Substitution both follow.

 A closure, `Closure(env, body)` or a fixpoint carrying its environment
 `FixF(p, body, Some(env))`, is a scope barrier: inside it only `env`
 applies. No binder around it binds a name inside it, and no outer
 substitution replaces one. So a name free in a term is of one of two kinds:

 - open: an enclosing binder can bind it, and a substitution can replace it;
 - sealed: it is free inside a closure (free in the body and not bound by
   the closure's environment, free in an environment value the body uses,
   or sealed further in), so nothing outside the closure reaches it. Sealed
   is sealed: nesting needs no levels.

 Substituting an environment into a term (Substitution.in_exp) replaces the
 term's open variables, and sealed names pass through. An entry `x ↦ x`
 replaces nothing. A closure is expanded with its own environment, and a
 fixpoint carrying one is unrolled under it. Then:

 - No name appears or disappears. The names free in the result, which has
   no closures, are the term's open names less those replaced, the names
   free in the values inserted, and the term's sealed names.
 - A binder is renamed only if its name is sealed beneath it or free in a
   value inserted beneath it: expanding closures and inserting values puts
   those names under the binder as plain syntax. The new name avoids every
   name free beneath the binder.

 Expressions inside types (`proof_of`) are outside these rules. The terms
 both see are elaborated, so their modules are already lowered to `let`s:
 neither treats a module item as a binder.

 `exp` sorts a term's free names: `vars` are its open variables, `sealed` its
 sealed names. */

module Names = Util.Sets.StringSet;

type t = {
  vars: Names.t,
  sealed: Names.t,
};

let empty = {
  vars: Names.empty,
  sealed: Names.empty,
};

let union = (a: t, b: t): t => {
  vars: Names.union(a.vars, b.vars),
  sealed: Names.union(a.sealed, b.sealed),
};

/* Every name free in the term, of either kind. */
let all = (names: t): Names.t => Names.union(names.vars, names.sealed);

/* The value `env` puts in place of `x`, if it replaces `x`. */
let replacement = (env: Environment.t(Exp.t), x: Var.t): option(Exp.t) =>
  switch (Environment.lookup(env, x)) {
  | Some({term: Var(y), _}) when String.equal(y, x) => None
  | found => found
  };

/* Terms' names keyed by physical identity, so a substitution reads a binder's
   body or an environment value from here instead of walking it again. */
module Memo =
  Stdlib.Hashtbl.Make({
    type t = Exp.t;
    let equal = phys_equal;
    let hash = Stdlib.Hashtbl.hash;
  });

let rec exp = (~memo: option(Memo.t(t))=?, e: Exp.t): t =>
  switch (Option.bind(memo, ~f=memo => Memo.find_opt(memo, e))) {
  | Some(names) => names
  | None =>
    /* Names met directly, and the names of the parts below a binder or in
       a closure, combined once at the end. */
    let vars = ref([]);
    let parts = ref([]);
    let add = names => parts := [names, ...parts^];
    let under = (p: Pat.t, body: Exp.t) => {
      let names = exp(~memo?, body);
      {
        ...names,
        vars:
          List.fold(
            ~f=(vars, x) => Names.remove(x, vars),
            ~init=names.vars,
            Pat.bound_vars(p),
          ),
      };
    };
    let _ =
      Exp.map_term(
        ~f_exp=
          (cont, e) =>
            switch (Exp.term_of(e)) {
            | Var(x) =>
              vars := [x, ...vars^];
              e;
            | Fun(p, body, _, _)
            | Forall(p, body)
            | FixF(p, body, None) =>
              add(under(p, body));
              e;
            | Let(p, def, body)
            | Theorem(p, def, body) =>
              add(exp(~memo?, def));
              add(under(p, body));
              e;
            | Match(scrut, rules) =>
              add(exp(~memo?, scrut));
              List.iter(~f=((p, body)) => add(under(p, body)), rules);
              e;
            | Closure(env, body) =>
              add(seal(~memo?, env, exp(~memo?, body)));
              e;
            | FixF(p, body, Some(env)) =>
              add(seal(~memo?, env, under(p, body)));
              e;
            | Invalid(_)
            | Constructor(_)
            | EmptyHole
            | MultiHole(_)
            | Deferral(_)
            | Undefined
            | Atom(_)
            | Label(_)
            | ExplicitNonlabel
            | LivelitName(_)
            | BuiltinFun(_) => e
            | DynamicErrorHole(_)
            | DrvQuote(_)
            | ListLit(_)
            | TypFun(_)
            | Tuple(_)
            | TupLabel(_)
            | Dot(_)
            | ProofObject(_)
            | TyAlias(_)
            | Use(_)
            | Ap(_)
            | TypAp(_)
            | DeferredAp(_)
            | If(_)
            | Seq(_)
            | Test(_)
            | HintedTest(_)
            | Filter(_)
            | Parens(_)
            | Projector(_)
            | Cons(_)
            | ListConcat(_)
            | UnOp(_)
            | BinOp(_)
            | TupleExtension(_)
            | Asc(_)
            | Module(_)
            | ModuleExp(_) => cont(e)
            },
        e,
      );
    let names =
      List.fold(
        ~f=union,
        ~init={
          vars: Names.of_list(vars^),
          sealed: Names.empty,
        },
        parts^,
      );
    Option.iter(~f=memo => Memo.add(memo, e, names), memo);
    names;
  }

/* The names of a closure with environment `env` around a body with names
   `inner`: everything free inside is sealed outside. */
and seal =
    (~memo: option(Memo.t(t))=?, env: Environment.t(Exp.t), inner: t): t => {
  ...empty,
  sealed:
    Names.fold(
      (x, sealed) =>
        switch (replacement(env, x)) {
        | Some(value) => Names.union(all(exp(~memo?, value)), sealed)
        | None => Names.add(x, sealed)
        },
      inner.vars,
      inner.sealed,
    ),
};
