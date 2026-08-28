open Util;

/* Co-contexts:

   A typing co-context (dual to a typing context), is a map between
   variable names and a list of that variable's uses within some scope.
   For each use, we retain the unique id and expected type of the use site.
   The co-ctx, along with the ctx, can be used to determine free and unused variables.

   The following definitions are useful:

   1. A locally free variable (in an expression) is one
      that occurs in the co_ctx of that expression.
   2. A global free variable (in the program) is one that
      occurs in the co_ctx but not the ctx of some expression
   3. A locally unused variable (in an expression) is one that
      occurs in the ctx but not the co-ctx of that expression
   4. A global unused variable (in the program) is one that
      occurs in the ctx but not the co_ctx of some expression

   The following theorems should hold:

   A. To determine if a variable is globally free, it suffices to consider
      expressions which are variable references (locus of ctx lookups)
   B. To determine if a variable is globally unused, it suffices to consider
      expressions which are the bodies of binding forms (locus of ctx extensions)

   */

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  id: Id.t,
  expected_ty: Typ.t,
};

/* Each co-context entry is a list of the uses of a variable within
   some scope, including their type demands. A name-keyed map
   representation was tried (2026-08-28) and REVERTED together with
   ctx-as-map: the DocSlides Mega-2k wedge it was hoped to fix
   persisted (900s cap, 1.57GB RSS), and frame benchmarks were
   unchanged (plans/perf-ledger.md stage A). The accessor API below
   remains the only sanctioned way to consume the representation. */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = VarMap.t_(list(entry));

let empty: t = VarMap.empty;

let to_list = (co_ctx: t): list((Var.t, list(entry))) => co_ctx;
let of_list = (l: list((Var.t, list(entry)))): t => l;

let lookup = (co_ctx: t, name: Var.t): option(list(entry)) =>
  VarMap.lookup(co_ctx, name);
let contains = (co_ctx: t, name: Var.t): bool =>
  VarMap.contains(co_ctx, name);
let names = (co_ctx: t): list(Var.t) => List.map(fst, co_ctx);
let filter_names = (pred: Var.t => bool, co_ctx: t): t =>
  VarMap.filter(((name, _)) => pred(name), co_ctx);

let mk = (ctx_before: Ctx.t, ctx_after, co_ctx: t): t => {
  let added_bindings = Ctx.added_bindings(ctx_after, ctx_before);
  filter_names(
    name =>
      switch (Ctx.lookup_var(added_bindings, name)) {
      | None => true
      | Some(_) => false
      },
    co_ctx,
  );
};

/* Merge co-contexts, combining entry lists for the same variable name. */
let union: list(t) => t =
  co_ctxs => {
    List.fold_left(
      (acc, co_ctx) =>
        List.fold_left(
          (acc, (name, entries)) =>
            if (VarMap.contains(acc, name)) {
              VarMap.update(acc, name, existing => existing @ entries);
            } else {
              VarMap.extend(acc, (name, entries));
            },
          acc,
          co_ctx,
        ),
      VarMap.empty,
      co_ctxs,
    );
  };

let singleton = (name, id, expected_ty): t => [
  (
    name,
    [
      {
        id,
        expected_ty,
      },
    ],
  ),
];

let meet: (Ctx.t, list(entry)) => Typ.t =
  (ctx, entries) => {
    let expected_tys = List.map(entry => entry.expected_ty, entries);
    switch (
      Typ.meet_all(~empty=Unknown(Internal) |> Typ.fresh, ctx, expected_tys)
    ) {
    | None => Unknown(Internal) |> Typ.fresh
    | Some(ty) => ty
    };
  };

let contains_hole = (co_ctx: t): bool => contains(co_ctx, "$hole");

let has_any = (co_ctx: t, vs: list(Var.t)): bool =>
  List.exists(v => contains(co_ctx, v), vs);

let of_bindings = (bindings: Binding.s): t =>
  union(
    List.map(
      (b: Binding.t) =>
        singleton(b.name, b.id, Typ.fresh(Unknown(Internal))),
      bindings,
    ),
  );
