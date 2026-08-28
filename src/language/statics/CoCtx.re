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

module NameMap = Map.Make(String);

/* Each co-context entry is a list of the uses of a variable within
   some scope, including their type demands. The map replaced an assoc
   list whose union was O(|a|*|b|): along a top-level chain the body
   co-ctx at depth k carries every free name of the rest of the
   program and was unioned at every level — a quadratic term in
   whole-program statics, and the amplifier in the exponential co_ctx
   blowups. Serialization stays the assoc-pairs shape (sorted by
   name). */
type t = NameMap.t(list(entry));

let to_list = (co_ctx: t): list((Var.t, list(entry))) =>
  NameMap.bindings(co_ctx);
let of_list = (l: list((Var.t, list(entry)))): t =>
  /* preserve assoc lookup semantics: FIRST binding of a name wins */
  List.fold_left(
    (m, (name, entries)) =>
      NameMap.mem(name, m) ? m : NameMap.add(name, entries, m),
    NameMap.empty,
    l,
  );

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent = list((Var.t, list(entry)));
let sexp_of_t = (co_ctx: t) => sexp_of_persistent(to_list(co_ctx));
let t_of_sexp = s => of_list(persistent_of_sexp(s));
let yojson_of_t = (co_ctx: t) => yojson_of_persistent(to_list(co_ctx));
let t_of_yojson = j => of_list(persistent_of_yojson(j));
let pp = (fmt, co_ctx: t) => pp_persistent(fmt, to_list(co_ctx));
let show = (co_ctx: t) => show_persistent(to_list(co_ctx));

let empty: t = NameMap.empty;

let lookup = (co_ctx: t, name: Var.t): option(list(entry)) =>
  NameMap.find_opt(name, co_ctx);
let contains = (co_ctx: t, name: Var.t): bool => NameMap.mem(name, co_ctx);
let names = (co_ctx: t): list(Var.t) =>
  NameMap.bindings(co_ctx) |> List.map(fst);
let filter_names = (pred: Var.t => bool, co_ctx: t): t =>
  NameMap.filter((name, _) => pred(name), co_ctx);

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

/* Merge co-contexts, combining entry lists for the same variable name
   (earlier operands' uses first, as before). */
let union: list(t) => t =
  co_ctxs =>
    List.fold_left(
      (acc, co_ctx) =>
        NameMap.union(
          (_, existing, entries) => Some(existing @ entries),
          acc,
          co_ctx,
        ),
      NameMap.empty,
      co_ctxs,
    );

let singleton = (name, id, expected_ty): t =>
  NameMap.singleton(
    name,
    [
      {
        id,
        expected_ty,
      },
    ],
  );

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
