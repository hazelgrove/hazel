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
type entry('a) = {
  id: Id.t,
  expected_ty: Typ.t('a),
};

/* Each co-context entry is a list of the uses of a variable
   within some scope, including their type demands */
[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = VarMap.t_(list(entry('a)));

let empty: t('a) = VarMap.empty;

let mk = (ctx_before: Ctx.t('a), ctx_after, co_ctx: t('a)): t('a) => {
  let added_bindings = Ctx.added_bindings(ctx_after, ctx_before);
  VarMap.filter(
    ((name, _)) =>
      switch (Ctx.lookup_var(added_bindings, name)) {
      | None => true
      | Some(_) => false
      },
    co_ctx,
  );
};

/* Note: this currently shadows in the case of duplicates */
let union: list(t('a)) => t('a) =
  xs => List.fold_left((co_ctx1, co_ctx2) => co_ctx1 @ co_ctx2, [], xs);

let singleton = (name, id, expected_ty): t(IdTag.t) => [
  (name, [{id, expected_ty}]),
];

let join: (Ctx.t('a), list(entry('a))) => Typ.t('a) =
  (ctx, entries) => {
    let expected_tys = List.map(entry => entry.expected_ty, entries);
    switch (
      Typ.join_all(~empty=Unknown(Internal) |> Typ.fresh, ctx, expected_tys)
    ) {
    | None => Unknown(Internal) |> Typ.fresh
    | Some(ty) => ty
    };
  };
