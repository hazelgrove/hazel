open Util;

/* Co-contexts:

   A typing co-context (dual to a typing context), is a map between
   names and a list of their uses within some scope.
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
type sort =
  | Value
  | Constructor
  | Alias;

type demand = Typ.t => Typ.t;
// Slices are opaque after serialization, so their demand closures can be too.
let pp_demand = (fmt: Format.formatter, _: demand) =>
  Format.pp_print_string(fmt, "<demand>");
let sexp_of_demand = (_: demand): Sexplib.Sexp.t => Sexplib.Sexp.List([]);
let demand_of_sexp = (_: Sexplib.Sexp.t): demand => Fun.id;
let yojson_of_demand = (_: demand): Yojson.Safe.t => `Null;
let demand_of_yojson = (_: Yojson.Safe.t): demand => Fun.id;

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  sort,
  id: Id.t,
  expected_ty: Typ.t,
  demanded: demand,
};

/* Each co-context entry is a list of the uses of a variable
   within some scope, including their type demands */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = VarMap.t_(list(entry));

let empty: t = VarMap.empty;

let filter_sort = (sort: sort, co_ctx: t): t =>
  List.fold_left(
    (acc, (name, entries)) => {
      let entries = List.filter(entry => entry.sort == sort, entries);
      switch (entries) {
      | [] => acc
      | _ => VarMap.extend(acc, (name, entries))
      };
    },
    empty,
    co_ctx,
  );

let values = filter_sort(Value);

let entries_at = (id: Id.t, co_ctx: t): list((string, entry)) =>
  List.concat_map(
    ((name, entries)) =>
      entries
      |> List.filter(entry => entry.id == id)
      |> List.map(entry => (name, entry)),
    co_ctx,
  );

let mk = (ctx_before: Ctx.t, ctx_after, co_ctx: t): t => {
  let added_bindings = Ctx.added_bindings(ctx_after, ctx_before);
  List.fold_left(
    (acc, (name, entries)) => {
      let entries =
        List.filter(
          entry =>
            switch (entry.sort) {
            | Value => Ctx.lookup_var(added_bindings, name) == None
            | Constructor => Ctx.lookup_ctr(added_bindings, name) == None
            | Alias => Ctx.lookup_tvar(added_bindings, name) == None
            },
          entries,
        );
      switch (entries) {
      | [] => acc
      | _ => VarMap.extend(acc, (name, entries))
      };
    },
    empty,
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

let singleton = (~sort=Value, ~demanded=Fun.id, name, id, expected_ty): t => [
  (
    name,
    [
      {
        sort,
        id,
        expected_ty,
        demanded,
      },
    ],
  ),
];

let meet: (Ctx.t, list(entry)) => Typ.t =
  (ctx, entries) => {
    let expected_tys =
      entries
      |> List.filter(entry => entry.sort == Value)
      |> List.map(entry => entry.expected_ty);
    switch (
      Typ.meet_all(~empty=Unknown(Internal) |> Typ.fresh, ctx, expected_tys)
    ) {
    | None => Unknown(Internal) |> Typ.fresh
    | Some(ty) => ty
    };
  };

let contains_hole = (co_ctx: t): bool =>
  VarMap.lookup(values(co_ctx), "$hole") !== None;

let has_any = (co_ctx: t, vs: list(Var.t)): bool => {
  let co_ctx = values(co_ctx);
  List.exists(v => VarMap.contains(co_ctx, v), vs);
};

let of_bindings = (bindings: Binding.s): t =>
  List.map(
    (b: Binding.t) =>
      (
        b.name,
        [
          {
            sort: Value,
            id: b.id,
            expected_ty: Typ.fresh(Unknown(Internal)),
            demanded: Fun.id,
          },
        ],
      ),
    bindings,
  );
