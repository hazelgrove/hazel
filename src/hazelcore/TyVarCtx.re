/** Associates a type variable with its kind. */
open Sexplib.Std;

/*
 WARNING: This is the cycle-breaking glue between HTyp and Kind.

 In here, only use HTypSyntax and KindCore modules directly. Attempts to use
 HTyp or Kind directly will create a dependency cycle.
 */

[@deriving sexp]
type binding = (string, KindCore.t);

[@deriving sexp]
type t = list(binding);

/* Constants */
let empty: t = [];

/* Constructors */
let of_list = (bindings: list(binding)): t => bindings;

/* Predicates */
let has_name = (tyvars: t, name: string): bool =>
  List.mem_assoc(name, tyvars);
let rec has_index = (tyvars: t, i: Index.t): bool =>
  switch (tyvars) {
  | [] => i == 0
  | [_, ...tyvars'] => i == 0 || has_index(tyvars', i - 1)
  };

/* Updaters */
let bind = (tyvars: t, name: string, k: KindCore.t): t =>
  tyvars @ [(name, k)];
let unbind0 = (tyvars: t): t =>
  switch (tyvars) {
  | [] => failwith(__LOC__ ++ ": nothing to unbind")
  | [_, ...tyvars] =>
    let subst_kind = k =>
      KindCore.subst_tyvar(k, 0, KindCore.canonical_type(k));
    // List.map(
    //   ((name, k)) => (name, KindCore.decrement_indices(subst_kind(k))),
    //   tyvars,
    // );
    List.map(((name, k)) => (name, subst_kind(k)), tyvars);
  };

/* Accessors */
let bindings = (bindings: t): list(binding) => bindings;
let binding = (tyvars: t, i: Index.t): option((string, KindCore.t)) =>
  List.nth_opt(tyvars, i);
let kind = (vars: t, i: Index.t): option(KindCore.t) =>
  List.nth_opt(vars, i) |> Option.map(snd);
let name = (tyvars: t, i: Index.t): option(string) =>
  List.nth_opt(tyvars, i) |> Option.map(fst);
let rec index = (tyvars: t, name: string): option(Index.t) =>
  switch (tyvars) {
  | [] => None
  | [(y, _), ...tyvars'] =>
    name == y ? Some(0) : Option.map((+)(1), index(tyvars', name))
  };
