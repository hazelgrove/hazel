/** Associates a type variable with its kind */
open Sexplib.Std;

[@deriving sexp]
type binding = (string, KindCore.t);

[@deriving sexp]
type t = list(binding);

/* Constants */

let empty: t = [];

/* Predicates */

let has_name = (tyvars: t, name: string): bool =>
  List.mem_assoc(name, tyvars);

let rec has_index = (tyvars: t, i: Index.t): bool =>
  switch (tyvars) {
  | [] => i == 0
  | [_, ...tyvars'] => i == 0 || has_index(tyvars', i - 1)
  };

/* Updaters */

let bind = (tyvars: t, name: string, k: KindCore.t): t => {
  let increment_singleton: binding => binding =
    fun
    | (name, Singleton(ty)) => {
        let k = KindCore.Singleton(HTypCore.increment_indices(ty));
        (name, k);
      }
    | binding => binding;
  let binding = increment_singleton((name, k));
  [binding, ...List.map(increment_singleton, tyvars)];
};

/* Accessors */

let binding = (tyvars: t, i: Index.t): option((string, KindCore.t)) =>
  List.nth_opt(tyvars, i);

let kind = (vars: t, i: Index.t): option(KindCore.t) =>
  List.nth_opt(vars, i) |> Option.map(snd);

let rec index = (tyvars: t, name: string): option(Index.t) =>
  switch (tyvars) {
  | [] => None
  | [(y, _), ...tyvars'] =>
    name == y ? Some(0) : Option.map((+)(1), index(tyvars', name))
  };
