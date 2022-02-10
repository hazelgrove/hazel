/** Associates a type variable with its kind */
open Sexplib.Std;

module HTyp = HTypCore;
module Kind = Kind;

[@deriving sexp]
type binding = (string, Kind.t);

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

let bind = (tyvars: t, name: string, k: Kind.t): t => {
  let increment_singleton: binding => binding =
    fun
    | (name, Singleton(k', ty)) => {
        let k = Kind.Singleton(k', HTyp.increment_indices(ty));
        (name, k);
      }
    | binding => binding;
  let binding = increment_singleton((name, k));
  [binding, ...List.map(increment_singleton, tyvars)];
};

/* Accessors */

let binding = (tyvars: t, i: Index.t): option((string, Kind.t)) =>
  List.nth_opt(tyvars, i);

let kind = (vars: t, i: Index.t): option(Kind.t) =>
  List.nth_opt(vars, i) |> Option.map(snd);

let rec index = (tyvars: t, x: string): option(Index.t) =>
  switch (tyvars) {
  | [] => None
  | [(y, _), ...tyvars'] =>
    x == y ? Some(0) : Option.map((+)(1), index(tyvars', x))
  };
