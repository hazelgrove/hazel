/** Associates a type variable with its kind */
open Sexplib.Std;
open TyVar;

module HTyp = HTypCore;
module Kind = Kind;

[@deriving sexp]
type binding = (Name.t, Kind.t);

[@deriving sexp]
type t = list(binding);

/* Constants */

let empty: t = [];

/* Predicates */

let bound = (name: Name.t, vars: t): bool => vars |> List.mem_assoc(name);

let rec has_index = (i: Index.t, tyvars: t): bool =>
  switch (tyvars) {
  | [] => i == 0
  | [_, ...tyvars'] => i == 0 || tyvars' |> has_index(i - 1)
  };

/* Updaters */

let bind = (name: Name.t, k: Kind.t, tyvars: t): t => {
  let increment_singleton: binding => binding =
    fun
    | (name, Singleton(k', ty)) => {
        let k = Kind.Singleton(k', HTyp.increment_indices(ty));
        (name, k);
      }
    | binding => binding;
  let binding = increment_singleton((name, k));
  [binding, ...tyvars |> List.map(increment_singleton)];
};

/* Accessors */

let rec index = (x: Name.t, tyvars: t): option(Index.t) =>
  switch (tyvars) {
  | [] => None
  | [(y, _), ...tyvars'] =>
    x == y ? Some(0) : index(x, tyvars') |> Option.map((+)(1))
  };

let binding = (i: Index.t, tyvars: t): option((Name.t, Kind.t)) =>
  List.nth_opt(tyvars, i);

let kind = (i: Index.t, vars: t): option(Kind.t) =>
  List.nth_opt(vars, i) |> Option.map(snd);
