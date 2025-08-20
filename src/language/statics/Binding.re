open Util;

/* The name and UUID of a variable binding site or reference.
 * In principle the name is redundant, but it is convienient
 * to have it */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = {
  id: Id.t,
  name: string,
};

/* A list of binding sites or references */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type s = list(t);

/* A binding site as specified by its id and a list of
 * the variables bound there*/
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type site = {
  id: Id.t,
  bound: s,
};

/* An enclosing list of binding sites */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type stem = list(site);

let variable_names = (bindings: s): list(string) =>
  List.map((b: t) => b.name, bindings);
