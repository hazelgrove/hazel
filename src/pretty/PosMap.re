open Sexplib.Std;
// Maps keyed by an end position
// Invarient: keys are ascending and unique
[@deriving (sexp, show)]
type key = int;
[@deriving (sexp, show)]
type t('a) = list((key, 'a));
let empty: 'a. t('a) = [];
let singleton: 'a. (key, 'a) => t('a) = (pos, x) => [(pos, x)];
let rec union: 'a. (('a, 'a) => 'a, t('a), t('a)) => t('a) =
  (f, t1, t2) =>
    switch (t1, t2) {
    | ([], t_other) => t_other
    | (t_other, []) => t_other
    | ([(p1, x1), ...xs1], [(p2, x2), ...xs2]) =>
      if (p1 < p2) {
        [(p1, x1), ...union(f, xs1, [(p2, x2), ...xs2])];
      } else if (p1 > p2) {
        [(p2, x2), ...union(f, [(p1, x1), ...xs1], xs2)];
      } else {
        [(p1, f(x1, x2)), ...union(f, xs1, xs2)];
      }
    };
let rec map: 'a 'b. ('a => 'b, t('a)) => t('b) =
  f =>
    fun
    | [] => []
    | [(pos, x), ...rest] => [(pos, f(x)), ...map(f, rest)];
let rec mapi: 'a 'b. ((key, 'a) => 'b, t('a)) => t('b) =
  f =>
    fun
    | [] => []
    | [(pos, x), ...rest] => [(pos, f(pos, x)), ...mapi(f, rest)];
let rec mapk: 'a 'b. ((key, 'a) => (key, 'b), t('a)) => t('b) =
  f =>
    fun
    | [] => []
    | [(pos, x), ...rest] => [f(pos, x), ...mapk(f, rest)];
let rec fold_left: 'a 'b. ((key, 'b, 'a) => 'b, 'b, t('a)) => 'b =
  (f, z) =>
    fun
    | [] => z
    | [(pos, x), ...rest] => fold_left(f, f(pos, z, x), rest);
