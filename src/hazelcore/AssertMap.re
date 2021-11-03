open Sexplib.Std;

[@deriving sexp]
type t = list((KeywordID.t, list(AssertStatus.t)));

let lookup = List.assoc_opt;
let empty: t = [];

let extend = ((x, res): (KeywordID.t, AssertStatus.t), ctx: t): t => {
  switch (List.assoc_opt(x, ctx)) {
  | Some(a) => [(x, List.append(a, [res])), ...List.remove_assoc(x, ctx)]
  | None => [(x, [res]), ...ctx]
  };
};

let rec to_list = (map: t): list(string) => {
  switch (map) {
  | [x, ...xs] =>
    switch (x) {
    | (num, _) => [string_of_int(num), ...to_list(xs)]
    }
  | [] => []
  };
};

let check = AssertStatus.join_all;

let lookup_and_join = (n, assert_map): AssertStatus.t =>
  switch (lookup(n, assert_map)) {
  | None => Indet
  | Some(a) => check(a)
  };
