open Sexplib.Std;

[@deriving sexp]
type t = list((AssertNumber.t, list(AssertStatus.t)));

let lookup = List.assoc_opt;
let empty: t = [];

let extend = (xa: (AssertNumber.t, AssertStatus.t), ctx: t): t => {
  let (x, res) = xa;
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
    //| _ => failwith("unexpected on to_list")
    }
  | [] => []
  };
};

/*
 let rec check = (result: list(AssertStatus.t)): AssertStatus.t =>
   switch (result) {
   | [x, ...xs] =>
     switch (x) {
     | Pass =>
       if (xs == []) {
         Pass;
       } else {
         switch (check(xs)) {
         | Pass => Pass
         | _ => Comp
         };
       }
     | Fail =>
       if (xs == []) {
         Fail;
       } else {
         switch (check(xs)) {
         | Fail => Fail
         | _ => Comp
         };
       }
     | Indet => Indet
     | Comp => Comp
     //| _ => failwith("unexpected on check")
     }
   | [] => Indet
   };
   */

let check = AssertStatus.join_all;

let lookup_and_join = (n, assert_map): AssertStatus.t =>
  switch (lookup(n, assert_map)) {
  | None => Indet
  | Some(a) =>
    switch (check(a)) {
    | Pass => Pass
    | Fail => Fail
    | Comp => Comp
    | Indet => Indet
    }
  };
