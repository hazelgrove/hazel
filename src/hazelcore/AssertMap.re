open Sexplib.Std;

[@deriving sexp]
type t = list((AssertNumber.t, list(AssertResult.t)));

let lookup = (x: AssertNumber.t, lst: t) => List.assoc_opt(x, lst);

let empty = [];

let extend = (xa: (AssertNumber.t, AssertResult.t), ctx: t): t => {
  let (x, res) = xa;
  switch (List.assoc_opt(x, ctx)) {
  | Some(a) => [(x, List.append(a, [res])), ...List.remove_assoc(x, ctx)]
  | None => [(x, [res]), ...ctx]
  };
};
/*
 let rec combine = (map1: t, map2: t) : t => {
   switch(map1){
     |[x, ...xs] =>
       let part2 = lookup(fst(x), map2);
       switch(part2){
         |Some(part2_2) =>
           if (snd(x) == part2_2){
             [x, ...combine(xs, List.remove_assoc(fst(x), map2))]
           }
           else if (List.length{

           }
         |None => [x, ... xs] @ map2
       }
   }
 }*/
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

let rec check = (result: list(AssertResult.t)): AssertResult.t =>
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
