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
