open Sexplib.Std;

/* Cheap DHExp rewriting */

[@deriving (show({with_path: false}), sexp, yojson)]
type rule =
  | IdPlusL
  | CommPlus
  | AssocPlusL
  | AssocPlusR
  | IdTimesL
  | CommTimes
  | AssocTimesL
  | AssocTimesR
  | DistPlusTimesL
  | DistPlusTimesR
  | DivDefL
  | DivDefR;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  name: rule,
  t: DHExp.t => option(DHExp.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type s = list(t);

let string_of = r =>
  switch (r.name) {
  | IdPlusL => "Iden(L, +)"
  | CommPlus => "Comm(+)"
  | AssocPlusL => "Assoc(L, +)"
  | AssocPlusR => "Assoc(R, +)"
  | IdTimesL => "Iden(L, *)"
  | CommTimes => "Comm(*)"
  | AssocTimesL => "Assoc(L, *)"
  | AssocTimesR => "Assoc(R, *)"
  | DistPlusTimesL => "Dist(L, +, *)"
  | DistPlusTimesR => "Dist(R, +, *)"
  | DivDefL => "DivDef(L)"
  | DivDefR => "DivDef(R)"
  };

let rewrites: list(t) = [
  {
    name: CommTimes,
    t:
      fun
      | BinIntOp(Times, e1, e2) => Some(BinIntOp(Times, e2, e1))
      | _ => None,
  },
  {
    name: AssocTimesL,
    t:
      fun
      | BinIntOp(Times, e1, BinIntOp(Times, e2, e3)) =>
        Some(BinIntOp(Times, BinIntOp(Times, e1, e2), e3))
      | _ => None,
  },
  {
    name: AssocTimesR,
    t:
      fun
      | BinIntOp(Times, BinIntOp(Times, e1, e2), e3) =>
        Some(BinIntOp(Times, e1, BinIntOp(Times, e2, e3)))
      | _ => None,
  },
  {
    name: CommPlus,
    t:
      fun
      | BinIntOp(Plus, e1, e2) => Some(BinIntOp(Plus, e2, e1))
      | _ => None,
  },
  {
    name: AssocPlusL,
    t:
      fun
      | BinIntOp(Plus, e1, BinIntOp(Plus, e2, e3)) =>
        Some(BinIntOp(Plus, BinIntOp(Plus, e1, e2), e3))
      | _ => None,
  },
  {
    name: AssocPlusR,
    t:
      fun
      | BinIntOp(Plus, BinIntOp(Plus, e1, e2), e3) =>
        Some(BinIntOp(Plus, e1, BinIntOp(Plus, e2, e3)))
      | _ => None,
  },
  {
    name: DistPlusTimesL,
    t:
      fun
      | BinIntOp(Times, BinIntOp(Plus, e1, e2), e3) =>
        Some(
          BinIntOp(Plus, BinIntOp(Times, e1, e3), BinIntOp(Times, e2, e3)),
        )
      | _ => None,
  },
  {
    name: DistPlusTimesR,
    t:
      fun
      | BinIntOp(Plus, BinIntOp(Times, e1, e3), BinIntOp(Times, e2, e3'))
          when e3 == e3' =>
        Some(BinIntOp(Times, BinIntOp(Plus, e1, e2), e3))
      | _ => None,
  },
  {
    name: DivDefL,
    t:
      fun
      | BinIntOp(Divide, e1, e2) =>
        Some(BinIntOp(Times, e1, BinIntOp(Divide, IntLit(2), e2)))
      | _ => None,
  },
  {
    name: DivDefR,
    t:
      fun
      | BinIntOp(Times, e1, BinIntOp(Divide, IntLit(2), e2)) =>
        Some(BinIntOp(Divide, e1, e2))
      | _ => None,
  },
  {
    name: IdTimesL,
    t:
      fun
      | BinIntOp(Times, e1, IntLit(1)) => Some(e1)
      | _ => None,
  },
  {
    name: IdPlusL,
    t:
      fun
      | BinIntOp(Plus, e1, IntLit(0)) => Some(e1)
      | _ => None,
  },
];

let go = (r: t) => r.t;

let matching_rewrites = (e: DHExp.t): list(t) =>
  List.filter_map(
    r =>
      switch (go(r, e)) {
      | Some(_) => Some(r)
      | None => None
      },
    rewrites,
  );
