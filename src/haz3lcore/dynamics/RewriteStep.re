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
  | NilTimesL
  | DistPlusTimesL
  | DistPlusTimesR
  | DistPlusTimesLC
  | DistPlusTimesRC
  | DistPlusDivL
  | DistPlusDivR
  | DefDivL
  | DefDivR
  | AssocTimesDivL
  | AssocTimesDivR;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  name: rule,
  t: DHExp.t => option(DHExp.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type s = list(t);

let string_of = r =>
  switch (r.name) {
  | IdPlusL => "Iden(+)L" // A+0=A
  | CommPlus => "Comm(+)" // A+B=B+A
  | AssocPlusL => "Assoc(+)L" // A+(B+C)=(A+B)+C
  | AssocPlusR => "Assoc(+)R" // (A+B)+C=A+(B+C)
  | IdTimesL => "Iden(×)L" // A*1=A
  | NilTimesL => "Nil(×)L" // A*0=0
  | CommTimes => "Comm(×)" // A*B=B*A
  | AssocTimesL => "Assoc(×)L" // A*(B*C)=(A*B)*C
  | AssocTimesR => "Assoc(×)R" // (A*B)*C=A*(B*C)
  | DistPlusTimesL => "Dist(+ ×)L" // (A+B)*C=A*C+B*C
  | DistPlusTimesR => "Dist(+ ×)R" // A*C+B*C=(A+B)*C
  | DistPlusTimesLC => "Dist(+ ×)L∘C" // A*(B+C)=A*B+A*C
  | DistPlusTimesRC => "Dist(+ ×)R∘C" // A*B+A*C=A*(B+C)
  | DefDivL => "Def(÷)L" // A/B=A*1/B
  | DefDivR => "Def(÷)R" // A*1/B=A/B
  | DistPlusDivL => "Dist(+ ÷)L" // (A+B)/C=A/C+B/C
  | DistPlusDivR => "Dist(+ ÷)R" // A/C+B/C=(A+B)/C
  | AssocTimesDivL => "Assoc(×)L∘Def(÷)" // A*(B/C)=(A*B)/C
  | AssocTimesDivR => "Assoc*(×)R∘Def(÷)" // (A*B)/C=A*(B/C)
  };

let symbolic_string_of = r =>
  switch (r.name) {
  | IdPlusL => "A+0=A"
  | CommPlus => "A+B=B+A"
  | AssocPlusL => "A+(B+C)=(A+B)+C"
  | AssocPlusR => "(A+B)+C=A+(B+C)"
  | IdTimesL => "A×1=A"
  | NilTimesL => "A×0=0"
  | CommTimes => "A×B=B×A"
  | AssocTimesL => "A×(B×C)=(A×B)×C"
  | AssocTimesR => "(A×B)×C=A×(B×C)"
  | DistPlusTimesL => "(A+B)×C=A×C+B×C"
  | DistPlusTimesR => "A×C+B×C=(A+B)×C"
  | DistPlusTimesLC => "A×(B+C)=A×B+A×C"
  | DistPlusTimesRC => "A×B+A*C=A×(B+C)"
  | DefDivL => "A÷B=A×1÷B"
  | DefDivR => "A×1÷B=A÷B"
  | DistPlusDivL => "(A+B)÷C=A÷C+B÷C"
  | DistPlusDivR => "A÷C+B÷C=(A+B)÷C"
  | AssocTimesDivL => "A×(B÷C)=(A×B)÷C"
  | AssocTimesDivR => "(A×B)÷C=A×(B÷C)"
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
    name: DistPlusTimesLC,
    t:
      fun
      | BinIntOp(Times, e1, BinIntOp(Plus, e2, e3)) =>
        Some(
          BinIntOp(Plus, BinIntOp(Times, e1, e2), BinIntOp(Times, e1, e3)),
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
    name: DistPlusTimesRC,
    t:
      fun
      | BinIntOp(Plus, BinIntOp(Times, e1, e2), BinIntOp(Times, e1', e3))
          when e1 == e1' =>
        Some(BinIntOp(Times, e1, BinIntOp(Plus, e2, e3)))
      | _ => None,
  },
  {
    name: DefDivL,
    t:
      fun
      | BinIntOp(Divide, e1, e2) =>
        Some(BinIntOp(Times, e1, BinIntOp(Divide, IntLit(1), e2)))
      | _ => None,
  },
  {
    name: DefDivR,
    t:
      fun
      | BinIntOp(Times, e1, BinIntOp(Divide, IntLit(1), e2)) =>
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
  {
    name: NilTimesL,
    t:
      fun
      | BinIntOp(Times, _, IntLit(0)) => Some(IntLit(0))
      | _ => None,
  },
  {
    name: DistPlusDivL,
    t:
      fun
      | BinIntOp(Divide, BinIntOp(Plus, e1, e2), e3) =>
        Some(
          BinIntOp(
            Plus,
            BinIntOp(Divide, e1, e3),
            BinIntOp(Divide, e2, e3),
          ),
        )
      | _ => None,
  },
  {
    name: DistPlusDivR,
    t:
      fun
      | BinIntOp(Plus, BinIntOp(Divide, e1, e3), BinIntOp(Divide, e2, e3'))
          when e3 == e3' =>
        Some(BinIntOp(Divide, BinIntOp(Plus, e1, e2), e3))
      | _ => None,
  },
  {
    name: AssocTimesDivL,
    t:
      fun
      | BinIntOp(Times, e1, BinIntOp(Divide, e2, e3)) =>
        Some(BinIntOp(Divide, BinIntOp(Times, e1, e2), e3))
      | _ => None,
  },
  {
    name: AssocTimesDivR,
    t:
      fun
      | BinIntOp(Divide, BinIntOp(Times, e1, e2), e3) =>
        Some(BinIntOp(Times, e1, BinIntOp(Divide, e2, e3)))
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
