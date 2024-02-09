/*

 ((n+1)*((n+1)+1))/2
 = ((n+1) * (n+(1+1)))/2 [assoc+]
 = ((n+1) * (n+2))/2 [eval 1+1]
 = (((n + 1) * n) + ((n + 1) * 2))/2 [dist+*]
 = ((n + 1) * n)/2 + ((n + 1) * 2)/2 [dist+/]
 = ((n + 1) * n)/2 + (n+1)*2*(1/2) [x/y = x*1/y]
 = ((n + 1) * n)/2 + (n+1)*(2*(1/2)) [assoc*]
 = ((n + 1) * n)/2 + (n+1)*1 [eval 2*(1/2)]
 = ((n + 1) * n)/2 + (n+1) [id*]

 */

[@deriving (show({with_path: false}), sexp, yojson)]
type rule =
  | Comm
  | AssocL
  | AssocR
  | DistL
  | DistR
  | DivToMultL
  | DivToMultR
  | IdTimesL
  | IdTimesR;

let string_of_rule = rule => show_rule(rule); // TODO: Make nicer

let transform = (rule: rule, exp: DHExp.t): option(DHExp.t) => {
  switch (rule, exp) {
  | (Comm, BinIntOp(op, e1, e2)) => Some(BinIntOp(op, e2, e1))
  | (AssocL, BinIntOp(op1, e1, BinIntOp(op2, e2, e3))) =>
    Some(BinIntOp(op1, BinIntOp(op2, e1, e2), e3))
  | (AssocR, BinIntOp(op1, BinIntOp(op2, e1, e2), e3)) =>
    Some(BinIntOp(op1, e1, BinIntOp(op2, e2, e3)))
  | (DistL, BinIntOp(op1, BinIntOp(op2, e1, e2), e3)) =>
    Some(BinIntOp(op2, BinIntOp(op1, e1, e3), BinIntOp(op1, e2, e3)))
  | (DistR, BinIntOp(op2, BinIntOp(op1, e1, e3), BinIntOp(op1', e2, e3')))
      when e3 == e3' && op1' == op1 =>
    Some(BinIntOp(op1, BinIntOp(op2, e1, e2), e3))
  | (DivToMultL, BinIntOp(Divide, e1, e2)) =>
    Some(BinIntOp(Times, e1, BinIntOp(Divide, IntLit(2), e2)))
  | (DivToMultR, BinIntOp(Times, e1, BinIntOp(Divide, IntLit(2), e2))) =>
    Some(BinIntOp(Divide, e1, e2))
  | (IdTimesL, BinIntOp(Times, e1, IntLit(1))) => Some(e1)
  | (IdTimesR, e1) => Some(BinIntOp(Times, e1, IntLit(1)))
  | _ => None
  };
};

let n_to_rule = (n: int): rule => {
  switch (n) {
  | 0 => Comm
  | 1 => AssocL
  | 2 => AssocR
  | 3 => DistL
  | 4 => DistR
  | 5 => DivToMultL
  | 6 => DivToMultR
  | 7 => IdTimesL
  | _ => IdTimesR
  };
};
