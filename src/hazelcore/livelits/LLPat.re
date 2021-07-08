type t = LivelitName.t;

let of_uhpat = (OpSeq(_, seq): UHPat.t): option(t) =>
  switch (seq) {
  | S(Var(_, _, x), E) when LivelitName.is_valid(x) => Some(x)
  | _ => None
  };

let to_uhpat = (llp: t): UHPat.t => UHPat.(mk_OpSeq(S(var(llp), E)));
