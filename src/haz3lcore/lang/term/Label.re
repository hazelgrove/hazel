include TermBase.Label;
type label_expectation =
  | NoLabel
  | Some(t)
  | Syn;
let hole = (tms: list(TermBase.Any.t)): TermBase.Label.term =>
  switch (tms) {
  | [] => Hole
  | [_, ..._] => MultiHole(tms)
  };

let get_label = (t: t): option(string) =>
  switch (t.term) {
  | Label(l) => Some(l)
  | _ => None
  };

let rec join = (l1: t, l2: t): option(t) => {
  switch (l1.term, l2.term) {
  | (Label(l1), Label(l2)) when String.equal(l1, l2) =>
    Some(IdTagged.temp(Label(l1): term))
  | (Label(_), Label(_)) => None
  | (Label(_), Hole) => Some(l1)
  | (Hole, Label(_)) => Some(l2)
  | (Hole, Hole) => Some(IdTagged.temp(Hole: term))
  | (MultiHole(_), MultiHole(_)) => Some(IdTagged.temp(Hole: term)) // TODO: Investigate multiholes in types
  | (MultiHole(_), _) => Some(l2)
  | (_, MultiHole(_)) => Some(l1)
  };
};

let rep_id: t => Id.t = IdTagged.rep_id;
