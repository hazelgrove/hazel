include TermBase.Label;

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
