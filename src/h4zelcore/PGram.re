type level = (Gram.t, Assoc.t);
type t = list(level);

let to_seq = (pg: t): Seq.t((Prec.t, level)) =>
  List.to_seq(pg) |> Seq.mapi((i, lvl) => (i, lvl));
