open Util;

include Label.Map;
type t = Label.Map.t(list(Mold.t));

let union2 = union((_, ms_l, ms_r) => Some(ms_l @ ms_r));
let union = List.fold_left(union2, empty);

let of_regex = (s: Sort.t, p: Prec.t, g: Regex.t(Sort.t)): t => {
  let mk = uz => Mold.mk(~frames=uz, s, p);
  let rec go = (uz: Regex.Unzipped.s(_), g: Regex.t(_)) =>
    switch (g) {
    | Atom(Kid(_)) => empty
    | Atom(Tok(t)) => singleton(t, [mk(uz)])
    | Star(g) => go([Star_, ...uz], g)
    | Alt(gs) =>
      ListUtil.elem_splits(gs)
      |> List.map(((pre, g, suf)) => go([Alt_(pre, suf), ...uz], g))
      |> union
    | Seq(gs) =>
      ListUtil.elem_splits(gs)
      |> List.map(((pre, g, suf)) => go([Seq_(pre, suf), ...uz], g))
      |> union
    };
  go(Regex.Unzipped.empty, g);
};
let v: t =
  Sort.Map.to_seq(Grammar.v)
  |> Seq.concat_map(((s, pe)) =>
       List.to_seq(pe)
       |> Seq.mapi((p, lvl) => (p, lvl))
       |> Seq.concat_map(((p, (g, _))) => to_seq(of_regex(s, p, g)))
     )
  |> Label.Map.of_seq;

let of_ = token =>
  switch (find_opt(token, v)) {
  | None => []
  | Some(ms) => ms
  };
