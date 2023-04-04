open Util;

include Token.Map;
type t = Token.Map.t(list(Mold.t));

let union2 = union((_, ms_l, ms_r) => Some(ms_l @ ms_r));
let union = List.fold_left(union2, empty);

let molds_of_gram = (s: Sort.t, p: Prec.t, g: Gram.t(Sort.t)): t => {
  let rec go = (m: Mold.t, g: Gram.t(Sort.o)) =>
    switch (g) {
    | Atom(Kid(_)) => empty
    | Atom(Tok(t)) => singleton(t, [m])
    | Star(g) => go(Mold.push(Star_, m), g)
    | Alt(gs) =>
      ListUtil.elem_splits(gs)
      |> List.map(((pre, g, suf)) => go(Mold.push(Alt_(pre, suf), m), g))
      |> union
    | Seq(gs) =>
      ListUtil.elem_splits(gs)
      |> List.map(((pre, g, suf)) => go(Mold.push(Seq_(pre, suf), m), g))
      |> union
    };
  g
  |> Gram.map_atoms(
       fun
       | Tok(_) as a => a
       | Kid(s) => Kid(Some(s)),
     )
  |> go(Mold.init(Some(s), p));
};
let t: t =
  List.to_seq(Lang.t)
  |> Seq.concat_map(((s, prec_lvls)) =>
       List.to_seq(prec_lvls)
       |> Seq.mapi((p, lvl) => (p, lvl))
       |> Seq.concat_map(((p, (g, _))) => to_seq(molds_of_gram(s, p, g)))
     )
  |> Token.Map.of_seq;

let of_token = token =>
  switch (find_opt(token, t)) {
  | None => []
  | Some(ms) => ms
  };
