open Util;

module Molds = {
  type t = Token.Map.t(list(Mold.t));

  let union2 = Token.Map.union((_, ms_l, ms_r) => Some(ms_l @ ms_r));
  let union = List.fold_left(union2, Token.Map.empty);

  let molds_of_grex = (s: Sort.t, p: Prec.t, g: Gram.t): t => {
    let rec go = (m: Mold.t, g: Gram.t) =>
      switch (g) {
      | Atom(Kid(_)) => Token.Map.empty
      | Atom(Tok(t)) => Token.Map.singleton(t, [m])
      | Star(g) => go(Mold.push(Star_, m), g)
      | Alt(gs) =>
        ListUtil.elem_splits(gs)
        |> List.map(((pre, g, suf)) =>
             go(Mold.push(Alt_(pre, suf), m), g)
           )
        |> union
      | Seq(gs) =>
        ListUtil.elem_splits(gs)
        |> List.map(((pre, g, suf)) =>
             go(Mold.push(Seq_(pre, suf), m), g)
           )
        |> union
      };
    go(Mold.init(s, p), g);
  };
  let molds: t =
    List.to_seq(Lang.t)
    |> Seq.concat_map(((s, prec_lvls)) =>
         List.to_seq(prec_lvls)
         |> Seq.mapi((p, lvl) => (p, lvl))
         |> Seq.concat_map(((p, (g, _))) =>
              Token.Map.to_seq(molds_of_grex(s, p, g))
            )
       )
    |> Token.Map.of_seq;

  let find = token =>
    switch (Token.Map.find_opt(token, molds)) {
    | None => []
    | Some(ms) => ms
    };
};

let molds = t => Molds.find(Token.shape(t));

let assoc = (s, p) => snd(List.nth(List.assoc(s, Lang.t), p));
