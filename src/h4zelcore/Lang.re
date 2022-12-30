type t = Sort.Map.t(PGram.t);

module Assoc = {
  type lang = t;
  type t = Sort.Map.t(Prec.Map.t(Assoc.t));

  let mk = (l: lang): t =>
    l
    |> Sort.Map.map(pg =>
         PGram.to_seq(pg)
         |> Seq.map(((i, (_, a))) => (i, a))
         |> Prec.Map.of_seq
       );
};

module Molds = {
  type lang = t;
  type t = Token.Map.t(list(Mold.t));

  let add = (t: Token.t, m: Mold.t, ms: t) =>
    Token.Map.update(
      t,
      fun
      | None => Some([m])
      | Some(ms) => Some([m, ...ms]),
    );

  let union2 = Token.Map.union((t, ms_l, ms_r) => Some(ms_l @ ms_r));
  let union = List.fold_left(union2, Token.Map.empty);

  let mk_g = (s: Sort.t, p: Prec.t, g: Gram.t): t => {
    let rec go = (m: Mold.t, g: Gram.t) =>
      switch (g) {
      | Atom(Kid(_)) => Token.Map.empty
      | Atom(Tok(t)) => Token.Map.singleton(t, m)
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

  let mk = (l: lang): t =>
    Sort.Map.to_seq(l)
    |> Seq.concat_map(((s, pg)) =>
         PGram.to_seq(pg)
         |> Seq.concat_map((p, (g, _)) => Token.Map.to_seq(mk_g(s, p, g)))
       )
    |> Token.Map.of_seq;
};

let hazel: t =
  [
    (Typ, failwith("todo typ")),
    (Pat, failwith("todo pat")),
    (Exp, [(Seq([Kid(Exp), Tok("+"), Kid(Exp)]), Some(L))]),
  ]
  |> List.to_seq
  |> Sort.Map.of_seq;
