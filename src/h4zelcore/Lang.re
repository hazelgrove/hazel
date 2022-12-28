type t = Sort.Map.t(PGram.t);

let hazel: t =
  [
    (Typ, failwith("todo typ")),
    (Pat, failwith("todo pat")),
    (Exp, failwith("todo exp")),
  ]
  |> List.to_seq
  |> Sort.Map.of_seq;

module Molds = {
  type l = t;
  type t = Token.Map.t(list(Mold.t));

  let add = (t: Token.t, m: Mold.t, ms: t) =>
    Token.Map.update(
      t,
      fun
      | None => Some([m])
      | Some(ms) => Some([m, ...ms]),
    );

  let union = Token.Map.union((t, ms_l, ms_r) => Some(ms_l @ ms_r));

  let mk_sort = (s: Sort.t, g: Gram.t): t => {
    let rec go = (m: Mold.t, g: Gram.t) =>
      switch (g) {
      | Bot
      | Eps
      | Kid(_) => Token.Map.empty
      | Tok(t) => Token.Map.singleton(t, m)
      | Prec(p, g) => go(Mold.push(Prec(p), m), g)
      | Alt(l, r) =>
        union(
          go(Mold.push(AltL(r), m), l),
          go(Mold.push(AltR(l), m), r),
        )
      | Seq(l, r) =>
        union(
          go(Mold.push(SeqL(r), m), l),
          go(Mold.push(SeqR(l), m), r),
        )
      };
    go(Mold.init(s), g);
  };

  let mk = (lang: l): t =>
    Sort.Map.bindings(lang)
    |> List.map(((s, g)) => mk_sort(s, g))
    |> List.fold_left(union, Token.Map.empty);
};
