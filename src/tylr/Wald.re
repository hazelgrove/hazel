open Util;

// a WALleD meld
// the wario to the meld's mario
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Chain.t(Piece.t, Meld.t);

let of_piece: _ => t = Chain.of_loop;
let mk = (mel: Meld.t): option((Meld.t, t, Meld.t)) =>
  Option.bind(Meld.distribute(mel).chain, Chain.trim);

let unmk = (~l=Meld.empty(), ~r=Meld.empty(), wal: t) =>
  Meld.of_chain(Chain.untrim(l, wal, r)) |> Meld.aggregate;

let join = (_, _, _) => failwith("todo Wald.join");

let of_range = (l: Piece.t, ~kid=Meld.empty(), r: Piece.t) =>
  Piece.(Mold.eq(mold(l), mold(r)))
  |> Option.map(compl =>
       of_piece(r)
       |> List.fold_right(
            ((sugg, mold), r) =>
              switch (Mold.tip(R, mold)) {
              | Convex => raise(Gram.Ill_typed)
              | Concave(s, _) =>
                let kid = Meld.of_grout(Grout.mk_convex(s));
                let g = Piece.of_grout(Grout.mk(~sugg, mold));
                Chain.link(g, kid, r);
              },
            compl,
          )
     )
  |> Option.map(Chain.link(l, kid));

module Padded = {
  type wald = t;
  type t = (Space.t, wald, Space.t);
  let mk = (~l=Space.empty, ~r=Space.empty, wal) => (l, wal, r);
  let link = (p, kid, (l, wal, r): t) =>
    mk(Chain.link(p, Meld.pad(kid, ~r=l), wal), ~r);
};

let cat =
    (f: (Piece.t, Piece.t) => option(Padded.t), l: t, r: t)
    : option(Padded.t) =>
  l
  |> Chain.fold_right(
       (p, kid) => Option.map(Padded.link(p, kid)),
       p_l =>
         switch (Chain.unlink(r)) {
         | None => f(p_l, Chain.fst(r))
         | Some((p_r, kid_r, tl_r)) =>
           f(p_l, p_r)
           |> Option.map(((l, wal, r)) =>
                wal
                |> Chain.fold_right(Chain.link, p =>
                     Chain.link(p, Meld.pad(~l=r, kid_r), tl_r)
                   )
                |> Padded.mk(~l)
              )
         },
     );

let rec eq = (l: t, ~kid=Meld.empty(), r: t): option(Padded.t) => {
  open OptUtil.Syntax;
  let eq_p = (l, r) => {
    let zips = Piece.zip(l, r);
    let eq_mold = Piece.(mold(l) == mold(r));
    switch (l.shape, Meld.is_porous(kid), r.shape) {
    | (_, Some(s), _) when Option.is_some(zips) && Space.is_empty(s) =>
      return(Padded.mk(of_piece(Option.get(zips))))
    | (G(_), Some(s), _) when eq_mold =>
      return(Padded.mk(~l=s, of_piece(r)))
    | (_, Some(s), G(_)) when eq_mold =>
      return(Padded.mk(of_piece(l), ~r=s))
    | _ =>
      let+ wal = of_range(l, ~kid, r);
      Padded.mk(wal);
    };
  };
  let/ () = cat(eq_p, l, r);
  let* (tl_l, kid_l, p_l) = Chain.unknil(l);
  let* (p_r, kid_r, tl_r) = Chain.unlink(r);
  switch (p_l.shape, Meld.is_porous(kid), p_r.shape) {
  | (G(_), Some(s), _) => eq(tl_l, ~kid=Meld.pad(kid_l, ~r=s), r)
  | (_, Some(s), G(_)) => eq(l, ~kid=Meld.pad(~l=s, kid_r), tl_r)
  | _ => None
  };
};
