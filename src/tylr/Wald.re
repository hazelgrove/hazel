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

let eq =
    (l: Piece.t, ~kid=Meld.empty(), r: Piece.t)
    : option((Space.t, t, Space.t)) => {
  let ret = (~l=Space.empty, ~r=Space.empty, wal) => Some((l, wal, r));
  let zips = Piece.zip(l, r);
  let eq_mold = Piece.(mold(l) == mold(r));
  switch (l.shape, Meld.is_porous(kid), r.shape) {
  | (_, Some(s), _) when Option.is_some(zips) && Space.is_empty(s) =>
    ret(of_piece(Option.get(zips)))
  | (G(_), Some(s), _) when eq_mold => ret(~l=s, of_piece(r))
  | (_, Some(s), G(_)) when eq_mold => ret(of_piece(l), ~r=s)
  | _ => Option.bind(of_range(l, ~kid, r), ret)
  };
};
