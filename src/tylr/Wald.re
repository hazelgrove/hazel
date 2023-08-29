open Util;
include Chain;

// "walled" meld, wario to meld's mario
[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = Meld.wald('a);
[@deriving (show({with_path: false}), sexp, yojson)]
type m = t(Material.t(Mold.t));
[@deriving (show({with_path: false}), sexp, yojson)]
type p = t(Piece.t);

let singleton: _ => t = Chain.of_loop;
let mk = (mel: Meld.t): option((Meld.t, t, Meld.t)) =>
  Option.bind(Meld.distribute(mel).chain, Chain.trim);

let unmk = (~l=Meld.empty(), ~r=Meld.empty(), wal: t) =>
  Meld.of_chain(Chain.untrim(l, wal, r)) |> Meld.aggregate;

let join = (_, _, _) => failwith("todo Wald.join");

let sort = wal => Piece.sort(Chain.fst(wal));
let prec = wal => Piece.prec(Chain.fst(wal));

// precond: p eq wal
let link = (p, ~slot=Meld.empty(), wal) =>
  // Chain.link(p, Meld.patch(~l=p, kid), wal);
  Chain.link(p, kid, wal);
let knil = (wal, ~slot=Meld.empty(), p) =>
  // Chain.knil(wal, Meld.patch(kid, ~r=p), p);
  Chain.knil(wal, kid, p);

let append = (l: t, ~slot=Meld.empty(), r: t) =>
  l |> Chain.fold_right((p, kid) => link(p, ~slot), p => link(p, ~slot, r));

// let of_complement = (cmpl: Complement.t): option(t) =>
//   List.fold_right(
//     (proto, wal) => {
//       let t = Piece.of_tile(Tile.mk(proto));
//       switch (wal) {
//       | None => Some(of_piece(t))
//       | Some(wal) => Some(link(t, wal))
//       };
//     },
//     cmpl,
//     None,
//   );

let face = (side: Dir.t): (t => Piece.t) =>
  switch (side) {
  | L => Chain.fst
  | R => Chain.lst
  };

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
