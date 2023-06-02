open Util;

// "walled" meld, wario to meld's mario
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Chain.t(Piece.t, Meld.t);

let of_piece: _ => t = Chain.of_loop;
let mk = (mel: Meld.t): option((Meld.t, t, Meld.t)) =>
  Option.bind(Meld.distribute(mel).chain, Chain.trim);

let unmk = (~l=Meld.empty(), ~r=Meld.empty(), wal: t) =>
  Meld.of_chain(Chain.untrim(l, wal, r)) |> Meld.aggregate;

let join = (_, _, _) => failwith("todo Wald.join");

let sort = wal => Piece.sort(Chain.fst(wal));
let prec = wal => Piece.prec(Chain.fst(wal));

// precond: p eq wal
let link = (p, ~kid=Meld.empty(), wal) =>
  Chain.link(p, Meld.patch(~l=p, kid), wal);
let knil = (wal, ~kid=Meld.empty(), p) =>
  Chain.knil(wal, Meld.patch(kid, ~r=p), p);

let append = (l: t, ~kid=Meld.empty(), r: t) =>
  l |> Chain.fold_right((p, kid) => link(p, ~kid), p => link(p, ~kid, r));

let of_complement = (cmpl: Complement.t): option(t) =>
  List.fold_right(
    (proto, wal) => {
      let t = Piece.of_tile(Tile.mk(proto));
      switch (wal) {
      | None => Some(of_piece(t))
      | Some(wal) => Some(link(t, wal))
      };
    },
    cmpl,
    None,
  );

let face = (side: Dir.t): (t => _) =>
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

let fuses = (~kid) =>
  cat((l, r) => {
    open OptUtil.Syntax;
    let* s = Meld.is_porous(kid);
    let+ p = Piece.fuse(l, r);
    Piece.(proto(p) == proto(l))
      ? Padded.mk(of_piece(p), ~r=s) : Padded.mk(~l=s, of_piece(p));
  });

let matches = (~kid) =>
  cat((l, r) => {
    open OptUtil.Syntax;
    let+ cmpl = Piece.matches(l, r);
    let cmpl_r =
      switch (of_complement(cmpl)) {
      | Some(wal) => knil(wal, r)
      | None => of_piece(r)
      };
    Padded.mk(link(l, ~kid, cmpl_r));
  });

let rec eq = (l: t, ~kid=Meld.empty(), r: t): option(Padded.t) => {
  open OptUtil.Syntax;
  let/ () = fuses(l, ~kid, r);
  let/ () = matches(l, ~kid, r);
  let* (tl_l, kid_l, p_l) = Chain.unknil(l);
  let* (p_r, kid_r, tl_r) = Chain.unlink(r);
  switch (p_l.shape, Meld.is_porous(kid), p_r.shape) {
  | (G(_), Some(s), _) => eq(tl_l, ~kid=Meld.pad(kid_l, ~r=s), r)
  | (_, Some(s), G(_)) => eq(l, ~kid=Meld.pad(~l=s, kid_r), tl_r)
  | _ => None
  };
};

// checks whether r wins custody battle against l over kid.
// if so, returns complemented r and left unikid (possibly empty).
let lt = (l: t, ~kid=Meld.empty(), r: t): option((Meld.t, t)) => {
  open OptUtil.Syntax;
  let+ cmpl = Piece.lt(face(R, l), face(L, r));
  let (kid, cmpl_r) =
    switch (of_complement(cmpl)) {
    | None => (kid, r)
    | Some(wal) => (Meld.empty(), append(wal, ~kid, r))
    };
  (Meld.patch(kid, ~r=face(L, cmpl_r)), cmpl_r);
};

let gt = (l: t, ~kid=Meld.empty(), r: t): option((t, Meld.t)) => {
  open OptUtil.Syntax;
  let+ cmpl = Piece.gt(face(R, l), face(L, r));
  let (l_cmpl, kid) =
    switch (of_complement(cmpl)) {
    | None => (l, kid)
    | Some(wal) => (append(l, ~kid, wal), Meld.empty())
    };
  (l_cmpl, Meld.patch(~l=face(R, l_cmpl), kid));
};
