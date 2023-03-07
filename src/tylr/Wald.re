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

// precond: p eq wal
let link = (p, ~kid=Meld.empty(), wal) =>
  // todo: remove tip check once grout-sort-as-upper-bound
  // is fully implemented
  switch (Mold.tip(R, Piece.mold(p))) {
  | Convex => raise(Invalid_argument("Wald.link"))
  | Concave(s, _) =>
    let kid = Meld.is_empty(kid) ? Meld.of_grout(Grout.mk_convex(s)) : kid;
    Chain.link(p, kid, wal);
  };

let append = (l: t, ~kid=Meld.empty(), r: t) =>
  l |> Chain.fold_right((p, kid) => link(p, ~kid), p => link(p, ~kid, r));

let of_complement = (cmpl: Complement.t): option(t) =>
  List.fold_right(
    ((sugg, mold), wal) => {
      let g = Piece.of_grout(Grout.mk(~sugg, mold));
      switch (wal) {
      | None => of_piece(g)
      | Some(wal) => link(g, wal)
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
    switch (Piece.replaces(l, r)) {
    | Some(L) => return(Padded.mk(~l=s, of_piece(r)))
    | Some(R) => return(Padded.mk(of_piece(l), ~r=s))
    | None =>
      let+ p = Piece.zips(l, r);
      assert(Space.is_empty(s));
      Padded.mk(of_piece(p));
    };
  });

let matches = (~kid) =>
  cat((l, r) => {
    open OptUtil.Syntax;
    let+ cmpl = Piece.matches(l, r);
    let wal = of_complement(cmpl);
    Padded.mk(link(l, ~kid, knil(wal, r)));
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

let lt = (l: t, ~kid=Meld.empty(), r: t): option((Meld.t, t)) => {
  let (p_l, p_r) = (face(R, l), face(L, r));
  let patch = Meld.patch(Piece.tip(R, p_l));
  Piece.lt(p_l, p_r)
  |> Option.map(cmpl =>
       switch (of_complement(cmpl)) {
       | None => (patch(kid, Piece.tip(L, p_r)), r)
       | Some(wal) =>
         let end_kid = patch(Meld.empty(), Piece.tip(L, face(L, wal)));
         (end_kid, append(wal, ~kid, r));
       }
     );
};

let gt = (l: t, ~kid=Meld.empty(), r: t): option((t, Meld.t)) => {
  let (p_l, p_r) = (face(R, l), face(L, r));
  let patch = (l, kid) => Meld.patch(l, kid, Piece.tip(L, p_r));
  Piece.gt(p_l, p_r)
  |> Option.map(cmpl =>
       switch (of_complement(cmpl)) {
       | None => (l, patch(Piece.tip(R, p_l), kid))
       | Some(wal) =>
         let end_kid = patch(Piece.tip(R, face(R, wal)), Meld.empty());
         (append(l, ~kid, wal), end_kid);
       }
     );
};
