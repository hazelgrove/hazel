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
// switch (cmpl) {
// | [] => (wal, kid)
// | [hd, ...tl] =>
//   let kid = failwith("fix sort consistency using hd right tip");
//   let g = Piece.of_grout(Grout.mk(~sugg=fst(hd), snd(hd)));
//   List.fold_left(
//     (wal, (sugg, mold)) => knil(wal, Piece.of_grout(Grout.mk(~sugg, mold))),
//     knil(wal, ~kid, g),
//     tl,
//   );
// };

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

let of_complement = (cmpl: list((Token.t, Mold.t))): option(t) =>
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

// let of_range = (l: Piece.t, ~kid=Meld.empty(), r: Piece.t) =>
//   Piece.complement_between(l, r)
//   |> Option.map(compl =>
//        of_piece(r)
//        |> List.fold_right(
//             ((sugg, mold)) => link(Piece.of_grout(Grout.mk(~sugg, mold))),
//             compl,
//           )
//      )
//   |> Option.map(Chain.link(l, kid));

let face = (side: Dir.t): (t => _) =>
  switch (side) {
  | L => Chain.fst
  | R => Chain.lst
  };

let complement_between = (l: t, ~kid=Meld.empty(), r: t): option(t) =>
  Piece.complement_between(face(R, l), face(L, r))
  |> Option.map(compl =>
       List.fold_right(
         ((sugg, mold), r) =>
           link(Piece.of_grout(Grout.mk(~sugg, mold)), r),
         compl,
         r,
       )
     )
  |> Option.map(Chain.append(l, kid));

let apply_complement_m = (l: t, kid, cmpl, r: t): option(t) =>
  r
  |> List.fold_right(
       ((sugg, mold), r) =>
         link(Piece.of_grout(Grout.mk(~sugg, mold)), r),
       compl,
     )
  |> Chain.append(l, kid);

let apply_complement_l = (cmpl, kid, wal: t): (Meld.t, t) =>
  switch (cmpl) {
  | None => (kid, wal)
  | Some((tl, hd)) =>
    let kid = failwith("fix sort consistency using hd right tip");
    let g = Piece.of_grout(Grout.mk(~sugg=fst(hd), snd(hd)));
    link(g, ~kid, wal)
    |> List.fold_right(
         ((sugg, mold)) => link(Piece.of_grout(Grout.mk(~sugg, mold))),
         tl,
       );
  };
let apply_complement_r =
    (wal: t, kid: Meld.t, cmpl: list((Token.t, Mold.t))): (t, Meld.t) =>
  switch (cmpl) {
  | [] => (wal, kid)
  | [hd, ...tl] =>
    let kid = failwith("fix sort consistency using hd right tip");
    let g = Piece.of_grout(Grout.mk(~sugg=fst(hd), snd(hd)));
    List.fold_left(
      (wal, (sugg, mold)) =>
        knil(wal, Piece.of_grout(Grout.mk(~sugg, mold))),
      knil(wal, ~kid, g),
      tl,
    );
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
