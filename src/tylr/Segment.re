open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Chain.t(Space.s, Meld.t);

// when input meld structure (specifically parent-kid relations)
// must be broken to give proper assembly
exception Nonmonotonic;

// ill-fitting tips
exception Disconnected(int);

let empty = ([Space.empty], []);
let is_empty: t => bool = (==)(empty);

let melds: t => list(Meld.t) = Chain.links;
let has_meld = seg => Option.is_some(Chain.unlink(seg));

let cat: (t, t) => t = Chain.cat((@));
let concat = (segs: list(t)): t => List.fold_right(cat, segs, empty);

let link = (~s=Space.empty, ~mel=?, seg) =>
  switch (mel) {
  | None => Chain.map_fst((@)(s), seg)
  | Some(mel) => Chain.link(s, mel, seg)
  };
let knil = (seg, ~mel=?, ~s=Space.empty, ()) =>
  switch (mel) {
  | None => Chain.map_lst(Fun.flip((@), s), seg)
  | Some(mel) => Chain.knil(seg, mel, s)
  };

let of_space = (s: Space.s): t => Chain.of_loop(s);
let of_meld = (mel: Meld.t): t => Chain.mk(Space.[empty, empty], [mel]);
let of_padded = ((mel, (l, r)): Meld.Padded.t): t =>
  Chain.mk([l, r], [mel]);
let to_padded =
  fun
  | ([s], []) => Some(Meld.Padded.empty(~r=s, ()))
  | ([l, r], [mel]) => Some(Meld.Padded.mk(~l, ~r, mel))
  | _ => None;

let zip_piece_l = (p_l, seg) =>
  switch (Chain.unlink(seg)) {
  | Some(([], mel, tl)) when Option.is_some(Meld.zip_piece_l(p_l, mel)) =>
    let mel = Option.get(Meld.zip_piece_l(p_l, mel));
    Some(link(~mel, tl));
  | _ => None
  };
let zip_piece_r = (seg, p_r) =>
  switch (Chain.unknil(seg)) {
  | Some((tl, mel, [])) when Option.is_some(Meld.zip_piece_r(mel, p_r)) =>
    let mel = Option.get(Meld.zip_piece_r(mel, p_r));
    Some(knil(tl, ~mel, ()));
  | _ => None
  };

let to_prefix: t => t =
  Chain.fold_right(
    (s, mel, pre) => concat([of_space(s), Meld.to_prefix(mel), pre]),
    of_space,
  );
let to_suffix: t => t =
  Chain.fold_left(of_space, (suf, mel, s) =>
    concat([suf, Meld.to_suffix(mel), of_space(s)])
  );

let split_nth_space = (n, seg): (t, Space.s, t) => {
  let (ss, ms) = seg;
  let (ss_l, s, ss_r) = ListUtil.split_nth(n, ss);
  let (ms_l, ms_r) = ListUtil.split_n(n, ms);
  (
    Chain.mk(ss_l @ [Space.empty], ms_l),
    s,
    Chain.mk([Space.empty, ...ss_r], ms_r),
  );
};

let rec mold =
        (~match: bool, pre: t, ~kid: option(Sort.o)=?, t: Token.t)
        : Mold.Result.t =>
  switch (Chain.unknil(pre)) {
  | None => Error(kid)
  | Some((pre, mel, _)) =>
    open Result.Syntax;
    let/ kid = Meld.mold(mel, ~kid?, t);
    mold(~match, pre, ~kid?, t);
  };

let rec cons_meld =
        (mel: Meld.t, ~kid=Meld.Padded.empty(), seg: t)
        : Cmp.lt_else(Meld.Padded.t, t) =>
  switch (Chain.unlink(seg)) {
  | None => Lt(Meld.Padded.pad(~r=Chain.fst(seg), kid))
  | Some((s, hd, tl)) =>
    switch (Meld.(cmp_merge(mel, ~kid=Padded.pad(~r=s, kid), hd))) {
    | In(mel_hd) => In(cat(of_padded(mel_hd), tl))
    | Eq(mel_hd) => Eq(cat(of_padded(mel_hd), tl))
    | Gt(mel_kid) => Gt(concat([of_padded(mel_kid), of_meld(hd), tl]))
    | Lt(kid_hd) => cons_meld(mel, ~kid=kid_hd, tl)
    }
  };
let rec snoc_meld =
        (seg: t, ~kid=Meld.Padded.empty(), mel: Meld.t)
        : Cmp.gt_else(Meld.Padded.t, t) =>
  switch (Chain.unknil(seg)) {
  | None => Gt(Meld.Padded.pad(~l=Chain.lst(seg), kid))
  | Some((tl, hd, s)) =>
    switch (Meld.(cmp_merge(hd, ~kid=Padded.pad(~l=s, kid), mel))) {
    | In(hd_mel) => In(cat(tl, of_padded(hd_mel)))
    | Eq(hd_mel) => Eq(cat(tl, of_padded(hd_mel)))
    | Lt(kid_mel) => Lt(concat([tl, of_meld(hd), of_padded(kid_mel)]))
    | Gt(hd_kid) => snoc_meld(tl, ~kid=hd_kid, mel)
    }
  };

module Bound = {
  // None represents program root
  type t = option(Meld.t);

  let cmp_l = (l: t, mel: Meld.t) =>
    switch (l) {
    | None => Cmp.Lt(Sort.Ana.mk(~strict=true, ~sort=Sort.root, ()))
    | Some(l) => Meld.cmp(l, mel)
    };
  let merge_l = (l: t, ~kid=Meld.Padded.empty(), mel: Meld.t) =>
    switch (cmp_l(l, mel)) {
    | In((sort, prec)) =>
      assert(Option.is_some(Meld.Padded.is_empty(kid)));
      let mel_g = Meld.of_grout(Grout.mk_concave(sort, prec));
      concat([of_padded(kid), of_meld(mel_g), Meld.to_prefix(mel)]);
    | Gt(_)
    | Eq(_) => cat(of_padded(kid), of_meld(mel))
    | Lt(_) =>
      let kid_mel = Meld.(merge(kid, Padded.mk(mel)));
      to_prefix(of_padded(kid_mel));
    };

  let cmp_r = (mel: Meld.t, r: t) =>
    switch (r) {
    | None => Cmp.Gt(Sort.Ana.mk(~strict=true, ~sort=Sort.root, ()))
    | Some(r) => Meld.cmp(mel, r)
    };
  let merge_r = (mel: Meld.t, ~kid=Meld.Padded.empty(), r: t) =>
    switch (cmp_r(mel, r)) {
    | In((sort, prec)) =>
      assert(Option.is_some(Meld.Padded.is_empty(kid)));
      let mel_g = Meld.of_grout(Grout.mk_concave(sort, prec));
      concat([Meld.to_suffix(mel), of_meld(mel_g), of_padded(kid)]);
    | Lt(_)
    | Eq(_) => cat(of_meld(mel), of_padded(kid))
    | Gt(_) =>
      let mel_kid = Meld.(merge(Padded.mk(mel), kid));
      to_suffix(of_padded(mel_kid));
    };
};

module Bounded = {
  let cons_meld = (mel: Meld.t, seg: t, r: Bound.t): t =>
    switch (cons_meld(mel, seg)) {
    | In(seg)
    | Eq(seg)
    | Gt(seg) => seg
    | Lt(kid) => Bound.merge_r(mel, ~kid, r)
    };
  let cons_lexeme = (lx: Lexeme.t, seg: t, r): t =>
    switch (lx) {
    | S(s) => link(~s=[s], seg)
    | T(_)
    | G(_) =>
      let p = Option.get(Lexeme.to_piece(lx));
      switch (zip_piece_l(p, seg)) {
      | Some(seg) => seg
      | None => cons_meld(Meld.of_piece(p), seg, r)
      };
    };

  let snoc_meld = (l: Bound.t, seg: t, mel: Meld.t): t =>
    switch (snoc_meld(seg, mel)) {
    | In(seg)
    | Eq(seg)
    | Lt(seg) => seg
    | Gt(kid) => Bound.merge_l(l, ~kid, mel)
    };
  let snoc_lexeme = (l, seg, lx: Lexeme.t) =>
    switch (lx) {
    | S(s) => knil(seg, ~s=[s], ())
    | T(_)
    | G(_) =>
      let p = Option.get(Lexeme.to_piece(lx));
      switch (zip_piece_r(seg, p)) {
      | Some(seg) => seg
      | None => snoc_meld(l, seg, Meld.of_piece(p))
      };
    };

  let cat_l = (l: Bound.t, seg: t, suf: t): t =>
    suf
    |> Chain.fold_left(
         s => knil(seg, ~s, ()),
         (seg, mel, s) => knil(snoc_meld(l, seg, mel), ~s, ()),
       );
  let cat_r = (pre: t, seg: t, r: Bound.t): t =>
    pre
    |> Chain.fold_right(
         (s, mel, seg) => link(~s, cons_meld(mel, seg, r)),
         s => link(~s, seg),
       );

  let concat_l = (l: Bound.t, segs: list(t)): t =>
    List.fold_left((segs, seg) => cat_l(l, segs, seg), empty, segs);
  let concat_r = (segs: list(t), r: Bound.t): t =>
    List.fold_right((seg, segs) => cat_r(seg, segs, r), segs, empty);

  let bound_l = (l: Bound.t, seg: t) =>
    seg
    |> Chain.fold_left(
         s => of_space(s),
         (bounded, mel, s) => knil(snoc_meld(l, bounded, mel), ~s, ()),
       );
  let bound_r = (seg: t, r: Bound.t) =>
    seg
    |> Chain.fold_right(
         (s, mel, bounded) => link(~s, cons_meld(mel, bounded, r)),
         s => of_space(s),
       );
};

// operations on melds that produce segments
module Meld_ = {
  // precond: mel is left-closed
  let uncons = (~from_piece, mel) => {
    let (p, tl_mel) =
      Meld.is_closed_l(mel)
      |> OptUtil.get_or_raise(Invalid_argument("Segment.Meld_.uncons"));
    let (hd, tl_p) = from_piece(p);
    let tl =
      List.fold_right(
        (lx, tl) => Bounded.cons_lexeme(lx, tl, None),
        tl_p,
        Meld.to_suffix(tl_mel),
      );
    (hd, tl);
  };
  let uncons_lexeme =
    uncons(~from_piece=p => ListUtil.split_first(Lexeme.s_of_piece(p)));
  let uncons_char =
    uncons(~from_piece=p =>
      Lexeme.(uncons_char_s(s_of_piece(p)))
      |> OptUtil.get_or_fail("Lexeme.s_of_piece returns nonempty")
    );

  // precond: mel is right-closed
  let unsnoc = (~from_piece, mel) => {
    let (tl_mel, p) =
      Meld.is_closed_r(mel)
      |> OptUtil.get_or_raise(Invalid_argument("Segment.Meld_.unsnoc"));
    let (tl_p, hd) = from_piece(p);
    let tl =
      List.fold_left(
        (tl, lx) => Bounded.snoc_lexeme(None, tl, lx),
        Meld.to_prefix(tl_mel),
        tl_p,
      );
    (tl, hd);
  };
  let unsnoc_lexeme =
    unsnoc(~from_piece=p => ListUtil.split_last(Lexeme.s_of_piece(p)));
  let unsnoc_char =
    unsnoc(~from_piece=p =>
      Lexeme.(unsnoc_char_s(s_of_piece(p)))
      |> OptUtil.get_or_fail("Lexeme.s_of_piece returns nonempty")
    );
};

let uncons = (~from_space, ~from_meld, seg) =>
  switch (from_space(Chain.fst(seg))) {
  | Some((a, s)) => Some((a, Chain.put_fst(s, seg)))
  | None =>
    open OptUtil.Syntax;
    let+ (_, mel, seg) = Chain.unlink(seg);
    let (a, tl) = from_meld(mel);
    (a, cat(tl, seg));
  };
let uncons_from_space =
  fun
  | [] => None
  | [s, ...ss] => Some((Lexeme.S(s), ss));

let uncons_lexeme =
  uncons(~from_space=uncons_from_space, ~from_meld=Meld_.uncons_lexeme);
let uncons_char =
  uncons(~from_space=uncons_from_space, ~from_meld=Meld_.uncons_char);

// precond: input seg is prefix
// postcond: output seg is prefix
let unsnoc = (~from_space, ~from_meld, seg: t) =>
  switch (from_space(Chain.lst(seg))) {
  | Some((s, a)) => Some((Chain.put_lst(s, seg), a))
  | None =>
    open OptUtil.Syntax;
    let+ (seg, mel, _) = Chain.unknil(seg);
    let (tl, a) = from_meld(mel);
    (cat(seg, tl), a);
  };
let unsnoc_from_space = ss =>
  ListUtil.split_last_opt(ss)
  |> Option.map(((ss, s)) => (ss, Lexeme.S(s)));
let unsnoc_lexeme =
  unsnoc(~from_space=unsnoc_from_space, ~from_meld=Meld_.unsnoc_lexeme);
let unsnoc_char =
  unsnoc(~from_space=unsnoc_from_space, ~from_meld=Meld_.unsnoc_char);

let split_lt = (pre: t, sel: t): (t as '_lt, t as '_geq) =>
  pre
  |> Chain.fold_right(
       (s, mel, ((lt, geq), sel)) =>
         if (has_meld(lt)) {
           ((link(~s, ~mel, lt), geq), sel);
         } else {
           switch (cons_meld(mel, sel)) {
           | In(_) => raise(Disconnected(0))
           | Lt(_) => ((link(~s, ~mel, lt), geq), sel)
           | Eq(sel)
           | Gt(sel) => ((lt, link(~s, ~mel, geq)), sel)
           };
         },
       s => ((empty, of_space(s)), link(~s, sel)),
     )
  |> fst;
let split_gt = (sel: t, suf: t): (t as '_leq, t as '_gt) =>
  suf
  |> Chain.fold_left(
       s => ((of_space(s), empty), knil(sel, ~s, ())),
       (((leq, gt), sel), mel, s) =>
         if (has_meld(gt)) {
           ((leq, knil(gt, ~mel, ~s, ())), sel);
         } else {
           switch (snoc_meld(sel, mel)) {
           | In(_) => raise(Disconnected(1))
           | Lt(sel)
           | Eq(sel) => ((knil(leq, ~mel, ~s, ()), gt), sel)
           | Gt(_) => ((leq, knil(gt, ~mel, ~s, ())), sel)
           };
         },
     )
  |> fst;

// module Lines = {
//   type seg = t;
//   type newline = Space.t;
//   type t = Chain.t(seg, newline);

//   let of_space = (s: Space.s) =>
//     Space.split_newlines(s) |> Chain.map(of_space, Fun.id);
//   let cons_meld = (mel, lines) => Chain.map_fst(cons_meld(mel), lines);
//   let cat = Chain.cat(cat);
// };

// let lines = (seg: t): Lines.t =>
//   seg
//   |> Chain.fold_right(
//        (s, mel, lines) =>
//          lines |> Lines.cons_meld(mel) |> Lines.(cat(of_space(s))),
//        Lines.of_space,
//      );

let complement = (~side: Dir.t, seg) =>
  // magically this works for both sides
  melds(seg)
  |> List.fold_left((compl, mel) => Meld.complement(~side, mel) @ compl, []);
