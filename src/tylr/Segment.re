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

let zip_piece_l = (p_l, seg) =>
  switch (Chain.unlink(seg)) {
  | Some(([], mel, tl)) when Option.is_some(Meld.zip_piece_l(p_l, mel)) =>
    let mel = Option.get(Meld.zip_piece_l(p_l, mel));
    Some(Chain.link([], mel, tl));
  | _ => None
  };
let zip_piece_r = (seg, p_r) =>
  switch (Chain.unknil(seg)) {
  | Some((tl, mel, [])) when Option.is_some(Meld.zip_piece_r(mel, p_r)) =>
    let mel = Option.get(Meld.zip_piece_r(mel, p_r));
    Some(Chain.knil(tl, mel, []));
  | _ => None
  };

let cons_space = (s, seg) => Chain.map_fst((@)(s), seg);
let cons_meld = (mel, seg) => Chain.link(Space.empty, mel, seg);

let snoc_space = (seg, s) => Chain.map_lst(s' => s' @ s, seg);
let snoc_meld = (seg, mel) => Chain.knil(seg, mel, Space.empty);

let of_space = (s: Space.s): t => Chain.of_loop(s);
let of_meld = (mel: Meld.t): t => Chain.mk(Space.[empty, empty], [mel]);
let of_padded = ((mel, (l, r)): Meld.Padded.t): t =>
  Chain.mk([l, r], [mel]);

let rec meld_to_prefix = (mel: Meld.t): t =>
  switch (Chain.unknil(mel)) {
  | None =>
    switch (Chain.lst(mel)) {
    | None => empty
    | Some(K(kid)) => meld_to_prefix(kid)
    }
  | Some((tl, p, kid)) =>
    let (p, s) = Piece.pop_space_r(p);
    let kid_pre =
      switch (kid) {
      | None => empty
      | Some(K(kid)) => meld_to_prefix(kid)
      };
    cons_meld(Chain.knil(tl, p, None), cons_space(s, kid_pre));
  };
let to_prefix: t => t =
  Chain.fold_right(
    (s, mel, pre) => concat([of_space(s), meld_to_prefix(mel), pre]),
    of_space,
  );

let rec meld_to_suffix = (mel: Meld.t): t =>
  switch (Chain.unlink(mel)) {
  | None =>
    switch (Chain.fst(mel)) {
    | None => empty
    | Some(K(kid)) => meld_to_suffix(kid)
    }
  | Some((kid, p, tl)) =>
    let (s, p) = Piece.pop_space_l(p);
    let kid_suf =
      switch (kid) {
      | None => empty
      | Some(K(kid)) => meld_to_suffix(kid)
      };
    snoc_meld(snoc_space(kid_suf, s), Chain.link(None, p, tl));
  };
let to_suffix: t => t =
  Chain.fold_left(of_space, (suf, mel, s) =>
    concat([suf, meld_to_suffix(mel), of_space(s)])
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
      concat([of_padded(kid), of_meld(mel_g), meld_to_prefix(mel)]);
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
      concat([meld_to_suffix(mel), of_meld(mel_g), of_padded(kid)]);
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
    | S(s) => cons_space([s], seg)
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
    | S(s) => snoc_space(seg, [s])
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
         s => snoc_space(seg, s),
         (seg, mel, s) => snoc_space(snoc_meld(l, seg, mel), s),
       );
  let cat_r = (pre: t, seg: t, r: Bound.t): t =>
    pre
    |> Chain.fold_right(
         (s, mel, seg) => cons_space(s, cons_meld(mel, seg, r)),
         s => cons_space(s, seg),
       );

  let concat_l = (l: Bound.t, segs: list(t)): t =>
    List.fold_left((segs, seg) => cat_l(l, segs, seg), empty, segs);
  let concat_r = (segs: list(t), r: Bound.t): t =>
    List.fold_right((seg, segs) => cat_r(seg, segs, r), segs, empty);

  let bound_l = (l: Bound.t, seg: t) =>
    seg
    |> Chain.fold_left(
         s => of_space(s),
         (bounded, mel, s) => snoc_space(snoc_meld(l, bounded, mel), s),
       );
  let bound_r = (seg: t, r: Bound.t) =>
    seg
    |> Chain.fold_right(
         (s, mel, bounded) => cons_space(s, cons_meld(mel, bounded, r)),
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
        meld_to_suffix(tl_mel),
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
        meld_to_prefix(tl_mel),
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
           ((Chain.link(s, mel, lt), geq), sel);
         } else {
           switch (cons_meld(mel, sel)) {
           | In(_) => raise(Disconnected(0))
           | Lt(_) => ((Chain.link(s, mel, lt), geq), sel)
           | Eq(sel)
           | Gt(sel) => ((lt, Chain.link(s, mel, geq)), sel)
           };
         },
       s => ((empty, of_space(s)), cons_space(s, sel)),
     )
  |> fst;
let split_gt = (sel: t, suf: t): (t as '_leq, t as '_gt) =>
  suf
  |> Chain.fold_left(
       s => ((of_space(s), empty), snoc_space(sel, s)),
       (((leq, gt), sel), mel, s) =>
         if (has_meld(gt)) {
           ((leq, Chain.knil(gt, mel, s)), sel);
         } else {
           switch (snoc_meld(sel, mel)) {
           | In(_) => raise(Disconnected(1))
           | Lt(sel)
           | Eq(sel) => ((Chain.knil(leq, mel, s), gt), sel)
           | Gt(_) => ((leq, Chain.knil(gt, mel, s)), sel)
           };
         },
     )
  |> fst;

let assemble_l = (~l: option(Meld.t)=?, seg: t): t =>
  seg
  |> Chain.fold_left(
       s => of_space(s),
       (seg, mel, s) =>
         switch (hsup_meld(seg, mel)) {
         | In(seg)
         | Lt(seg)
         | Eq(seg) => snoc_space(seg, s)
         | Gt(kid) =>
           let l_cmp_c =
             switch (l) {
             | None => Cmp.Lt()
             | Some(l) => Meld.cmp(l, mel)
             };
           switch (l_cmp_c) {
           | In () => raise(Disconnected(2))
           | Lt ()
           | Eq () => of_padded(Meld.(merge(kid, Padded.mk(~r=s, mel))))
           | Gt () =>
             concat([of_padded(kid), of_padded(Meld.Padded.mk(~r=s, mel))])
           };
         },
     );
let assemble_r = (~r: option(Meld.t)=?, seg: t): t =>
  seg
  |> Chain.fold_right(
       (s, mel, seg) =>
         switch (push_meld(mel, seg)) {
         | In(seg) => cons_space(s, seg)
         | Lt(kid) =>
           let c_cmp_r =
             switch (r) {
             | None => Cmp.Gt()
             | Some(r) => Meld.cmp(mel, r)
             };
           switch (c_cmp_r) {
           | In () => raise(Disconnected(3))
           | Lt () =>
             concat([of_padded(Meld.Padded.mk(~l=s, mel)), of_padded(kid)])
           | Eq ()
           | Gt () => of_padded(Meld.(merge(Padded.mk(~l=s, mel), kid)))
           };
         | Eq(seg)
         | Gt(seg) => cons_space(s, seg)
         },
       s => of_space(s),
     );
// todo: rename this meld
let assemble = (~l: option(Meld.t)=?, ~r: option(Meld.t)=?, seg: t): t =>
  seg |> assemble_r(~r?) |> assemble_l(~l?);

let to_padded =
  fun
  | ([s], []) => Some(Meld.Padded.empty(~r=s, ()))
  | ([l, r], [mel]) => Some(Meld.Padded.mk(~l, ~r, mel))
  | _ => None;

// precond: in prefix form
// todo: rework using aba interface or possibly reformulate
// overall using parent merging
let finish_prefix = (pre: t): Meld.Padded.t =>
  pre
  |> Chain.fold_right(
       (s, mel, kid) => Meld.(merge(Padded.mk(~l=s, mel), kid)),
       s => Meld.(Padded.mk(~l=s, Meld.empty)),
     );
