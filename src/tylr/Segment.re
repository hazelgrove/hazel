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
let cons_lexeme = (lx: Lexeme.t, seg: t): t =>
  switch (lx) {
  | S(s) => cons_space([s], seg)
  | T(_)
  | G(_) =>
    let p = Option.get(Lexeme.to_piece(lx));
    switch (zip_piece_l(p, seg)) {
    | Some(seg) => seg
    | None => Chain.link(Space.empty, Meld.of_piece(p), seg)
    };
  };

let snoc_space = (seg, s) => Chain.map_lst(s' => s' @ s, seg);
let snoc_meld = (seg, mel) => Chain.knil(seg, mel, Space.empty);
let snoc_lexeme = (seg, lx: Lexeme.t) =>
  switch (lx) {
  | S(s) => snoc_space(seg, [s])
  | T(_)
  | G(_) =>
    let p = Option.get(Lexeme.to_piece(lx));
    switch (zip_piece_r(seg, p)) {
    | Some(seg) => seg
    | None => Chain.knil(seg, Meld.of_piece(p), Space.empty)
    };
  };

let of_space = (s: Space.s): t => Chain.of_loop(s);
let of_meld = (mel: Meld.t): t => Chain.mk(Space.[empty, empty], [mel]);
let of_padded = ((mel, (l, r)): Meld.Padded.t): t =>
  Chain.mk([l, r], [mel]);
let of_lexemes = (ls: Lexeme.s): t =>
  List.fold_right(cons_lexeme, ls, empty);

let rec meld_to_prefix = (mel: Meld.t): t =>
  switch (Chain.unknil(mel)) {
  | None => empty
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
  | None => empty
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

// operations on melds that produce segments
module Meld_ = {
  // precond: mel is left-closed
  let uncons = (~from_piece, mel) => {
    open OptUtil.Syntax;
    let+ (_none, p, mel_tl) = Chain.unlink(mel);
    let (lx, p_tl) = from_piece(p);
    (lx, to_suffix(cat(of_lexemes(p_tl), of_meld(mel_tl))));
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
    open OptUtil.Syntax;
    let+ (mel_tl, p, _none) = Chain.unknil(mel);
    let (p_tl, c) = from_piece(p);
    (to_prefix(cat(of_meld(mel_tl), of_lexemes(p_tl))), c);
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
    let* (_, mel, seg) = Chain.unlink(seg);
    // todo: may need to convert to prefix form here
    let+ (a, tl) = from_meld(mel);
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
    let* (seg, mel, _) = Chain.unknil(seg);
    let+ (tl, a) = from_meld(mel);
    (cat(seg, tl), a);
  };
let unsnoc_from_space = ss =>
  ListUtil.split_last_opt(ss)
  |> Option.map(((ss, s)) => (ss, Lexeme.S(s)));
let unsnoc_lexeme =
  unsnoc(~from_space=unsnoc_from_space, ~from_meld=Meld_.unsnoc_lexeme);
let unsnoc_char =
  unsnoc(~from_space=unsnoc_from_space, ~from_meld=Meld_.unsnoc_char);

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
        (~match: bool, pre: t, ~kid: option(Sort.t)=?, t: Token.t)
        : Mold.Result.t =>
  switch (Chain.unknil(pre)) {
  | None => Error(kid)
  | Some((pre, mel, _)) =>
    open Result.Syntax;
    let/ kid = Meld.mold(mel, ~kid?, t);
    mold(~match, pre, ~kid?, t);
  };

let push_meld =
    (mel: Meld.t, ~kid=Meld.Padded.empty(), seg: t)
    : (Cmp.r(t, Meld.Padded.t, t, t) as 'r) =>
  seg
  |> Chain.fold_left(
       s => Cmp.Lt(Meld.Padded.pad(~r=s, kid)),
       (r: 'r, mel', s) =>
         switch (r) {
         | In(seg) => In(Chain.knil(seg, mel', s))
         | Lt(kid) =>
           switch (Meld.cmp_merge(mel, ~kid, mel')) {
           | In(mel) => In(of_padded(Meld.Padded.pad(~r=s, mel)))
           | Lt(kid) => Lt(kid)
           | Eq(mel) => Eq(of_padded(Meld.Padded.pad(~r=s, mel)))
           | Gt(kid) => Gt(Chain.knil(of_padded(kid), mel', s))
           }
         | Eq(seg) => Eq(Chain.knil(seg, mel', s))
         | Gt(seg) => Gt(Chain.knil(seg, mel', s))
         },
     );
let hsup_meld =
    (seg: t, ~kid=Meld.Padded.empty(), mel: Meld.t)
    : (Cmp.r(t, t, t, Meld.Padded.t) as 'r) =>
  seg
  |> Chain.fold_right(
       (s, mel', r: 'r) =>
         switch (r) {
         | In(seg) => In(Chain.link(s, mel', seg))
         | Lt(seg) => Lt(Chain.link(s, mel', seg))
         | Eq(seg) => Eq(Chain.link(s, mel', seg))
         | Gt(kid) =>
           switch (Meld.cmp_merge(mel', ~kid, mel)) {
           | In(mel) => In(of_padded(Meld.Padded.pad(~l=s, mel)))
           | Lt(kid) => Lt(Chain.link(s, mel', of_padded(kid)))
           | Eq(mel) => Eq(of_padded(Meld.Padded.pad(~l=s, mel)))
           | Gt(kid) => Gt(kid)
           }
         },
       s => Cmp.Gt(Meld.Padded.pad(~l=s, kid)),
     );

let split_lt = (pre: t, sel: t): (t as '_lt, t as '_geq) =>
  pre
  |> Chain.fold_right(
       (s, mel, ((lt, geq), sel)) =>
         if (has_meld(lt)) {
           ((Chain.link(s, mel, lt), geq), sel);
         } else {
           switch (push_meld(mel, sel)) {
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
           switch (hsup_meld(sel, mel)) {
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
