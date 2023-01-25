open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Chain.t(Space.s, Meld.t);

// when input meld structure (specifically parent-kid relations)
// must be broken to give proper assembly
exception Nonmonotonic;

// ill-fitting tips
exception Disconnected;

let empty = ([Space.empty], []);
let is_empty: t => bool = (==)(empty);

let has_meld = seg => Option.is_some(Chain.unlink(seg));

let cons = Chain.link;
let snoc = Chain.knil;

let cons_space = (s, seg) => Chain.map_fst((@)(s), seg);
let cons_meld = (mel, seg) => Chain.link(Space.empty, mel, seg);
let cons_lexeme = (l: Lexeme.t, seg: t): t =>
  switch (l) {
  | S(s) => cons_space([s], seg)
  | T(t) => Chain.link(Space.empty, Meld.of_tile(t), seg)
  | G(g) => Chain.link(Space.empty, Meld.of_grout(g), seg)
  };

let snoc_space = (seg, s) => Chain.map_lst(s' => s' @ s, seg);
let snoc_meld = (seg, mel) => Chain.knil(seg, mel, Space.empty);

let cat: (t, t) => t = Chain.cat((@));
let concat = (segs: list(t)): t => List.fold_right(cat, segs, empty);

let of_space = (s: Space.s): t => Chain.of_loop(s);
let of_meld = (mel: Meld.t): t => Chain.mk(Space.[empty, empty], [mel]);
let of_padded = ((mel, (l, r)): Meld.Padded.t): t =>
  Chain.mk([l, r], [mel]);

let of_lexemes = (ls: list(Lexeme.t)): t =>
  List.fold_right(cons_lexeme, ls, empty);

let join = (segs: Chain.t(Space.s, t)): t =>
  segs
  |> Chain.fold_right(
       (s, seg, acc) => concat([of_space(s), seg, acc]),
       s => of_space(s),
     );

[@warning "-27"]
let pop_lexeme = (~from: Dir.t, seg: t) => failwith("todo pop_lexeme");

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
           | In(_) => raise(Disconnected)
           | Lt(_) => ((cons(s, mel, lt), geq), sel)
           | Eq(sel)
           | Gt(sel) => ((lt, cons(s, mel, geq)), sel)
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
           | In(_) => raise(Disconnected)
           | Lt(sel)
           | Eq(sel) => ((snoc(leq, mel, s), gt), sel)
           | Gt(_) => ((leq, snoc(gt, mel, s)), sel)
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
           | In () => raise(Disconnected)
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
           | In () => raise(Disconnected)
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
    kid_pre |> cons_space(s) |> cons_meld(Chain.knil(tl, p, None));
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
