open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Aba.t(Space.t, Chain.t);

// when input chain structure (specifically parent-kid relations)
// must be broken to give proper assembly
exception Nonmonotonic;

// ill-fitting tips
exception Disconnected;

let empty = ([Space.empty], []);
let is_empty: t => bool = (==)(empty);

let has_chain = seg => Option.is_some(Aba.uncons(seg));

let cons = Aba.cons;
let snoc = Aba.snoc;

let cons_space = (s, seg) => Aba.map_first((@)(s), seg);
let cons_chain = (c, seg) => Aba.cons(Space.empty, c, seg);
let cons_lexeme = (l: Lexeme.t, seg: t): t =>
  switch (l) {
  | S(s) => cons_space(s, seg)
  | T(t) => Aba.cons(Space.empty, Chain.of_tile(t), seg)
  | G(g) => Aba.cons(Space.empty, Chain.of_grout(g), seg)
  };

let snoc_space = (seg, s) => Aba.map_last(s' => s' @ s, seg);
let snoc_chain = (seg, c) => Aba.snoc(seg, c, Space.empty);

let concat = (segs: list(t)): t =>
  List.fold_right(
    (seg, acc) => seg |> Aba.fold_right(Aba.cons, s => cons_space(s, acc)),
    segs,
    empty,
  );

let of_space = (s: Space.t): t => Aba.singleton(s);
let of_chain = (c: Chain.t): t => Aba.mk(Space.[empty, empty], [c]);
let of_padded = ((c, (l, r)): Chain.Padded.t): t => Aba.mk([l, r], [c]);

let of_lexemes = (ls: list(Lexeme.t)): t =>
  List.fold_right(cons_lexeme, ls, empty);

let join = (segs: Aba.t(Space.t, t)): t =>
  segs
  |> Aba.fold_right(
       (s, seg, acc) => concat([of_space(s), seg, acc]),
       s => of_space(s),
     );

[@warning "-27"]
let pop_lexeme = (~from: Dir.t, seg: t) => failwith("todo pop_lexeme");

let rec mold =
        (~match: bool, pre: t, ~kid: option(Sort.t)=?, t: Token.t)
        : Mold.Result.t =>
  switch (Aba.unsnoc(pre)) {
  | None => Error(kid)
  | Some((pre, c, _)) =>
    open Result.Syntax;
    let/ kid = Chain.mold(c, ~kid?, t);
    mold(~match, pre, ~kid?, t);
  };

let push_chain =
    (c: Chain.t, ~kid=Chain.Padded.empty(), seg: t)
    : (Cmp.r(t, Chain.Padded.t, t, t) as 'r) =>
  seg
  |> Aba.fold_left(
       s => Cmp.Lt(Chain.Padded.pad(~r=s, kid)),
       (r: 'r, c', s) =>
         switch (r) {
         | In(seg) => In(Aba.snoc(seg, c', s))
         | Lt(kid) =>
           switch (Chain.cmp_merge(c, ~kid, c')) {
           | In(c) => In(of_padded(Chain.Padded.pad(~r=s, c)))
           | Lt(kid) => Lt(kid)
           | Eq(c) => Eq(of_padded(Chain.Padded.pad(~r=s, c)))
           | Gt(kid) => Gt(Aba.snoc(of_padded(kid), c', s))
           }
         | Eq(seg) => Eq(Aba.snoc(seg, c', s))
         | Gt(seg) => Gt(Aba.snoc(seg, c', s))
         },
     );
let hsup_chain =
    (seg: t, ~kid=Chain.Padded.empty(), c: Chain.t)
    : (Cmp.r(t, t, t, Chain.Padded.t) as 'r) =>
  seg
  |> Aba.fold_right(
       (s, c', r: 'r) =>
         switch (r) {
         | In(seg) => In(Aba.cons(s, c', seg))
         | Lt(seg) => Lt(Aba.cons(s, c', seg))
         | Eq(seg) => Eq(Aba.cons(s, c', seg))
         | Gt(kid) =>
           switch (Chain.cmp_merge(c', ~kid, c)) {
           | In(c) => In(of_padded(Chain.Padded.pad(~l=s, c)))
           | Lt(kid) => Lt(Aba.cons(s, c', of_padded(kid)))
           | Eq(c) => Eq(of_padded(Chain.Padded.pad(~l=s, c)))
           | Gt(kid) => Gt(kid)
           }
         },
       s => Cmp.Gt(Chain.Padded.pad(~l=s, kid)),
     );

let split_lt = (pre: t, sel: t): (t as '_lt, t as '_geq) =>
  pre
  |> Aba.fold_right(
       (s, c, ((lt, geq), sel)) =>
         if (has_chain(lt)) {
           ((Aba.cons(s, c, lt), geq), sel);
         } else {
           switch (push_chain(c, sel)) {
           | In(_) => raise(Disconnected)
           | Lt(_) => ((cons(s, c, lt), geq), sel)
           | Eq(sel)
           | Gt(sel) => ((lt, cons(s, c, geq)), sel)
           };
         },
       s => ((empty, of_space(s)), cons_space(s, sel)),
     )
  |> fst;
let split_gt = (sel: t, suf: t): (t as '_leq, t as '_gt) =>
  suf
  |> Aba.fold_left(
       s => ((of_space(s), empty), snoc_space(sel, s)),
       (((leq, gt), sel), c, s) =>
         if (has_chain(gt)) {
           ((leq, Aba.snoc(gt, c, s)), sel);
         } else {
           switch (hsup_chain(sel, c)) {
           | In(_) => raise(Disconnected)
           | Lt(sel)
           | Eq(sel) => ((snoc(leq, c, s), gt), sel)
           | Gt(_) => ((leq, snoc(gt, c, s)), sel)
           };
         },
     )
  |> fst;

let assemble_l = (~l: option(Chain.t)=?, seg: t): t =>
  seg
  |> Aba.fold_left(
       s => of_space(s),
       (seg, c, s) =>
         switch (hsup_chain(seg, c)) {
         | In(seg)
         | Lt(seg)
         | Eq(seg) => snoc_space(seg, s)
         | Gt(kid) =>
           let l_cmp_c =
             switch (l) {
             | None => Cmp.Lt()
             | Some(l) => Chain.cmp(l, c)
             };
           switch (l_cmp_c) {
           | In () => raise(Disconnected)
           | Lt ()
           | Eq () => of_padded(Chain.(merge(kid, Padded.mk(~r=s, c))))
           | Gt () =>
             concat([of_padded(kid), of_padded(Chain.Padded.mk(~r=s, c))])
           };
         },
     );
let assemble_r = (~r: option(Chain.t)=?, seg: t): t =>
  seg
  |> Aba.fold_right(
       (s, c, seg) =>
         switch (push_chain(c, seg)) {
         | In(seg) => cons_space(s, seg)
         | Lt(kid) =>
           let c_cmp_r =
             switch (r) {
             | None => Cmp.Gt()
             | Some(r) => Chain.cmp(c, r)
             };
           switch (c_cmp_r) {
           | In () => raise(Disconnected)
           | Lt () =>
             concat([of_padded(Chain.Padded.mk(~l=s, c)), of_padded(kid)])
           | Eq ()
           | Gt () => of_padded(Chain.(merge(Padded.mk(~l=s, c), kid)))
           };
         | Eq(seg)
         | Gt(seg) => cons_space(s, seg)
         },
       s => of_space(s),
     );
// todo: rename this meld
let assemble = (~l: option(Chain.t)=?, ~r: option(Chain.t)=?, seg: t): t =>
  seg |> assemble_r(~r?) |> assemble_l(~l?);

let to_padded =
  fun
  | ([s], []) => Some(Chain.Padded.empty(~r=s, ()))
  | ([l, r], [c]) => Some(Chain.Padded.mk(~l, ~r, c))
  | _ => None;

// precond: in prefix form
// todo: rework using aba interface or possibly reformulate
// overall using parent merging
let finish_prefix = (pre: t): Chain.Padded.t =>
  pre
  |> Aba.fold_right(
       (s, c, kid) => Chain.(merge(Padded.mk(~l=s, c), kid)),
       s => Chain.(Padded.mk(~l=s, Chain.empty)),
     );

let rec chain_to_prefix = (c: Chain.t): t =>
  switch (Aba.unsnoc(c)) {
  | None => empty
  | Some((tl, p, kid)) =>
    let (p, s) = Piece.pop_space_r(p);
    let kid_pre =
      switch (kid) {
      | None => empty
      | Some(K(kid)) => chain_to_prefix(kid)
      };
    kid_pre |> cons_space(s) |> cons_chain(Aba.snoc(tl, p, None));
  };
let to_prefix: t => t =
  Aba.fold_right(
    (s, c, pre) => concat([of_space(s), chain_to_prefix(c), pre]),
    of_space,
  );

let rec chain_to_suffix = (c: Chain.t): t =>
  switch (Aba.uncons(c)) {
  | None => empty
  | Some((kid, p, tl)) =>
    let (s, p) = Piece.pop_space_l(p);
    let kid_suf =
      switch (kid) {
      | None => empty
      | Some(K(kid)) => chain_to_suffix(kid)
      };
    snoc_chain(snoc_space(kid_suf, s), Aba.cons(None, p, tl));
  };
let to_suffix: t => t =
  Aba.fold_left(of_space, (suf, c, s) =>
    concat([suf, chain_to_suffix(c), of_space(s)])
  );
