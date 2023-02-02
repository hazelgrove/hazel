open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Chain.t(option(kid), Piece.t)
[@deriving (show({with_path: false}), sexp, yojson)]
and kid =
  | K(t);

// for use in submodules below
[@deriving (show({with_path: false}), sexp, yojson)]
type meld = t;

// we expect a kid to be constructed only when there is
// a concrete parent piece inducing kidhood, hence we should
// never encounter a meld consisting solely of Some(kid).
exception Orphaned_kid;
// we expect kids to have higher precedence than their
// parent tips (which may be min prec in bidelim containers)
exception Invalid_prec;
exception Missing_root;

let empty = Chain.of_loop(None);
let is_empty: t => bool = (==)(empty);

let of_piece = (~l=?, ~r=?, p: Piece.t) => Chain.mk([l, r], [p]);
let of_grout = (~l=?, ~r=?, g: Grout.t) =>
  of_piece(~l?, ~r?, Piece.mk(G(g)));
let of_tile = (~l=?, ~r=?, t: Tile.t) =>
  of_piece(~l?, ~r?, Piece.mk(T(t)));

let link = (~kid=None, p, mel) => Chain.link(kid, p, mel);
let knil = (mel, p, ~kid=None, ()) => Chain.knil(mel, p, kid);

let root: t => list(Piece.t) = Chain.links;
let kids: t => list(option(kid)) = Chain.loops;

let rec to_lexemes = c =>
  c |> Chain.to_list(kid_to_lexemes, Lexeme.s_of_piece) |> List.concat
and kid_to_lexemes =
  fun
  | None => []
  | Some(K(c)) => to_lexemes(c);

let tip = (side: Dir.t, mel: t): option(Tip.t) => {
  let tip = (kid, p) =>
    switch (kid) {
    | None => Piece.tip(side, p)
    | Some(_) => Tip.Convex
    };
  switch (side) {
  | L => Chain.unlink(mel) |> Option.map(((kid, p, _)) => tip(kid, p))
  | R => Chain.unknil(mel) |> Option.map(((_, p, kid)) => tip(kid, p))
  };
};

let uncons_space_l = mel =>
  switch (Chain.unlink(mel)) {
  | None
  | Some((Some(_), _, _)) => (Space.empty, mel)
  | Some((None, p, tl)) =>
    let (s, p) = Piece.pop_space_l(p);
    (s, Chain.link(None, p, tl));
  };
let uncons_space_r = mel =>
  switch (Chain.unknil(mel)) {
  | None
  | Some((_, _, Some(_))) => (mel, Space.empty)
  | Some((tl, p, None)) =>
    let (p, s) = Piece.pop_space_r(p);
    (Chain.knil(tl, p, None), s);
  };

// precond: root(c) != []
let sort = mel => {
  let (_, p, _) =
    Chain.unlink(mel) |> OptUtil.get_or_raise(Invalid_argument("Meld.sort"));
  Piece.sort(p);
};
// precond: root(c) != []
let prec = _ => failwith("todo prec");

let rec to_prefix = (mel: t): Chain.t(Space.s, t) => {
  let kid_pre =
    switch (Chain.lst(mel)) {
    | None => Chain.of_loop(Space.empty)
    | Some(K(kid)) => to_prefix(kid)
    };
  switch (Chain.unknil(mel)) {
  | None => kid_pre
  | Some((tl_mel, p, _kid)) =>
    let (p, s) = Piece.pop_space_r(p);
    let mel = knil(tl_mel, p, ());
    Chain.(link(Space.empty, mel, map_fst((@)(s), kid_pre)));
  };
};
let rec to_suffix = (mel: t): Chain.t(Space.s, t) => {
  let kid_suf =
    switch (Chain.fst(mel)) {
    | None => Chain.of_loop(Space.empty)
    | Some(K(kid)) => to_suffix(kid)
    };
  switch (Chain.unlink(mel)) {
  | None => kid_suf
  | Some((_kid, p, tl)) =>
    let (s, p) = Piece.pop_space_l(p);
    let mel = link(p, tl);
    Chain.(knil(map_lst(Fun.flip((@), s), kid_suf), mel, Space.empty));
  };
};

let mold = (mel: t, ~kid: option(Sort.o)=?, t: Token.t): Mold.Result.t => {
  open Result.Syntax;
  // todo: possibly join with kid sort
  let error = Some(sort(mel));
  let* tip = Result.of_option(~error, tip(R, mel));
  switch (tip) {
  | Tip.Convex => Error(error)
  | Concave(sort, _) =>
    Result.of_option(~error, LangUtil.mold_of_token(kid, sort, t))
  };
};

let end_piece = (~side: Dir.t, mel: t): option(Piece.t) =>
  switch (side) {
  | L => Chain.unlink(mel) |> Option.map(((_, p, _)) => p)
  | R => Chain.unknil(mel) |> Option.map(((_, p, _)) => p)
  };

let fst_id = mel => Option.map(Piece.id, end_piece(~side=L, mel));
let lst_id = mel => Option.map(Piece.id, end_piece(~side=R, mel));

let complement = (~side: Dir.t, mel: t) =>
  switch (end_piece(~side, mel)) {
  | None => []
  | Some(p) => Piece.complement(~side, p)
  };

module Padded = {
  // meld with padding (ie single-meld segment)
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (meld, (Space.s, Space.s));
  let mk = (~l=Space.empty, ~r=Space.empty, mel): t => (mel, (l, r));
  let empty = (~l=Space.empty, ~r=Space.empty, ()) => mk(~l, ~r, empty);
  let is_empty = ((mel, (l, r))) => is_empty(mel) ? Some(l @ r) : None;
  let pad = (~l=Space.empty, ~r=Space.empty, (c, (l', r')): t) => (
    c,
    (l @ l', r' @ r),
  );
};

let length = mel => List.length(root(mel));

module Step = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = int;
};
module Path = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // top-down
    steps: list(Step.t),
    // negative offset: last piece of last meld
    // positive offset: trailing space
    offset: int,
  };
  let mk = (~steps=[], ~offset=0, ()) => {steps, offset};
  let empty = mk();

  let cons = (step, path) => {...path, steps: [step, ...path.steps]};
  let cons_s = (steps, path) => List.fold_right(cons, steps, path);
};
module Zipped = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (Padded.t, Path.t);
  exception Invalid(t);
  let init = offset => (Padded.empty(), Path.mk(~offset, ()));
};

let split_nth_kid = (n, mel: t) => {
  let (ks, ps) = mel;
  print_endline("split_nth_kid bef");
  let (ks_l, k, ks_r) = ListUtil.split_nth(n, ks);
  print_endline("split_nth_kid aft");
  let (ps_l, ps_r) = ListUtil.split_n(n, ps);
  (Chain.mk(ks_l @ [None], ps_l), k, Chain.mk([None, ...ks_r], ps_r));
};

let zip_piece_l = (p_l: Piece.t, mel: t): option(t) => {
  open OptUtil.Syntax;
  let* (kid, p_r, tl) = Chain.unlink(mel);
  let+ p = Piece.zip(p_l, p_r);
  assert(kid == None);
  Chain.link(kid, p, tl);
};
let zip_piece_r = (mel: t, p_r: Piece.t): option(t) => {
  open OptUtil.Syntax;
  let* (tl, p_l, kid) = Chain.unknil(mel);
  let+ p = Piece.zip(p_l, p_r);
  assert(kid == None);
  Chain.knil(tl, p, kid);
};

let is_closed_l = mel =>
  switch (Chain.unlink(mel)) {
  | Some((None, p, tl)) => Some((p, tl))
  | _ => None
  };
let is_closed_r = mel =>
  switch (Chain.unknil(mel)) {
  | Some((tl, p, None)) => Some((tl, p))
  | _ => None
  };

// precond: l is right-closed, r is left-closed
// todo: consider generalizing to return expected sort
let cmp = (l: t, r: t) =>
  // todo: probably want to relax closed requirement
  switch (is_closed_r(l), is_closed_l(r)) {
  | (None, _)
  | (_, None) =>
    print_endline("l = " ++ show(l));
    print_endline("r = " ++ show(r));
    raise(Invalid_argument("Meld.cmp"));
  | (Some((_, p_l)), Some((p_r, _))) => Piece.cmp(p_l, p_r)
  };

let convexify_l = (~expected: Sort.Ana.t, c) =>
  if (is_empty(c)) {
    of_grout(Grout.mk_convex(expected.sort));
  } else {
    let (kid, p, tl) =
      Chain.unlink(c) |> OptUtil.get_or_raise(Orphaned_kid);
    switch (Piece.tip(L, p)) {
    | Convex =>
      assert(kid == None);
      c;
    | Concave(s, _) =>
      switch (kid) {
      | Some(_) => c
      | None =>
        let kid = of_grout(Grout.mk_convex(s));
        Chain.link(Some(K(kid)), p, tl);
      }
    };
  };
let convexify_r = (~expected: Sort.Ana.t, c) =>
  if (is_empty(c)) {
    of_grout(Grout.mk_convex(expected.sort));
  } else {
    let (tl, p, kid) =
      Chain.unknil(c) |> OptUtil.get_or_raise(Orphaned_kid);
    switch (Piece.tip(R, p)) {
    | Convex =>
      assert(kid == None);
      c;
    | Concave(s, _) =>
      switch (kid) {
      | Some(_) => c
      | None =>
        let kid = of_grout(Grout.mk_convex(s));
        Chain.knil(tl, p, Some(K(kid)));
      }
    };
  };
let convexify = (~expected: Sort.Ana.t, c) =>
  c |> convexify_r(~expected) |> convexify_l(~expected);

let rec is_porous = c =>
  c
  |> Chain.to_list(kid_is_porous, Piece.is_porous)
  |> OptUtil.sequence
  |> Option.map(List.concat)
and kid_is_porous =
  fun
  | None => Some(Space.empty)
  | Some(K(kid)) => is_porous(kid);

// precond: l and r are nonempty
let rec degrout = (l: Padded.t, r: Padded.t): option(Padded.t) => {
  let (mel_l, (s_ll, s_lr)) = l;
  let (mel_r, (s_rl, s_rr)) = r;
  open OptUtil.Syntax;
  let* (tl_l, p_l, kid_l) = Chain.unknil(mel_l);
  let* (kid_r, p_r, tl_r) = Chain.unlink(mel_r);
  let* dg = Piece.degrout(p_l, p_r);
  switch (dg) {
  | Degrout =>
    let* s_l = kid_is_porous(kid_l);
    let* s_r = kid_is_porous(kid_r);
    let l = Padded.mk(~l=s_ll, ~r=s_lr @ s_l, tl_l);
    let r = Padded.mk(~l=s_r @ s_rl, ~r=s_rr, tl_r);
    degrout(l, r);
  | Fill(d) =>
    let+ s_l = kid_is_porous(kid_l)
    and+ s_r = kid_is_porous(kid_r);
    let s_mid = List.concat([s_l, s_lr, s_rl, s_r]);
    let p =
      switch (d) {
      | L => Piece.pad(~l=Piece.space(p_l) @ s_mid, p_r)
      | R => Piece.pad(~r=s_mid @ Piece.space(p_r), p_l)
      };
    Padded.mk(~l=s_ll, ~r=s_rr, Chain.append(tl_l, p, tl_r));
  | Pass(L) =>
    let* s_l = kid_is_porous(kid_l);
    let s_mid = List.concat([Piece.space(p_l), s_l, s_lr]);
    let l = Padded.mk(~l=s_ll, ~r=s_mid, tl_l);
    degrout(l, r);
  | Pass(R) =>
    let* s_r = kid_is_porous(kid_r);
    let s_mid = List.concat([s_rl, s_r, Piece.space(p_l)]);
    let r = Padded.mk(~l=s_mid, ~r=s_rr, tl_r);
    degrout(l, r);
  };
};

let rec merge = (l: Padded.t, r: Padded.t): Padded.t =>
  switch (Padded.is_empty(l), Padded.is_empty(r)) {
  | (Some(l), Some(r)) => Padded.empty(~l, ~r, ())
  | (Some(l), None) => Padded.pad(~l, r)
  | (None, Some(r)) => Padded.pad(~r, l)
  | (None, None) => merge_nonempty(l, r)
  }
and merge_nonempty = (l, r) =>
  switch (degrout(l, r)) {
  | Some(c) => c
  | None =>
    let (c_l, (s_ll, s_lr)) = l;
    let (c_r, (s_rl, s_rr)) = r;
    let (tl_l, p_l, kid_l) =
      Chain.unknil(c_l) |> OptUtil.get_or_raise(Orphaned_kid);
    let (kid_r, p_r, tl_r) =
      Chain.unlink(c_r) |> OptUtil.get_or_raise(Orphaned_kid);
    switch (Piece.cmp(p_l, p_r)) {
    | In((sort, prec)) =>
      assert(kid_l == None && kid_r == None);
      // todo: may want to use expected sort here.
      // as it stands, this bottom-up method of sorting
      // the grout may lead to sort inconsistency in general.
      // or this may be fine under interpretation of grout
      // sort being upper bound on its parent's expected sort.
      Piece.mk(G(Grout.mk_concave(sort, prec)))
      |> Piece.pad(~l=s_lr, ~r=s_rl)
      |> of_piece(~l=?kid_l, ~r=?kid_r)
      |> Padded.mk(~l=s_ll, ~r=s_rr);
    | Lt(expected) =>
      assert(kid_l == None);
      let p_l = Piece.pad(~r=s_lr @ s_rl, p_l);
      let r = mk_kid(~expected, c_r);
      Chain.knil(tl_l, p_l, Some(r)) |> Padded.mk(~l=s_ll, ~r=s_rr);
    | Eq(expected) =>
      let (kid, (l, r)) = merge_kids(~expected, kid_l, s_lr, s_rl, kid_r);
      let p_l = Piece.pad(~r=l, p_l);
      let p_r = Piece.pad(~l=r, p_r);
      Chain.append(tl_l, p_l, Chain.link(Some(K(kid)), p_r, tl_r))
      |> Padded.mk(~l=s_ll, ~r=s_rr);
    | Gt(expected) =>
      assert(kid_l == None);
      let p_r = Piece.pad(~l=s_lr @ s_rl, p_r);
      let l = mk_kid(~expected, c_l);
      Chain.link(Some(l), p_r, tl_r) |> Padded.mk(~l=s_ll, ~r=s_rr);
    };
  }
and merge_kids =
    (
      ~expected: Sort.Ana.t,
      l: option(kid),
      s_l: Space.s,
      s_r: Space.s,
      r: option(kid),
    )
    : Padded.t =>
  switch (l, r) {
  | (None, None) =>
    of_grout(Grout.mk_convex(expected.sort)) |> Padded.mk(~l=s_l, ~r=s_r)
  | (None, Some(K(r))) => Padded.mk(~l=s_l @ s_r, r)
  | (Some(K(l)), None) => Padded.mk(~r=s_l @ s_r, l)
  | (Some(K(l)), Some(K(r))) =>
    let l = Padded.mk(~r=s_l, l);
    let r = Padded.mk(~l=s_r, r);
    merge(l, r);
  }
and mk_kid = (~expected: Sort.Ana.t, c: t): kid =>
  K(convexify(~expected, c));

let merge_all = pcs => List.fold_right(merge, pcs, Padded.empty());

let cmp_mold = (_: t, _: Mold.t): option(Cmp.t) =>
  failwith("todo cmp_mold");

let cmp_merge = (l: t, ~kid=Padded.empty(), r: t): Cmp.s(Padded.t) =>
  // todo: incorporate sort info produced by cmp
  switch (cmp(l, r)) {
  | In(_) =>
    let (c, (s_l, s_r)) = kid;
    assert(is_empty(c));
    In(merge(Padded.mk(~r=s_l, l), Padded.mk(~l=s_r, r)));
  | Lt(_) => Lt(merge(kid, Padded.mk(r)))
  | Eq(_) => Eq(merge_all([Padded.mk(l), kid, Padded.mk(r)]))
  | Gt(_) => Gt(merge(Padded.mk(l), kid))
  };
