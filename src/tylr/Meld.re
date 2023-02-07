open Sexplib.Std;
open Util;

module Paths = {
  // current a list but could turn into record for specific paths
  // (eg cursor vs variable uses)
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(Path.t);

  let merge = List.concat;

  let link = (ps_kid, ps_p, ps_mel: t) => {
    let ps_kid = List.map(Path.cons(0), ps_kid);
    let ps_p = List.map(Path.of_piece(0), ps_p);
    let ps_mel = List.map(Path.link, ps_mel);
    merge([ps_kid, ps_p, ps_mel]);
  };
  let knil = (~len, ps_mel: t, ps_p, ps_kid) => {
    let ps_mel = List.map(Path.knil(~len), ps_mel);
    let ps_p = List.map(Path.of_piece(len), ps_p);
    let ps_kid = List.map(Path.cons(len + 1), ps_kid);
    merge([ps_mel, ps_p, ps_kid]);
  };

  let unlink = ListUtil.partition3_map(Path.unlink);
  let unknil = (~len) => ListUtil.partition3_map(Path.unknil(~len));

  // let trim = step => List.map(Path.trim(step));
  let with_kid = (ps: t, kid: int) =>
    List.filter_map(Path.with_kid(kid), ps);
  let with_piece = (ps: t, index: int) =>
    List.filter_map(Path.with_piece(index), ps);
  let with_space = (ps: t, side: Dir.t) =>
    List.filter_map(Path.with_space(side), ps);
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  chain: Chain.t(kid, Piece.t),
  paths: Paths.t,
  space: (Space.t, Space.t),
}
[@deriving (show({with_path: false}), sexp, yojson)]
and kid = option(t);

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

let mk = (~l=Space.empty, ~r=Space.empty, ~paths=[], chain) => {
  space: (l, r),
  paths,
  chain,
};
let empty_chain = Chain.of_loop(None);
let empty = mk(empty_chain);
let is_empty = (mel: t) => mel.chain == empty_chain;

// todo: defer path management to aggregate_paths
let pad = (~l=Space.empty, ~r=Space.empty, mel) => {
  let paths =
    List.concat([
      List.map(Path.of_space(L), l.paths),
      List.map(Path.shift_space(~side=L, Space.length(l)), mel.paths),
      List.map(Path.of_space(R), r.paths),
    ]);

  let (l', r') = mel.space;
  let space = Space.(cat(clear_paths(l), l'), cat(r', clear_paths(r)));
  {...mel, paths, space};
};

let add_paths = (paths, mel) => {...mel, paths: paths @ mel.paths};
let clear_paths = mel => {...mel, paths: []};

let paths_of_kid =
  fun
  | None => []
  | Some(kid) => kid.paths;

let unpad = mel => {
  let (l, r) = mel.space;
  let with_s = Paths.with_space(mel.paths);
  let space = Space.(add_paths(with_s(L), l), add_paths(with_s(R), r));
  (space, {...mel, space: Space.(empty, empty)});
};

let distribute_paths = mel => {
  // distribute paths into space
  let (space, mel) = unpad(mel);
  let with_k = Paths.with_kid(mel.paths);
  let with_p = Paths.with_piece(mel.paths);
  let chain =
    mel.chain
    |> Chain.mapi(
         i => Option.map(add_paths(with_k(i))),
         j => Piece.add_paths(with_p(j)),
       );
  {chain, space, paths: []};
};

let aggregate_paths = mel => {
  let (l, r) = mel.space;
  let ps_space =
    List.map(Path.of_space(L), l.paths)
    @ List.map(Path.of_space(R), r.paths);
  let space = Space.(clear_paths(l), clear_paths(r));

  let ps_chain =
    mel.chain
    |> Chain.mapi(
         (i, kid) => List.map(Path.cons(i), paths_of_kid(kid)),
         (i, p: Piece.t) => List.map(Path.of_piece(i), p.paths),
       )
    |> Chain.to_list(Fun.id, Fun.id)
    |> List.concat;
  let chain =
    mel.chain |> Chain.map(Option.map(clear_paths), Piece.clear_paths);

  // todo: maintain order if invariant
  {chain, space, paths: ps_space @ ps_chain};
};

let is_empty = (mel: t) => {
  let mel = distribute_paths(mel);
  let (l, r) = mel.space;
  mel.chain == empty_chain ? Some(Space.cat(l, r)) : None;
};

let of_piece = (~l=?, ~r=?, p: Piece.t) => Chain.mk([l, r], [p]);
let of_grout = (~l=?, ~r=?, g: Grout.t) =>
  of_piece(~l?, ~r?, Piece.mk(G(g)));
let of_tile = (~l=?, ~r=?, t: Tile.t) =>
  of_piece(~l?, ~r?, Piece.mk(T(t)));

let root = mel => Chain.links(mel.chain);
let kids = mel => Chain.loops(mel.chain);
let length = mel => List.length(root(mel));

// todo: review in wrt to preserving space paths
let absorb_space_l = mel => {
  let (l, r) = mel.space;
  let chain = mel.chain |> Chain.map_fst(Option.map(kid => pad(~l, kid)));
  let paths =
    mel.paths
    |> List.map((path: Path.t) =>
         switch (path) {
         | {kids: [], here: Space(L, _)} => {...path, kids: [0]}
         | _ => path
         }
       );
  mk(~r, ~paths, chain);
};
let absorb_space_r = mel => {
  let (l, r) = mel.space;
  let chain = mel.chain |> Chain.map_lst(Option.map(kid => pad(kid, ~r)));
  let paths =
    mel.paths
    |> List.map((path: Path.t) =>
         switch (path) {
         | {kids: [], here: Space(L, _)} => {...path, kids: [length(mel)]}
         | _ => path
         }
       );
  mk(~l, ~paths, chain);
};

let map_chain = (f, mel) => {...mel, chain: f(mel.chain)};

let link = (~s=Space.empty, ~kid=None, p: Piece.t, mel) =>
  absorb_space_l(mel)
  |> distribute_paths
  |> map_chain(Chain.link(kid, p))
  |> pad(~l=s)
  |> aggregate_paths;
// let link = (~kid=None, p: Piece.t, mel) => {
//   let mel = absorb_space_l(mel);
//   let paths = Paths.link(paths_of_kid(kid), p.paths, mel.paths);
//   let chain = Chain.link(Option.map(clear_paths, kid), p, mel.chain);
//   mk(~paths, chain);
// };
let knil = (~kid=None, ~s=Space.empty, mel, p: Piece.t) =>
  absorb_space_r(mel)
  |> distribute_paths
  |> map_chain(c => Chain.knil(c, p, kid))
  |> pad(~r=s)
  |> aggregate_paths;
// {
//   let mel = absorb_space_r(mel);
//   let paths =
//     Paths.knil(~len=length(mel), mel.paths, p.paths, paths_of_kid(kid));
//   let chain = Chain.knil(mel.chain, p, Option.map(clear_paths, kid));
//   mk(~paths, chain);
// };

// let unlink = mel =>
//   Chain.unlink(mel.chain)
//   |> Option.map(((kid, p, tl)) => {
//        let (ps_kid, ps_p, ps_tl) = Paths.unlink(mel.paths);
//        let kid = Option.map(add_paths(ps_kid), kid);
//        let p = Piece.{...p, paths: ps_p};
//        let tl = mk(~paths=ps_tl, tl);
//        (kid, p, tl);
//      });
// let unknil = mel =>
//   Chain.unknil(mel.chain)
//   |> Option.map(((tl, p, kid)) => {
//        let (ps_tl, ps_p, ps_kid) =
//          Paths.unknil(~len=length(mel), mel.paths);
//        let tl = mk(~paths=ps_tl, tl);
//        let p = Piece.{...p, paths: ps_p};
//        let kid = Option.map(add_paths(ps_kid), kid);
//        (tl, p, kid);
//      });

let of_kid =
  fun
  | None => mk(empty_chain)
  | Some(mel) => mel;

type unlinked = Result.t((t, Piece.t, t), t);
let unlink = (mel: t): unlinked => {
  let mel = distribute_paths(mel);
  let (l, r) = mel.space;
  Chain.unlink(mel.chain)
  |> Option.map(((kid, p, tl)) => {
       // todo: may want to externalize space in leftmost kid
       let tl = aggregate_paths(mk(tl, ~r));
       let kid = pad(~l, of_kid(kid));
       (kid, p, tl);
     })
  |> Result.of_option(~error=pad(~l, ~r, of_kid(Chain.fst(mel.chain))));
};
type unkniled = Result.t((t, Piece.t, t), t);
let unknil = mel => {
  let mel = distribute_paths(mel);
  let (l, r) = mel.space;
  Chain.unknil(mel.chain)
  |> Option.map(((tl, p, kid)) => {
       // todo: may want to externalize space in rightmost kid
       let tl = aggregate_paths(mk(~l, tl));
       let kid = pad(of_kid(kid), ~r);
       (tl, p, kid);
     })
  |> Result.of_option(~error=pad(~l, ~r, of_kid(Chain.lst(mel.chain))));
};

let rec to_lexemes = (mel): Lexeme.s => {
  let (l, r) = mel.space;
  let (l, r) = Lexeme.(s_of_space(l), s_of_space(r));
  mel.chain
  |> Chain.to_list(kid_to_lexemes, p => [Lexeme.of_piece(p)])
  |> List.cons(l)
  |> Fun.flip((@), [r])
  |> List.concat;
}
and kid_to_lexemes =
  fun
  | None => []
  | Some(mel) => to_lexemes(mel);

let tip = (side: Dir.t, mel: t): option(Tip.t) => {
  let tip = (kid, p) =>
    switch (is_empty(kid)) {
    | Some(_) => Piece.tip(side, p)
    | None => Tip.Convex
    };
  switch (side) {
  | L =>
    Result.to_option(unlink(mel))
    |> Option.map(((kid, p, _)) => tip(kid, p))
  | R =>
    Result.to_option(unknil(mel))
    |> Option.map(((_, p, kid)) => tip(kid, p))
  };
};

// let uncons_space_l = mel =>
//   switch (Chain.unlink(mel)) {
//   | None
//   | Some((Some(_), _, _)) => (Space.empty, mel)
//   | Some((None, p, tl)) =>
//     let (s, p) = Piece.pop_space_l(p);
//     (s, Chain.link(None, p, tl));
//   };
// let uncons_space_r = mel =>
//   switch (Chain.unknil(mel)) {
//   | None
//   | Some((_, _, Some(_))) => (mel, Space.empty)
//   | Some((tl, p, None)) =>
//     let (p, s) = Piece.pop_space_r(p);
//     (Chain.knil(tl, p, None), s);
//   };

// precond: root(c) != []
let sort = mel => {
  let (_, p, _) =
    Result.to_option(unlink(mel))
    |> OptUtil.get_or_raise(Invalid_argument("Meld.sort"));
  Piece.sort(p);
};
// precond: root(c) != []
let prec = _ => failwith("todo prec");

module Segment = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Chain.t(Space.t, meld);
  let of_space = (s: Space.t): t => Chain.of_loop(s);
  let empty = of_space(Space.empty);
  let of_meld = (mel: meld): t =>
    switch (is_empty(mel)) {
    | Some(s) => of_space(s)
    | None => Chain.mk(Space.[empty, empty], [mel])
    };
  let is_empty: t => bool = (==)(empty);
  let cons_space = s => Chain.map_fst(Space.cat(s));
  let snoc_space = (seg, s) => Chain.map_lst(Fun.flip(Space.cat, s), seg);
  let link = (~s=Space.empty, mel, seg) => Chain.link(s, mel, seg);
  let knil = (~s=Space.empty, seg, mel) => Chain.knil(seg, mel, s);
};

// let distribute_paths = (mel: t) =>

// let unwrap = mel =>
//   switch (unlink(mel)) {
//   | Some(_) => Some(mel)
//   | None =>
//     Chain.fst(mel.chain)
//     |> Option.map(add_paths(Paths.with_kid(mel.paths, 0)))
//   };

// todo: make sure padding gets pulled off melds
let rec to_prefix = (mel: t): Segment.t =>
  switch (unknil(mel)) {
  | Error(kid) =>
    switch (is_empty(kid)) {
    | Some(s) => Segment.of_space(s)
    | None => to_prefix(kid)
    }
  | Ok((tl_mel, p, kid)) =>
    // let (p, s) = Piece.pop_space_r(p);
    let mel = knil(tl_mel, p);
    let kid_pre = to_prefix(kid);
    Segment.(link(mel, kid_pre));
  };
// and kid_to_prefix =
//   fun
//   | None => Segment.empty
//   | Some(kid) => to_prefix(kid);
let rec to_suffix = (mel: t): Segment.t =>
  switch (unlink(mel)) {
  | Error(kid) =>
    switch (is_empty(kid)) {
    | Some(s) => Segment.of_space(s)
    | None => to_suffix(kid)
    }
  | Ok((kid, p, tl)) =>
    let mel = link(p, tl);
    let kid_suf = to_suffix(kid);
    Segment.knil(kid_suf, mel);
  };
// and kid_to_suffix =
//   fun
//   | None => Segment.empty
//   | Some(kid) => to_suffix(kid);

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
  | L => Result.to_option(unlink(mel)) |> Option.map(((_, p, _)) => p)
  | R => Result.to_option(unknil(mel)) |> Option.map(((_, p, _)) => p)
  };

let fst_id = mel => Option.map(Piece.id, end_piece(~side=L, mel));
let lst_id = mel => Option.map(Piece.id, end_piece(~side=R, mel));

let complement = (~side: Dir.t, mel: t) =>
  switch (end_piece(~side, mel)) {
  | None => []
  | Some(p) => Piece.complement(~side, p)
  };

// module Padded = {
//   // meld with padding (ie single-meld segment)
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type t = (meld, (Space.s, Space.s));
//   let mk = (~l=Space.empty, ~r=Space.empty, mel): t => (mel, (l, r));
//   let empty = (~l=Space.empty, ~r=Space.empty, ()) => mk(~l, ~r, empty);
//   let is_empty = ((mel, (l, r))) => is_empty(mel) ? Some(l @ r) : None;
//   let pad = (~l=Space.empty, ~r=Space.empty, (c, (l', r')): t) => (
//     c,
//     (l @ l', r' @ r),
//   );
// };

// module Zipped = {
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type t = (Padded.t, Path.t);
//   exception Invalid(t);
//   let init = offset => (Padded.empty(), Path.mk(~offset, ()));
// };

// let split_nth_kid = (n, mel: t) => {
//   let (ks, ps) = mel;
//   print_endline("split_nth_kid bef");
//   let (ks_l, k, ks_r) = ListUtil.split_nth(n, ks);
//   print_endline("split_nth_kid aft");
//   let (ps_l, ps_r) = ListUtil.split_n(n, ps);
//   (Chain.mk(ks_l @ [None], ps_l), k, Chain.mk([None, ...ks_r], ps_r));
// };

let zip_piece_l = (p_l: Piece.t, mel: t): option(t) => {
  open OptUtil.Syntax;
  let* (kid, p_r, tl) = Result.to_option(unlink(mel));
  let+ p = Piece.zip(p_l, p_r);
  assert(Option.is_some(is_empty(kid)));
  pad(~l=Option.get(is_empty(kid)), link(p, tl));
};
let zip_piece_r = (mel: t, p_r: Piece.t): option(t) => {
  open OptUtil.Syntax;
  let* (tl, p_l, kid) = Result.to_option(unknil(mel));
  let+ p = Piece.zip(p_l, p_r);
  assert(Option.is_some(is_empty(kid)));
  pad(knil(tl, p), ~r=Option.get(is_empty(kid)));
};

let is_closed_l = mel =>
  switch (unlink(mel)) {
  | Ok((kid, p, tl)) when Option.is_some(is_empty(kid)) =>
    Some((Option.get(is_empty(kid)), p, tl))
  | _ => None
  };
let is_closed_r = mel =>
  switch (unknil(mel)) {
  | Ok((tl, p, kid)) when Option.is_some(is_empty(kid)) =>
    Some((tl, p, Option.get(is_empty(kid))))
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
  | (Some((_, p_l, _)), Some((_, p_r, _))) => Piece.cmp(p_l, p_r)
  };

let convexify_l = (~expected: Sort.Ana.t, mel: t): t =>
  if (is_empty(mel)) {
    of_grout(Grout.mk_convex(expected.sort));
  } else {
    let (kid, p, tl) = unlink(mel) |> OptUtil.get_or_raise(Orphaned_kid);
    switch (Piece.tip(L, p)) {
    | Convex =>
      assert(kid == None);
      mel;
    | Concave(s, _) =>
      switch (kid) {
      | Some(_) => mel
      | None =>
        let kid = of_grout(Grout.mk_convex(s));
        link(~kid, p, tl);
      }
    };
  };
let convexify_r = (~expected: Sort.Ana.t, mel) =>
  if (is_empty(mel)) {
    of_grout(Grout.mk_convex(expected.sort));
  } else {
    let (tl, p, kid) = unknil(mel) |> OptUtil.get_or_raise(Orphaned_kid);
    switch (Piece.tip(R, p)) {
    | Convex =>
      assert(kid == None);
      mel;
    | Concave(s, _) =>
      switch (kid) {
      | Some(_) => mel
      | None =>
        let kid = of_grout(Grout.mk_convex(s));
        knil(tl, p, ~kid);
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
  let* (tl_l, p_l, kid_l) = unknil(mel_l);
  let* (kid_r, p_r, tl_r) = unlink(mel_r);
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

let merge = (l: t, r: t): t => {};

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
      // todo: fixed dropped tls
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
