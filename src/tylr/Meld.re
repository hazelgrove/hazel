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
  chain: option(Chain.t(t, Piece.t)),
  paths: Paths.t,
  space: (Space.t, Space.t),
};

// [@deriving (show({with_path: false}), sexp, yojson)]
// type t = {
//   chain: Chain.t(kid, Piece.t),
//   paths: Paths.t,
//   space: (Space.t, Space.t),
// }
// [@deriving (show({with_path: false}), sexp, yojson)]
// and kid = option(t);

// for use in submodules below
[@deriving (show({with_path: false}), sexp, yojson)]
type meld = t;

module Closed = {
  type l = (meld, Piece.t);
  type r = (Piece.t, meld);
  type t = Chain.t(Piece.t, meld);
};

// we expect a kid to be constructed only when there is
// a concrete parent piece inducing kidhood, hence we should
// never encounter a meld consisting solely of Some(kid).
exception Orphaned_kid;
// we expect kids to have higher precedence than their
// parent tips (which may be min prec in bidelim containers)
exception Invalid_prec;
exception Missing_root;

let mk = (~l=Space.empty, ~r=Space.empty, ~paths=[], ~chain=?, ()) => {
  space: (l, r),
  paths,
  chain,
};
let empty = (~l=Space.empty, ~r=Space.empty, ~paths=[], ()) =>
  mk(~l, ~r, ());
let is_empty = (mel: t) => Option.is_none(mel.chain);

let add_paths = (paths, mel) => {...mel, paths: paths @ mel.paths};
let clear_paths = mel => {...mel, paths: []};
let paths_of_kid =
  fun
  | None => []
  | Some(kid) => kid.paths;

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
let unpad = mel => {
  let (l, r) = mel.space;
  let with_s = Paths.with_space(mel.paths);
  let space = Space.(add_paths(with_s(L), l), add_paths(with_s(R), r));
  (space, {...mel, space: Space.(empty, empty)});
};

let distribute_paths = mel => {
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
  let ps_l = List.map(Path.of_space(L), l.paths);
  let ps_r = List.map(Path.of_space(R), r.paths);
  let ps_chain =
    mel.chain
    |> Chain.mapi(
         (i, kid) => List.map(Path.cons(i), paths_of_kid(kid)),
         (i, p: Piece.t) => List.map(Path.of_piece(i), p.paths),
       )
    |> Chain.to_list(Fun.id, Fun.id)
    |> List.concat;
  let paths = List.concat([ps_l, ps_chain, ps_r]);
  let space = Space.(clear_paths(l), clear_paths(r));
  let chain =
    mel.chain |> Chain.map(Option.map(clear_paths), Piece.clear_paths);
  {paths, space, chain};
};

let is_empty = (mel: t) => {
  let mel = distribute_paths(mel);
  let (l, r) = mel.space;
  mel.chain == empty_chain ? Some(Space.cat(l, r)) : None;
};

// todo: review for externalizing padding
let of_piece = (~l=?, ~r=?, p: Piece.t) =>
  mk(Chain.mk([l, r], [p])) |> aggregate_paths;
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

let link = (~s=Space.empty, ~kid=?, p: Piece.t, mel) =>
  absorb_space_l(mel)
  |> distribute_paths
  // todo: externalize kid's left padding
  |> map_chain(Chain.link(kid, p))
  |> pad(~l=s)
  |> aggregate_paths;
let knil = (~kid=?, ~s=Space.empty, mel, p: Piece.t) =>
  absorb_space_r(mel)
  |> distribute_paths
  // todo: externalize kid's right padding
  |> map_chain(c => Chain.knil(c, p, kid))
  |> pad(~r=s)
  |> aggregate_paths;

let prepend = (_: Closed.r, _: t) => failwith("todo prepend");
let append = (_: t, _: Closed.l) => failwith("todo append");

type lt = (Closed.r, t);
let lt = (l: Closed.r, ~kid=empty(), r: Closed.l): option(t) => {
  open OptUtil.Syntax;
  let (_, p_l) = l;
  let (p_r, _) = r;
  let+ _ = Piece.lt(p_l, p_r);
  let kid =
    switch (Piece.tip(L, p_r)) {
    | Convex =>
      assert(Option.is_some(is_empty(kid)));
      kid;
    | Concave(s, _) => complete(s, kid)
    };
  Closed.open_l(kid, r);
};

type gt = (t, Closed.l);
let gt = (l: Closed.r, ~kid=empty(), r: Closed.l): option(t) => {
  open OptUtil.Syntax;
  let (_, p_l) = l;
  let (p_r, _) = r;
  let+ _ = Piece.gt(p_l, p_r);
  let kid =
    switch (Piece.tip(R, p_l)) {
    | Convex =>
      assert(Option.is_some(is_empty(kid)));
      kid;
    | Concave(s, _) => complete(s, kid)
    };
  Closed.open_r(l, kid);
};

let rec eq = (l: Closed.r, ~kid=empty(), r: Closed.l): option(t) => {
  open OptUtil.Syntax;
  let (tl_l, p_l) = l;
  let (p_r, tl_r) = r;
  switch (is_empty(kid), Piece.replaces(p_l, p_r), Piece.passes(p_l, p_r)) {
  | (Some(s), Some(L), _) => return(append(pad(tl_l, ~r=s), r))
  | (Some(s), Some(R), _) => return(prepend(l, pad(~l=s, tl_r)))
  | (Some(s), _, Some(L)) =>
    let (l, kid) = unknil_(tl_l);
    let* l = l;
    let kid = pad(kid, ~r);
    eq(l, ~kid, r);
  | (Some(s), _, Some(R)) =>
    let (kid, r) = unlink_(tl_r);
    let* r = r;
    let kid = pad(~l, kid);
    eq(l, ~kid, r);
  | _ =>
    let+ compl = Piece.eq(p_l, p_r);
    // todo: abstract into some join-complement fn
    let (hd_r, tl_r) =
      List.fold_right(
        ((sugg, mold), (p, tl)) =>
          switch (Mold.tip(R, mold)) {
          | Convex => raise(Invalid_prec)
          | Concave(s, _) =>
            let kid = Some(of_grout(Grout.mk_convex(s)));
            let g = Piece.of_grout(Grout.mk(~sugg, mold));
            (g, link(~kid, p, tl));
          },
        compl,
        (p_r, tl_r),
      );
    prepend(l, link(~kid, hd_r, tl_r));
  };
};

type cmp = {
  lt: option(t),
  eq: option(t),
  gt: option(t),
};
let cmp = (l: Closed.r, ~kid=empty(), r: Closed.l) => {
  lt: lt(l, ~kid, r),
  eq: eq(l, ~kid, r),
  gt: gt(l, ~kid, r),
};

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

let unlink_ = (mel: t): (t, option(Closed.l)) => {
  let mel = distribute_paths(mel);
  let (l, r) = mel.space;
  Chain.unlink(mel.chain)
  |> Option.map(((kid, p, tl)) => {
       // todo: may want to externalize space in leftmost kid
       let tl = aggregate_paths(mk(tl, ~r));
       let kid = pad(~l, kid);
       (kid, Some((p, tl)));
     })
  |> Option.value(~default=(pad(~l, Chain.fst(mel.chain), ~r), None));
};
let unknil_ = mel => {
  let mel = distribute_paths(mel);
  let (l, r) = mel.space;
  Chain.unknil(mel.chain)
  |> Option.map(((tl, p, kid)) => {
       // todo: may want to externalize space in rightmost kid
       let tl = aggregate_paths(mk(~l, tl));
       let kid = pad(kid, ~r);
       (Some((tl, p)), kid);
     })
  |> Option.value(~default=(None, pad(~l, Chain.lst(mel.chain), ~r)));
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
    | None =>
      let ((l, r), mel) = unpad(mel);
      Chain.mk(Space.[l, r], [mel]);
    };
  let cons_space = s => Chain.map_fst(Space.cat(s));
  let snoc_space = (seg, s) => Chain.map_lst(Fun.flip(Space.cat, s), seg);
  let cat: (t, t) => t = Chain.cat(Space.cat);
  let concat = (segs: list(t)): t => List.fold_right(cat, segs, empty);
  let link = (~s=Space.empty, mel, seg) =>
    switch (is_empty(mel)) {
    | Some(s') => cons_space(Space.cat(s, s'), seg)
    | None => Chain.link(s, mel, seg)
    };
  let knil = (~s=Space.empty, seg, mel) =>
    switch (is_empty(mel)) {
    | Some(s') => snoc_space(seg, Space.cat(s', s))
    | None => Chain.knil(seg, mel, s)
    };
  let is_empty: t => bool = (==)(empty);
};

let rec is_porous = mel =>
  mel.chain
  |> Chain.to_list(
       fun
       | None => Some(Space.empty)
       | Some(kid) => is_porous(kid),
       Piece.is_porous,
     )
  |> OptUtil.sequence
  |> Option.map(spaces => {
       let ((l, r), _) = unpad(mel);
       Space.concat([l, ...spaces] @ [r]);
     });

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
    let seg_mel = Segment.of_meld(knil(tl_mel, p));
    let seg_kid = to_prefix(kid);
    Segment.cat(seg_mel, seg_kid);
  };
let rec to_suffix = (mel: t): Segment.t =>
  switch (unlink(mel)) {
  | Error(kid) =>
    switch (is_empty(kid)) {
    | Some(s) => Segment.of_space(s)
    | None => to_suffix(kid)
    }
  | Ok((kid, p, tl)) =>
    let seg_kid = to_suffix(kid);
    let seg_mel = Segment.of_meld(link(p, tl));
    Segment.cat(seg_kid, seg_mel);
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
  link(p, tl);
};
let zip_piece_r = (mel: t, p_r: Piece.t): option(t) => {
  open OptUtil.Syntax;
  let* (tl, p_l, kid) = Result.to_option(unknil(mel));
  let+ p = Piece.zip(p_l, p_r);
  assert(Option.is_some(is_empty(kid)));
  knil(tl, p);
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

let rec merge = (l: t, r: t): t => {
  let get = OptUtil.get_or_raise(Orphaned_kid);
  switch (unknil(l), unlink(r)) {
  | (Error(l), Error(r)) =>
    let l = get(is_empty(l));
    let r = get(is_empty(r));
    empty(~l, ~r, ());
  | (Error(l), Ok(_)) =>
    let l = get(is_empty(l));
    pad(~l, r);
  | (Ok(_), Error(r)) =>
    let r = get(is_empty(r));
    pad(l, ~r);
  | (Ok((tl_l, p_l, kid_l)), Ok((kid_r, p_r, tl_r))) =>
    switch (degrout(l, r)) {
    | Some(degrouted) => degrouted
    | None =>
      let get = OptUtil.get_or_raise(Invalid_prec);
      let assert_empty = kid => assert(Option.is_some(is_empty(kid)));
      switch (Piece.cmp(p_l, p_r)) {
      | In((sort, prec)) =>
        assert_empty(kid_l);
        assert_empty(kid_r);
        of_grout(~l, Grout.mk_concave(sort, prec), ~r);
      | Lt(expected) =>
        assert_empty(kid_l);
        knil(tl_l, p_l, ~kid=Some(complete(~expected, r)));
      | Eq(expected) =>
        let kid = Some(merge(kid_l, kid_r));
        append(tl_l, p_l, link(~kid, p_r, tl_r));
      | Gt(expected) =>
        assert_empty(kid_r);
        link(~kid=Some(complete(~expected, l)), p_r, tl_r);
      };
    }
  };
  // todo: aggregate paths
}
and degrout = (l: t, r: t): option(t) =>
  /**
    type degrouted =
    | Degrouted(Space.t, Space.t)
    | Replaced({d: Dir.t, replaced: Space.t, replacer: Piece.t})
    | Passed({d: Dir.t, passed: Space.t, passer: Piece.t})
   */
  {
    open OptUtil.Syntax;
    let* (tl_l, p_l, kid_l) = Result.to_option(unknil(l));
    let* (kid_r, p_r, tl_r) = Result.to_option(unlink(r));
    let* dg = Piece.degrout(p_l, p_r);
    switch (dg) {
    | Degrouted(dg_l, dg_r) =>
      let+ s_l = is_porous(kid_l)
      and+ s_r = is_porous(kid_r);
      let l = pad(tl_l, ~r=Space.cat(dg_l, s_l));
      let r = pad(~l=Space.cat(s_r, dg_r), tl_r);
      merge(l, r);
    | Replaced({d, replaced, replacer}) =>
      let+ s_l = is_porous(kid_l)
      and+ s_r = is_porous(kid_r);
      let (s_l, s_r) =
        switch (d) {
        | L => (Space.cat(s_l, replaced), s_r)
        | R => (s_l, Space.cat(replaced, s_r))
        };
      append(pad(tl_l, ~r=s_l), replacer, pad(~l=s_r, tl_r));
    | Passed({d: L, passed}) =>
      let* s_l = is_porous(kid_l);
      degrout(pad(tl_l, ~r=Space.cat(s_l, passed)), r);
    | Passed({d: R, passed}) =>
      let* s_r = is_porous(kid_r);
      degrout(l, pad(~l=Space.cat(passed, s_r), tl_r));
    };
  }
and complete = (~side: option(Dir.t)=?, ~expected: Sort.Ana.t, mel: t): t =>
  switch (side) {
  | None => complete_l(~expected, complete_r(~expected, mel))
  | Some(L) => complete_l(~expected, mel)
  | Some(R) => complete_r(~expected, mel)
  }
and complete_l = (~expected: Sort.Ana.t, mel: t): t => {
  let complete_kid = complete_kid(~expected);
  let complemented =
    List.fold_right(
      ((sugg, mold)) => merge(of_grout(Grout.mk(~sugg, mold))),
      complement(~side=L, mel),
      mel,
    );
  switch (unlink(complemented)) {
  | Ok((_, p, _)) when Piece.convexable(L, p) => complemented
  | Ok((kid, p, tl)) => link(~kid=Some(complete_kid(kid)), p, tl)
  | Error(kid) => complete_kid(kid)
  };
}
and complete_r = (~expected: Sort.Ana.t, mel: t): t => {
  let complete_kid = complete_kid(~expected);
  let complemented =
    List.fold_left(
      (mel, (sugg, mold)) => merge(mel, of_grout(Grout.mk(~sugg, mold))),
      mel,
      complement(~side=L, mel),
    );
  switch (unknil(complemented)) {
  | Ok((_, p, _)) when Piece.convexable(R, p) => complemented
  | Ok((tl, p, kid)) => knil(tl, p, ~kid=Some(complete_kid(kid)))
  | Error(kid) => complete_kid(kid)
  };
}
and complete_kid = (~expected, kid) =>
  switch (is_empty(kid)) {
  | None => kid
  | Some(s) => Grout.mk_convex(expected.sort) |> of_grout |> pad(~r=s)
  };

let merge_all = pcs => List.fold_right(merge, pcs, Padded.empty());

let cmp_mold = (_: t, _: Mold.t): option(Cmp.t) =>
  failwith("todo cmp_mold");

let merge_kids = (~expected: Sort.Ana.t, kids: list(t)) => {
  let (s, padded) =
    List.fold_right(
      (kid, (s, padded)) =>
        switch (is_empty(kid)) {
        | Some(s') => (Space.cat(s', s), padded)
        | None => (Space.empty, [pad(kid, ~r=s), ...padded])
        },
      kids,
      (Space.empty, []),
    );
  switch (padded) {
  | [] => Grout.mk_convex(expected.sort) |> of_grout |> pad(~l=s)
  | [hd, ...tl] =>
    let kids = [pad(~l=s, hd), ...tl];
    let (sort_min, prec_min) =
      kids
      |> List.fold_left(
           ((s, p), kid) => {
             // safe gets bc we padded away empty melds above
             let s = Sort.lca(s, sort(kid));
             let p = Prec.min(p, prec(kid));
             (s, p);
           },
           (Sort.root_o, Prec.max),
         );
    let kids = List.map(Option.some, kids);
    let grout =
      List.(
        init(length(kids) - 1, _ => Grout.mk_concave(sort_min, prec_min))
        |> map(Piece.of_grout)
      );
    mk(Chain.mk(kids, grout)) |> aggregate_paths;
  };
};

let rec eq_merge = (l: t, ~kid=empty(), r: t): option(t) =>
  switch (unknil(l), unlink(r)) {
  | (Error(_), Ok(_))
  | (Ok(_), Error(_)) => None
  | (Error(l), Error(r)) =>
    open OptUtil.Syntax;
    let+ l = is_empty(l)
    and+ r = is_empty(r)
    and+ kid = is_empty(kid);
    empty(~l, ~r=Space.cat(kid, r), ());
  | (Ok((tl_l, p_l, kid_l)), Ok((kid_r, p_r, tl_r))) =>
    open OptUtil.Syntax;
    let get = OptUtil.get_or_raise(Invalid_argument("eq_merge"));
    let (l, r) = (get(is_empty(kid_l)), get(is_empty(kid_r)));
    let kid = pad(~l, kid, ~r);
    switch (
      is_empty(kid),
      Piece.replaces(p_l, p_r),
      Piece.passes(p_l, p_r),
    ) {
    | (Some(s), Some(L), _) => return(append(pad(tl_l, ~r=s), p_r, tl_r))
    | (Some(s), Some(R), _) => return(append(tl_l, p_l, pad(~l=s, tl_r)))
    | (Some(s), _, Some(L)) => eq_merge(pad(tl_l, ~r=s), p_r, tl_r)
    | (Some(s), _, Some(R)) => eq_merge(tl_l, p_l, pad(~l=s, tl_r))
    | _ =>
      let+ compl = Piece.eq(p_l, p_r);
      let (hd_r, tl_r) =
        List.fold_right(
          ((sugg, mold), (p, tl)) =>
            switch (Mold.tip(R, mold)) {
            | Convex => raise(Invalid_prec)
            | Concave(s, _) =>
              let kid = Some(of_grout(Grout.mk_convex(s)));
              let g = Piece.of_grout(Grout.mk(~sugg, mold));
              (g, link(~kid, p, tl));
            },
          compl,
          (p_r, tl_r),
        );
      append(tl_l, p_l, link(~kid=Some(kid), hd_r, tl_r));
    };
  };

let rec cmp_merge = (l: t, ~kid=empty(), r: t): Cmp.s(t) => {
  let get = OptUtil.get_or_raise(Orphaned_kid);
  switch (unknil(l), unlink(r)) {
  | (Error(l), Error(r)) =>
    let l = get(is_empty(l));
    let r = Space.cat(get(is_empty(kid)), get(is_empty(r)));
    Eq(empty(~l, ~r, ()));
  | (Error(l), Ok(_)) =>
    let l = get(is_empty(l));
    Lt(pad(~l, kid));
  | (Ok(_), Error(r)) =>
    let r = get(is_empty(l));
    Gt(pad(kid, ~r));
  | (Ok((tl_l, p_l, kid_l)), Ok((kid_r, p_r, tl_r))) =>
    let kid = pad(~l=get(is_empty(kid_l)), kid, ~r=get(is_empty(kid_r)));
    switch (eq_merge((tl_l, p_l), ~kid, (p_r, tl_r))) {
    | Some(mel) => Eq(mel)
    | None =>
      Piece.lt(p_l, p_r)
        ? Lt(link(~kid=Some(kid), p_r, tl_r))
        : Gt(knil(tl_l, p_l, ~kid=Some(kid)))
    };
  };
};

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

let rec cmp_merge = (l: t, ~kid=empty(), r: t): Cmp.s(t) => {
  let get = OptUtil.get_or_raise(Orphaned_kid);
  switch (unknil(l), unlink(r)) {
  | (Error(l), Error(r)) =>
    let l = get(is_empty(l));
    let r = Space.cat(get(is_empty(kid)), get(is_empty(r)));
    Eq(empty(~l, ~r, ()));
  | (Error(l), Ok(_)) =>
    let l = get(is_empty(l));
    Lt(pad(~l, kid));
  | (Ok(_), Error(r)) =>
    let r = get(is_empty(l));
    Gt(pad(kid, ~r));
  | (Ok((tl_l, p_l, kid_l)), Ok((kid_r, p_r, tl_r))) =>
    let kid = pad(~l=get(is_empty(kid_l)), kid, ~r=get(is_empty(kid_r)));
    switch (eq_merge((tl_l, p_l), ~kid, (p_r, tl_r))) {
    | Some(mel) => Eq(mel)
    | None =>
      Piece.lt(p_l, p_r)
        ? Lt(link(~kid=Some(kid), p_r, tl_r))
        : Gt(knil(tl_l, p_l, ~kid=Some(kid)))
    };
  };
}
and eq_merge = (l: t, ~kid=empty(), r: t): option(t) => {
  let get = OptUtil.get_or_raise(Invalid_argument("Meld.cmp_merge"));
  switch (unknil(l), unlink(r)) {
  | (Error(l), Error(r)) =>
    let l = get(is_empty(l));
    let r = get(is_empty(r));
    ();
  };
};

let cmp_merge = (l: t, ~kid=empty(), r: t): Cmp.s(t) => {
  let get = OptUtil.get_or_raise(Invalid_argument("Meld.cmp_merge"));
  let (tl_l, hd_l, s_l) = get(is_closed_r(l));
  let (s_r, hd_r, tl_r) = get(is_closed_l(r));
  let kid = pad(~l=s_l, ~r=s_r, kid);
  ();
};
