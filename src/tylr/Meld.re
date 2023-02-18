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
    List.partition_map(
      p =>
        switch (Path.with_kid(kid, p)) {
        | Some(p) => Left(p)
        | None => Right(p)
        },
      ps,
    );
  let with_piece = (ps: t, index: int) =>
    List.partition_map(
      p =>
        switch (Path.with_piece(index, p)) {
        | Some(p) => Left(p)
        | None => Right(p)
        },
      ps,
    );
  let with_space = (ps: t, side: Dir.t) =>
    List.partition_map(
      p =>
        switch (Path.with_space(side, p)) {
        | Some(p) => Left(p)
        | None => Right(p)
        },
      ps,
    );
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
let of_chain = (~l=Space.empty, ~r=Space.empty, ~paths=[], c) =>
  mk(~l, ~r, ~paths, Some(c));
let empty = (~l=Space.empty, ~r=Space.empty, ~paths=[], ()) =>
  mk(~l, ~r, ~paths, None);

let map_space = (f, mel) => {...mel, space: f(mel.space)};
let put_space = ss => map_space(_ => ss);

let map_paths = (f, mel) => {...mel, paths: f(mel.paths)};
let put_paths = ps => map_paths(_ => ps);
let add_paths = ps => map_paths((@)(ps));
let clear_paths = map_paths(_ => []);

let map_chain = (f, mel) =>
  switch (mel.chain) {
  | None => mel
  | Some(c) => {...mel, chain: Some(f(c))}
  };
let bind_chain = (f, mel) =>
  switch (mel.chain) {
  | None => mel
  | Some(c) => f(c)
  };

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
let unpad_ = (side, mel) => {
  let (this, that) = Dir.order(side, mel.space);
  let (with_s, paths) = Paths.with_space(mel.paths, side);
  let this = Space.add_paths(with_s, this);
  let space = Dir.unorder(side, (Space.empty, that));
  (this, {...mel, space, paths});
};
let unpad = mel => {
  let (l, mel) = unpad_(L, mel);
  let (r, mel) = unpad_(R, mel);
  ((l, r), mel);
};

let distribute_paths = mel => {
  let ((l, r), mel) = unpad(mel);
  let ps = mel.paths;
  {...mel, space: (l, r), paths: []}
  |> map_chain(
       Chain.mapi(
         i => add_paths(fst(Paths.with_kid(ps, i))),
         j => Piece.add_paths(fst(Paths.with_piece(ps, j))),
       ),
     );
};
let distribute_space = mel => {
  let (l, r) = mel.space;
  mel
  |> bind_chain(c => {
       assert(mel.paths == []);
       c
       |> Chain.map_fst(kid => pad(~l, kid))
       |> Chain.map_lst(kid => pad(kid, ~r))
       |> of_chain;
     });
};
let distribute = mel =>
  // order important
  mel |> distribute_paths |> distribute_space;

let aggregate_paths = mel => {
  let (l, r) = mel.space;
  let ps_l = List.map(Path.of_space(L), l.paths);
  let ps_r = List.map(Path.of_space(R), r.paths);
  let ps_c =
    switch (mel.chain) {
    | None => []
    | Some(c) =>
      c
      |> Chain.mapi(
           (i, kid) => List.map(Path.cons(i), kid.paths),
           (i, p: Piece.t) => List.map(Path.of_piece(i), p.paths),
         )
      |> Chain.to_list(Fun.id, Fun.id)
      |> List.concat
    };

  let paths = List.concat([ps_l, ps_c, ps_r]);
  let (l, r) = Space.(clear_paths(l), clear_paths(r));
  mel.chain
  |> Option.map(Chain.map(clear_paths, Piece.clear_paths))
  |> mk(~l, ~r, ~paths);
};
let aggregate_space = mel =>
  mel
  |> bind_chain(c => {
       let (l, r) = mel.space;
       let (l', fst) = unpad_(L, Chain.fst(c));
       let c = Chain.put_fst(fst, c);
       let (r', lst) = unpad_(R, Chain.lst(c));
       let c = Chain.put_lst(lst, c);
       let space = Space.(cat(l, l'), cat(r', r));
       {...mel, space, chain: Some(c)};
     });
let aggregate = mel => mel |> aggregate_paths |> aggregate_space;

let is_empty = (mel: t) =>
  switch (mel.chain) {
  | Some(_) => None
  | None =>
    let (l, r) = distribute(mel).space;
    Some(Space.cat(l, r));
  };

let of_piece = (~l=empty(), ~r=empty(), p: Piece.t) =>
  of_chain(Chain.mk([l, r], [p])) |> aggregate;
let of_grout = (~l=empty(), ~r=empty(), g: Grout.t) =>
  of_piece(~l, Piece.of_grout(g), ~r);
let of_tile = (~l=empty(), ~r=empty(), t: Tile.t) =>
  of_piece(~l, Piece.of_tile(t), ~r);

let root = mel =>
  mel.chain |> Option.map(Chain.links) |> Option.value(~default=[]);
let kids = mel =>
  mel.chain |> Option.map(Chain.loops) |> Option.value(~default=[]);
let length = mel => List.length(root(mel));

// note: does not distribute paths
let end_piece = (~side: Dir.t, mel: t): option(Piece.t) =>
  switch (side) {
  | L =>
    Option.bind(mel.chain, Chain.unlink) |> Option.map(((_, p, _)) => p)
  | R =>
    Option.bind(mel.chain, Chain.unknil) |> Option.map(((_, p, _)) => p)
  };

let fst_id = mel => Option.map(Piece.id, end_piece(~side=L, mel));
let lst_id = mel => Option.map(Piece.id, end_piece(~side=R, mel));

let link = (~kid=empty(), p: Piece.t, mel) => {
  let mel = distribute(mel);
  let linked =
    switch (mel.chain) {
    | None => of_piece(~l=kid, p)
    | Some(c) => of_chain(Chain.link(kid, p, c))
    };
  aggregate(linked);
};
let knil = (~kid=empty(), mel, p: Piece.t) => {
  let mel = distribute(mel);
  let linked =
    switch (mel.chain) {
    | None => of_piece(p, ~r=kid)
    | Some(c) => of_chain(Chain.knil(c, p, kid))
    };
  aggregate(linked);
};

let complement = (~side: Dir.t, mel: t) =>
  switch (end_piece(~side, mel)) {
  | None => []
  | Some(p) => Piece.complement(~side, p)
  };

type unlinked = Result.t((t, Piece.t, t), option(t));
let unlink = (mel: t): unlinked =>
  switch (distribute(mel).chain) {
  | None => Error(None)
  | Some(c) =>
    Chain.unlink(c)
    |> Option.map(((kid, p, tl)) => {
         let tl = aggregate(of_chain(tl));
         (kid, p, tl);
       })
    |> Result.of_option(~error=Some(Chain.fst(c)))
  };
type unkniled = Result.t((t, Piece.t, t), option(t));
let unknil = mel =>
  switch (distribute(mel).chain) {
  | None => Error(None)
  | Some(c) =>
    Chain.unknil(c)
    |> Option.map(((tl, p, kid)) => {
         let tl = aggregate(of_chain(tl));
         (tl, p, kid);
       })
    |> Result.of_option(~error=Some(Chain.lst(c)))
  };

let complete_empty = (~space, ~paths, ~expected: Sort.Ana.t) =>
  of_grout(Grout.mk_convex(expected.sort))
  |> put_space(space)
  |> put_paths(paths)
  |> aggregate;

let rec complete = (~side: option(Dir.t)=?, ~expected: Sort.Ana.t, mel: t): t =>
  switch (side) {
  | None => complete_l(~expected, complete_r(~expected, mel))
  | Some(L) => complete_l(~expected, mel)
  | Some(R) => complete_r(~expected, mel)
  }
and complete_l = (~expected: Sort.Ana.t, mel: t): t => {
  let mel = distribute(mel);
  switch (mel.chain) {
  | None => complete_empty(~space=mel.space, ~paths=mel.paths, ~expected)
  | Some(c) =>
    switch (end_piece(~side=L, mel)) {
    | None => complete_l(~expected, Chain.fst(c))
    | Some(p) =>
      c
      |> List.fold_right(
           ((sugg, mold), c) => {
             let g = Piece.of_grout(Grout.mk(~sugg, mold));
             let expected = Mold.expected(~side=R, mold);
             Chain.link(empty(), g, Chain.map_fst(complete(~expected), c));
           },
           Piece.complement(~side=L, p),
         )
      |> of_chain
      |> aggregate
    }
  };
}
and complete_r = (~expected: Sort.Ana.t, mel: t): t => {
  let mel = distribute(mel);
  switch (mel.chain) {
  | None => complete_empty(~space=mel.space, ~paths=mel.paths, ~expected)
  | Some(c) =>
    switch (end_piece(~side=R, mel)) {
    | None => complete_r(~expected, Chain.lst(c))
    | Some(p) =>
      Piece.complement(~side=R, p)
      |> List.fold_left(
           (c, (sugg, mold)) => {
             let g = Piece.of_grout(Grout.mk(~sugg, mold));
             let expected = Mold.expected(~side=R, mold);
             Chain.knil(Chain.map_lst(complete(~expected), c), g, empty());
           },
           c,
         )
      |> of_chain
      |> aggregate
    }
  };
};

let rec to_lexemes = (mel): Lexeme.s => {
  let (l, r) = mel.space;
  let (l, r) = Lexeme.(S(l), S(r));
  (
    switch (mel.chain) {
    | None => []
    | Some(c) => Chain.to_list(to_lexemes, p => [Lexeme.of_piece(p)], c)
    }
  )
  |> List.cons([l])
  |> Fun.flip((@), [[r]])
  |> List.concat;
};

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

// precond: root(c) != []
let sort = mel => {
  let (_, p, _) =
    Result.to_option(unlink(mel))
    |> OptUtil.get_or_raise(Invalid_argument("Meld.sort"));
  Piece.sort(p);
};
// precond: root(c) != []
let prec = _ => failwith("todo prec");

let mold =
    (mel: t, ~kid: option(Sort.o)=?, t: Token.t): Result.t(Mold.t, Sort.o) => {
  open Result.Syntax;
  // todo: possibly join with kid sort
  let error = sort(mel);
  let* tip = Result.of_option(~error, tip(R, mel));
  switch (tip) {
  | Tip.Convex => Error(error)
  | Concave(sort, _) =>
    Result.of_option(~error, LangUtil.mold_of_token(kid, sort, t))
  };
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
