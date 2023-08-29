open Sexplib.Std;
open Util;

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('a) =
    | M(slot('a), wald('a), slot('a))
  and wald('a) =
    | W(Chain.t('a, slot('a)))
  and slot('a) = option(t('a));
};
include Base;

module Profile = {
  type t = {
    has_tokens: bool,
    sort: Sort.t,
  };
};

// we expect kids to have higher precedence than their
// parent tips (which may be min prec in bidelim containers)
exception Invalid_prec;

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
let is_empty_ = mel => Option.is_some(is_empty(mel));

let of_piece = (~l=empty(), ~r=empty(), p: Piece.t) =>
  of_chain(Chain.mk([l, r], [p])) |> aggregate;
// let of_grout = (~l=empty(), ~id=?, ~r=empty(), g: Grout.t) =>
//   of_piece(~l, Piece.of_grout(~id?, g), ~r);
// let of_tile = (~l=empty(), ~id=?, ~r=empty(), t: Tile.t) =>
//   of_piece(~l, Piece.of_tile(~id?, t), ~r);

let root = mel =>
  mel.chain |> Option.map(Chain.links) |> Option.value(~default=[]);
let kids = mel =>
  mel.chain |> Option.map(Chain.loops) |> Option.value(~default=[]);
let length = mel => List.length(root(mel));

let sort = mel => root(mel) |> ListUtil.hd_opt |> Option.map(Piece.sort);
// precond: root(c) != []
let prec = _ => failwith("todo prec");

let rec is_porous = mel =>
  List.for_all(Piece.is_empty, root(mel))
    ? kids(mel)
      |> List.map(is_porous)
      |> OptUtil.sequence
      |> Option.map(Space.concat)
      |> Option.map(s => {
           let (l, r) = mel.space;
           Space.concat([l, s, r]);
         })
    : None;

// note: does not distribute paths
let end_piece = (~side: Dir.t, mel: t): option(Piece.t) =>
  switch (side) {
  | L =>
    Option.bind(mel.chain, Chain.unlink) |> Option.map(((_, p, _)) => p)
  | R =>
    Option.bind(mel.chain, Chain.unknil) |> Option.map(((_, p, _)) => p)
  };

let fst_id = mel => Option.map(Piece.id_, end_piece(~side=L, mel));
let lst_id = mel => Option.map(Piece.id_, end_piece(~side=R, mel));

let link = (~slot=empty(), p: Piece.t, mel) => {
  let mel = distribute(mel);
  let linked =
    switch (mel.chain) {
    | None => of_piece(~l=kid, p)
    | Some(c) => of_chain(Chain.link(kid, p, c))
    };
  aggregate(linked);
};
let knil = (~slot=empty(), mel, p: Piece.t) => {
  let mel = distribute(mel);
  let linked =
    switch (mel.chain) {
    | None => of_piece(p, ~r=kid)
    | Some(c) => of_chain(Chain.knil(c, p, kid))
    };
  aggregate(linked);
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

let split_piece = (n, mel) =>
  try({
    let c = Option.get(distribute(mel).chain);
    let (l, p, r) = Chain.split_nth_link(n, c);
    (aggregate(of_chain(l)), p, aggregate(of_chain(r)));
  }) {
  | Invalid_argument(_) => raise(Invalid_argument("Meld.split_piece"))
  };

// let zip_piece_l = (p_l: Piece.t, mel: t): option(t) => {
//   open OptUtil.Syntax;
//   let* (kid, p_r, tl) = Result.to_option(unlink(mel));
//   let+ p = Piece.zip(p_l, p_r);
//   assert(Option.is_some(is_empty(kid)));
//   link(p, tl);
// };
// let zip_piece_r = (mel: t, p_r: Piece.t): option(t) => {
//   open OptUtil.Syntax;
//   let* (tl, p_l, kid) = Result.to_option(unknil(mel));
//   let+ p = Piece.zip(p_l, p_r);
//   assert(Option.is_some(is_empty(kid)));
//   knil(tl, p);
// };
