open Sexplib.Std;
// open Util;

module Cell = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('meld) = {
    marks: Path.Marks.t,
    meld: option('meld),
  };
  let mk = (~marks=Path.Marks.empty, ~meld=?, ()) => {marks, meld};
  let empty = mk();
  // let empty = {marks: Path.Marks.empty, dims: Dims.zero, meld: None};
  // let full = m => {marks: Path.Marks.empty, meld: Some(m)};
};

module Wald = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('cell) =
    | W(Chain.t(Token.t, 'cell));
  let mk = (toks: list(_), cells: list(Cell.t(_))) =>
    W(Chain.mk(toks, cells));
  let unit = tok => mk([tok], []);
  let face = (~side=Dir.L, W(w): t(_)) => {
    let tok = Dir.pick(side, (Chain.fst, Chain.lst), w);
    (tok.mtrl, tok.mold);
  };
  // let dims = (W(w)) =>
  //   w |> Chain.to_list(Token.dims, c => Cell.(c.dims)) |> Dims.sum;
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | M(Cell.t(t), Wald.t(Cell.t(t)), Cell.t(t));

let mk = (~l=Cell.empty, ~r=Cell.empty, w) => M(l, w, r);

let length = (M(_, W(w), _): t) => Chain.length(w);

let is_empty =
  fun
  | M({meld: None, _}, W(([tok], [])), {meld: None, _}) =>
    Token.is_empty(tok)
  | _ => false;

let to_chain = (M(l, W((ts, cs)), r): t) => ([l, ...cs] @ [r], ts);

let fold = (f_hd, f_tl, m: t) => Chain.fold_left(f_hd, f_tl, to_chain(m));

module Space = {
  let mk = (tok: Token.t) => {
    assert(Mtrl.is_space(tok.mtrl));
    mk(Wald.unit(tok));
  };
  let get =
    fun
    | M(_, W(([tok], [])), _) when Token.is_space(tok) => Some(tok)
    | _ => None;
};
module Grout = {
  let op_ = (s: Mtrl.Sorted.t) => mk(Wald.unit(Token.Grout.op_(s)));
};

// let get_spaces = ms =>
//   ms
//   |> List.map(get_space)
//   |> OptUtil.sequence
//   |> Option.map(String.concat(""));

// let mk = (~l=Cell.empty, ~r=Cell.empty, w) => M(l, w, r);
[@warning "-27-39"]
let rec mk_grout = (~l=false, ~r=false, sort: Mtrl.Sorted.t) => {
  // let c_l = l ? Cell.full(mk_grout(Grout)) : Cell.empty;
  // let c_r = r ? Cell.full(mk_grout(Grout)) : Cell.empty;
  // mk(~l=c_l, Wald.of_tok(Token.mk_grout(~l, sort, ~r)), ~r=c_r);
  failwith(
    "todo: make full molded sorts for Cell.full calls above",
  );
};
// let of_tok = tok => mk(Wald.of_tok(tok));

let to_chain = (M(l, W((toks, cells)), r)) => (
  [l, ...cells] @ [r],
  toks,
);
let unzip = (n, m) => Chain.unzip_nth(n, to_chain(m));

let link = (~cell=Cell.empty, t: Token.t, M(l, W(w), r): t) =>
  M(cell, W(Chain.link(t, l, w)), r);

let rev = (M(l, W(w), r): t) => M(r, W(Chain.rev(w)), l);

let face = (~side: Dir.t, M(_, w, _)) => Wald.face(~side, w);

let is_space =
  fun
  | M(_, W(([{mtrl: Space, _} as tok], [])), _) => Some(tok)
  | _ => None;

let map_cells = (f, M(l, W((toks, cells)), r)) => {
  let (l, r) = (f(l), f(r));
  let cells = List.map(f, cells);
  M(l, W((toks, cells)), r);
};

// let singleton = (~l=Cell.empty, ~r=Cell.empty, t) =>
//   mk(~l, W(Chain.unit(t)), ~r);

// let of_piece = (~l=empty(), ~r=empty(), p: Piece.t) =>
//   of_chain(Chain.mk([l, r], [p])) |> aggregate;

// let root = mel =>
//   mel.chain |> Option.map(Chain.links) |> Option.value(~default=[]);
// let kids = mel =>
//   mel.chain |> Option.map(Chain.loops) |> Option.value(~default=[]);
// let length = mel => List.length(root(mel));

// let sort = mel => root(mel) |> ListUtil.hd_opt |> Option.map(Piece.sort);
// let prec = _ => failwith("todo prec");

// let fst_id = mel => Option.map(Piece.id_, end_piece(~side=L, mel));
// let lst_id = mel => Option.map(Piece.id_, end_piece(~side=R, mel));

// let link = (~slot=None, a, M(l, W(w), r)) =>
//   M(slot, W(Chain.link(a, l, w)), r);

// // left to right: l w r a slot
// let knil = (~slot=None, M(l, W(w), r), a) =>
//   M(l, W(Chain.knil(w, r, a)), slot);

// let unlink = (M(l, W(w), r)) =>
//   Chain.unlink(w)
//   |> Result.map(~f=((a, slot, tl)) => (l, a, (slot, W(tl), r)))
//   |> Result.map_error(~f=a => (l, a, r));
