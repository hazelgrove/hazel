module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t('t) =
    | Space
    | Grout
    | Tile('t);
};
include Base;

let tile = t => Tile(t);

let is_space =
  fun
  | Space => true
  | Grout
  | Tile(_) => false;
let is_grout =
  fun
  | Grout => true
  | Space
  | Tile(_) => false;

module Labeled = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Base.t(Label.t);
  module Map =
    Map.Make({
      type nonrec t = t;
      let compare = compare;
    });
};
module Sorted = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Base.t(Sort.t);
  let root = Tile(Sort.root);
  module Map =
    Map.Make({
      type nonrec t = t;
      let compare = compare;
    });
};

module T = Labeled;
module NT = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Padded.t(Sorted.t);
  let space = Padded.mk(Space);
  let grout = Padded.mk(Grout);
};
module Sym = {
  include Sym;
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Sym.t(T.t, NT.t);
};

module Regex = {
  include Regex;
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Regex.t(Sym.t);
};
// module RFrame = {
//   include RFrame;
//   [@deriving (show({with_path: false}), sexp, yojson, ord)]
//   type t = RFrame(Sym.t);
// };

module Grammar = {
  type t = Sorted.Map.t(Prec.Table.t(Regex.t));

  let mtrlize_tiles = (g: Grammar.t): t =>
    Sort.Map.to_seq(g)
    |> Seq.map(((s, tbl)) => {
         let mtbl =
           tbl
           |> Prec.Table.map(
                Regex.map(Sym.map(tile, ((pad, s)) => (pad, tile(s)))),
              );
         (Tile(s), mtbl);
       })
    |> Sorted.Map.of_seq;
  // intention here is, given grammar in operator form (no consecutive NTs),
  // insert a space-mtrl NT between any consecutive NTs and before/following
  // any Ts at the left/right end of each prec level.
  // currently assumes input grammar is in alternating form (neither consecutive
  // NTs nor Ts) and only inserts space NTs at the ends of prec levels as needed.
  let mtrlize_space: t => t =
    Sorted.Map.map(
      Prec.Table.map(rgx => {
        let (ls, rs) = RZipper.(enter(~from=L, rgx), enter(~from=R, rgx));
        let exists_t =
          List.exists(
            fun
            | Bound.Root => false
            | Node((sym, _)) => Sym.is_t(sym),
          );
        let spc = Regex.atom(Sym.nt(Padded.mk(~indent=false, Space)));
        rgx
        |> (exists_t(ls) ? Regex.push(~from=L, spc) : Fun.id)
        |> (exists_t(rs) ? Regex.push(~from=R, spc) : Fun.id);
      }),
    );
  let mtrlize = (g: Grammar.t): t =>
    // grout not explicitly materialized in grammar form,
    // instead generated as needed when baking walks
    g |> mtrlize_tiles |> mtrlize_space;

  let v = mtrlize(Grammar.v);
};
