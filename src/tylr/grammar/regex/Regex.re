open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t('a) =
  | Atom('a)
  | Star(t('a))
  | Seq(s('a))
  | Alt(s('a))
and s('a) = list(t('a));

let atom = a => Atom(a);
let star = r => Star(r);
let seq = rs => Seq(rs);
let alt = rs => Alt(rs);

let eps = Seq([]);
let opt = r => Alt([eps, r]);

let rec flatten =
  fun
  | Seq(rs) => List.concat_map(flatten, rs)
  | r => [r];

let push = (~from: Dir.t, r) =>
  fun
  | Seq(rs) => Seq(from == L ? [r, ...rs] : rs @ [r])
  | (Atom(_) | Star(_) | Alt(_)) as r' =>
    Seq(from == L ? [r, r'] : [r', r]);

let rec atoms =
  fun
  | Atom(a) => [a]
  | Star(r) => atoms(r)
  | Seq(rs)
  | Alt(rs) => List.concat_map(atoms, rs);

let rec map = f =>
  fun
  | Atom(a) => Atom(f(a))
  | Star(r) => Star(map(f, r))
  | Seq(rs) => Seq(List.map(map(f), rs))
  | Alt(rs) => Alt(List.map(map(f), rs));

let rec fold =
        (
          ~atom: 'a => 'acc,
          ~star: 'acc => 'acc,
          ~seq: list('acc) => 'acc,
          ~alt: list('acc) => 'acc,
        )
        : (t('a) => 'acc) => {
  let fold = fold(~atom, ~star, ~seq, ~alt);
  fun
  | Atom(a) => atom(a)
  | Star(r) => star(fold(r))
  | Seq(rs) => seq(List.map(fold, rs))
  | Alt(rs) => alt(List.map(fold, rs));
};

let nullable = r =>
  r
  |> fold(
       // assuming all atoms are non-nullable
       // but could change this in future
       ~atom=_ => false,
       ~star=_ => true,
       ~seq=List.for_all(Fun.id),
       ~alt=List.exists(Fun.id),
     );

// currently assuming:
// (1) no consecutive kids
// (2) no consecutive tokens
// (3) every sort is convex
// only (1) fundamentally necessary
exception Ill_typed;

// let tok = (lbl: Label.t) => Atom(Atom.Tok(lbl));
// let tokc = (t: Token.t) => tok(Const(t));
// let kid = s => Atom(Atom.Kid(s));
