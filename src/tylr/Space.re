open Sexplib.Std;
// open Util;

module Char = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type shape =
    | Space
    | Newline;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    id: Id.t,
    shape,
  };
  let mk = shape => {
    let id = Id.Gen.next();
    {id, shape};
  };
  let to_string = c =>
    switch (c.shape) {
    | Space => " "
    | Newline => "\n"
    };
};

module Path = {
  // number of spaces to left of cursor
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = int;
  let shift = (n, p) => p + n;
};
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  chars: list(Char.t),
  paths: list(Path.t),
};

let mk = (~paths=[], chars) => {paths, chars};
let empty = mk([]);
let is_empty = (s: t) => s.chars == [];
let length = (s: t) => List.length(s.chars);

let add_paths = (ps, s) => {...s, paths: ps @ s.paths};
let clear_paths = s => {...s, paths: []};

let cat = (l: t, r: t) =>
  mk(
    ~paths=l.paths @ List.map(Path.shift(length(l)), r.paths),
    l.chars @ r.chars,
  );

// let split_newlines = (ss: s): Chain.t(s, t) =>
//   List.fold_right(
//     (s, split) =>
//       switch (s.shape) {
//       | Newline => Chain.link(empty, s, split)
//       | _ => Chain.map_fst(List.cons(s), split)
//       },
//     ss,
//     Chain.of_loop(empty),
//   );

// let newline_length = ss =>
//   ss
//   |> List.filter(
//        fun
//        | Newline => true
//        | _ => false,
//      )
//   |> List.length;

// let unzip = (step: Step.t, ss: s): (s, s) => ListUtil.split_n(step, ss);

// let is_cursor = (_: t) => failwith("todo split_cursor");
