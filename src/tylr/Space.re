open Sexplib.Std;
open Util;

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
  // todo: utf
  let of_char =
    fun
    | '\r'
    | '\n' => mk(Newline)
    | ' '
    | '\t' => mk(Space)
    | c =>
      raise(Invalid_argument("Space.Char.of_char: " ++ String.make(1, c)));
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

let map = (f: Char.t => _, s) => List.map(f, s.chars);

let add_paths = (ps, s) => {...s, paths: ps @ s.paths};
let clear_paths = s => {...s, paths: []};

let cat = (l: t, r: t) =>
  mk(
    ~paths=l.paths @ List.map(Path.shift(length(l)), r.paths),
    l.chars @ r.chars,
  );
let concat = ss => List.fold_right(cat, ss, empty);

let to_string = s =>
  s.chars |> List.map(Char.to_string) |> String.concat("");
let of_string = s => {
  let chars = ref([]);
  let i = ref(0);
  while (i^ < String.length(s)) {
    switch (s.[i^], s.[i^ + 1]) {
    | ('\r', '\n') =>
      chars := [Char.mk(Newline), ...chars^];
      i := i^ + 2;
    | (c, _) =>
      chars := [Char.of_char(c), ...chars^];
      i := i^ + 1;
    };
  };
  mk(List.rev(chars^));
};

let uncons = (~char=false, s: t) =>
  switch (s.chars) {
  | [] => None
  | [hd, ...tl] when char =>
    let (ps_hd, ps_tl) =
      s.paths |> List.partition_map(p => p < 1 ? Left(p) : Right(p - 1));
    Some(({paths: ps_hd, chars: [hd]}, {paths: ps_tl, chars: tl}));
  | [_, ..._] => Some((s, empty))
  };
let unsnoc = (~char=false, s: t) =>
  ListUtil.split_last_opt(s.chars)
  |> Option.map(((tl, hd)) =>
       if (char) {
         let (ps_hd, ps_tl) =
           s.paths
           |> List.partition_map(p =>
                p > length(s) - 1 ? Left(p) : Right(p)
              );
         ({paths: ps_tl, chars: tl}, {paths: ps_hd, chars: [hd]});
       } else {
         (empty, s);
       }
     );

let split = (n, s) => {
  let (ps_l, ps_r) =
    s.paths
    |> List.partition_map(col => col <= n ? Left(col) : Right(col - n));
  let (l, r) = ListUtil.split_n(n, s.chars);
  (mk(~paths=ps_l, l), mk(~paths=ps_r, r));
};

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
