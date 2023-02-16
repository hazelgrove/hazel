open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  foc: Dir.t,
  zig: option(Ziggurat.t),
};

let empty = {foc: L, zig: None};
let is_empty = sel => Option.is_none(sel.zig);

let cons_lexeme = (lx, sel, (l, r)) => {
  open Segment.Bounded;
  let zig =
    switch (sel.foc) {
    | L => bound_l(l, cons_lexeme(lx, sel.zig, r))
    | R => bound_r(snoc_lexeme(l, sel.zig, lx), r)
    };
  {...sel, zig};
};

let uncons = (~from_l, ~from_r, sel) =>
  switch (sel.foc) {
  | L => from_l(sel.zig) |> Option.map(((a, zig)) => (a, {...sel, zig}))
  | R => from_r(sel.zig) |> Option.map(((zig, a)) => (a, {...sel, zig}))
  };
let uncons_char =
  uncons(~from_l=Segment.uncons_char, ~from_r=Segment.unsnoc_char);
