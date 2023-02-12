[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  foc: Dir.t,
  seg: Arch.t,
};

let empty = {foc: L, seg: Arch.empty};

let is_empty = sel => Arch.is_empty(sel.seg);

let cons_lexeme = (lx, sel, (l, r)) => {
  open Segment.Bounded;
  let seg =
    switch (sel.foc) {
    | L => bound_l(l, cons_lexeme(lx, sel.seg, r))
    | R => bound_r(snoc_lexeme(l, sel.seg, lx), r)
    };
  {...sel, seg};
};

let uncons = (~from_l, ~from_r, sel) =>
  switch (sel.foc) {
  | L => from_l(sel.seg) |> Option.map(((a, seg)) => (a, {...sel, seg}))
  | R => from_r(sel.seg) |> Option.map(((seg, a)) => (a, {...sel, seg}))
  };
let uncons_char =
  uncons(~from_l=Segment.uncons_char, ~from_r=Segment.unsnoc_char);
