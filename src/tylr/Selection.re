[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  foc: Dir.t,
  seg: Segment.t,
};

let empty = {foc: L, seg: Segment.empty};

let is_empty = sel => Segment.is_empty(sel.seg);

let cons = (~onto_l, ~onto_r, a, sel) =>
  switch (sel.foc) {
  | L => {...sel, seg: onto_l(a, sel.seg)}
  | R => {...sel, seg: onto_r(sel.seg, a)}
  };
let cons_lexeme =
  cons(
    ~onto_l=(lx, seg) => Segment.Bounded.cons_lexeme(lx, seg, None),
    ~onto_r=(seg, lx) => Segment.Bounded.snoc_lexeme(None, seg, lx),
  );

let uncons = (~from_l, ~from_r, sel) =>
  switch (sel.foc) {
  | L => from_l(sel.seg) |> Option.map(((a, seg)) => (a, {...sel, seg}))
  | R => from_r(sel.seg) |> Option.map(((seg, a)) => (a, {...sel, seg}))
  };
let uncons_char =
  uncons(~from_l=Segment.uncons_char, ~from_r=Segment.unsnoc_char);
