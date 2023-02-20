// open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  foc: Dir.t,
  seg: Segment.t,
};

let empty = {foc: L, seg: Segment.empty};
let is_empty = sel => Segment.is_empty(sel.seg);

let map_seg = (f, sel) => {...sel, seg: f(sel.seg)};

let uncons = (~from_l, ~from_r, sel) =>
  switch (sel.foc) {
  | L => from_l(sel.seg) |> Option.map(((a, seg)) => (a, {...sel, seg}))
  | R => from_r(sel.seg) |> Option.map(((seg, a)) => (a, {...sel, seg}))
  };
let uncons_char =
  uncons(
    ~from_l=Segment.pull_lexeme(~char=true),
    ~from_r=Segment.llup_lexeme(~char=true),
  );
let uncons_lexeme =
  uncons(
    ~from_l=Segment.pull_lexeme(~char=false),
    ~from_r=Segment.llup_lexeme(~char=false),
  );
