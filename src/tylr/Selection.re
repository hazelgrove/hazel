[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  foc: Dir.t,
  seg: Segment.t,
};

let empty = {foc: L, seg: Segment.empty};

let is_empty = sel => Segment.is_empty(sel.seg);

let pop_char = _ => failwith("todo pop_char");
let push_char = (_, _) => failwith("todo push_char");
