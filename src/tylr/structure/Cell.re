open Util;

include Meld.Cell;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Meld.Cell.t(Meld.t);

let mk = (~marks=Path.Marks.empty, ~meld=?, ()): t => {
  let dims =
    meld |> Option.map(Meld.dims) |> Option.value(~default=Dims.zero);
  {marks, dims, meld};
};
let empty = mk();
let is_empty = (==)(empty);

let cursor = mk(~marks=Path.Marks.cursor, ());

let has_space = (cell: t) =>
  switch (cell.meld) {
  | Some(M(_, W(([tok], [])), _)) when Token.is_space(tok) => true
  | _ => false
  };

let add_marks = (marks, cell) => {
  ...cell,
  marks: Path.Marks.union(marks, cell.marks),
};
let clear_marks = cell => {...cell, marks: Path.Marks.empty};

let get = ({marks, meld, dims: _}: t) => {
  open OptUtil.Syntax;
  let+ Meld.M(l, W((toks, cells)), r) = meld;
  let n = List.length(toks);
  let l = l |> add_marks(Path.Marks.peel(0, marks));
  let cells =
    cells
    |> List.mapi((i, cell) =>
         cell |> add_marks(Path.Marks.peel(i + 1, marks))
       );
  let r = r |> add_marks(Path.Marks.peel(n, marks));
  Meld.M(l, W((toks, cells)), r);
};

let put = (m: Meld.t) => {
  let M(l, W((toks, cells)), r) = m;
  let n = List.length(toks);
  let marks =
    Path.Marks.(
      union_all(
        [
          cons(0, l.marks),
          ...cells |> List.mapi((i, cell) => cons(i + 1, cell.marks)),
        ]
        @ [cons(n, r.marks)],
      )
    );
  mk(~marks, ~meld=Meld.map_cells(clear_marks, m), ());
};

let rec pad = (~side as d: Dir.t, ~pad as p: t, cell: t) =>
  switch (get(cell)) {
  | None => add_marks(cell.marks, p)
  | Some(M(l, w, r)) =>
    let (c_d, c_b) = Dir.order(d, (l, r));
    let c_d = pad(~side=d, ~pad=p, c_d);
    let (l, r) = Dir.order(d, (c_d, c_b));
    put(M(l, w, r));
  };

let face = (~side: Dir.t, cell: t) =>
  cell.meld
  |> Option.map(Meld.face(~side))
  |> Option.value(~default=Molded.Label.space);
