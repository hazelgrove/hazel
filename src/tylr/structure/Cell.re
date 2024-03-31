open Util;

include Meld.Cell;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Meld.Cell.t(Meld.t);

// let empty = mk();
let is_empty = c => Option.is_none(c.meld);

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

let get = ({marks, meld, _}: t) => {
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

let empty = (mold, mtrl) => mk(mold, mtrl);
let put = (mold, mtrl, m: Meld.t) => {
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
  mk(~marks, ~meld=Meld.map_cells(clear_marks, m), mold, mtrl);
};
