open Util;

include Meld.Cell;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Meld.Cell.t(Meld.t);

// let empty = mk();
let is_empty = c => Option.is_none(c.meld);

let face = (~side: Dir.t, c: t) =>
  switch (c.meld) {
  | None => Space.Molded.t
  | Some(m) => Meld.face(~side, m)
  };

// let has_space = (cell: t) =>
//   switch (cell.meld) {
//   | Some(M(_, W(([tok], [])), _)) when Token.is_space(tok) => true
//   | _ => false
//   };

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

// let empty = () => mk();
let put = (m: Meld.t) =>
  if (Meld.is_empty(m)) {
    empty;
  } else {
    let M(l, W((toks, cells)), r) = m;
    let n = List.length(toks);
    let marks = {
      open Path.Marks;
      let l = cons(0, l.marks);
      let r = cons(n, r.marks);
      let mid = cells |> List.mapi((i, cell) => cons(i + 1, cell.marks));
      union_all([l, ...mid] @ [r]);
    };
    mk(~marks, ~meld=Meld.map_cells(clear_marks, m), ());
  };

module Space = {
  let get = (c: t) =>
    switch (get(c)) {
    | None => Some(Token.Space.empty)
    | Some(m) => Meld.Space.get(m)
    };
};
