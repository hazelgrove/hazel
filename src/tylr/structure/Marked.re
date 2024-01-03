[@deriving (show({with_path: false}), sexp, yojson)]
type t = (Path.Marks.t, Meld.t);

let lift_marks = (n: int, cell: Cell.t(t)): (Path.Marks.t, Cell.t(Meld.t)) =>
  switch (cell) {
  | Empty => (Path.Marks.empty, cell)
  | Full((marks, m)) => (Path.Marks.cons(n, marks), Full(m))
  };

let mk = (~l=Cell.Empty, ~r=Cell.Empty, W(toks, cells): Wald.t): t => {
  let (marks_l, l) = lift_marks(0, l);
  let (marks_r, r) = lift_marks(List.length(ps), r);
  let (marks, cells) =
    cells |> List.mapi(i => lift_marks(1 + i)) |> List.split;
  let marks = Path.Marks.union([marks_l] @ marks @ [marks_r]);
  (marks, M(l, Wald.mk(toks, cells), r));
};

let unmk = ((marks, m): t): Meld.t =>
  m |> mapi_cells(i => Cell.map(m => (Path.Marks.uncons(i, marks), m)));
