open Sexplib.Std;

include Meld;

module Cell = {
  type t('a) = {
    marks: EPath.Marks.t,
    content: option('a),
  };
  let mk = (~marks=EPath.Marks.empty, content) => {marks, content};
  let empty = mk(None);
};

type t = Meld.t(Cell.t(t), EToken.t);

let mk = (~l=Cell.empty, ~r=Cell.empty, w) => M(l, w, r);
let singleton = (~l=Cell.empty, ~r=Cell.empty, t) =>
  mk(~l, Wald.singleton(t), ~r);

module Marked = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = EPath.Marked.t(Base.t);

  let lift_marks = (n: int, slot: Slot.t(t)): (EPath.Marks.t, Slot.t(Base.t)) =>
    switch (slot) {
    | Empty => (EPath.Marks.empty, slot)
    | Full((marks, m)) => (EPath.Marks.cons(n, marks), Full(m))
    };

  let mk = (~l=Slot.Empty, ~r=Slot.Empty, W(ps, slots): Wald.t(Piece.t, t)) => {
    let (marks_l, l) = lift_marks(0, l);
    let (marks_r, r) = lift_marks(List.length(ps), r);
    let (marks, slots) =
      slots |> List.mapi(i => lift_marks(1 + i)) |> List.split;
    let marks = EPath.Marks.union([marks_l] @ marks @ [marks_r]);
    // todo: add ghost pieces in ps
    (marks, M(l, Wald.mk(ps, slots), r));
  };

  let unmk = ((marks, m): t): Meld.t(t, Piece.t) =>
    m
    |> mapi_slots(i =>
      Slot.map(m => (EPath.Marks.uncons(i, marks), m))
    );
};