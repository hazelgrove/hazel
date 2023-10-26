open Sexplib.Std;

include Meld;

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Meld.t(t, Piece.t);
};
include Base;

let mk = (~l=Slot.Empty, ~r=Slot.Empty, wald) => M(l, wald, r)

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