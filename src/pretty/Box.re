open Util;

[@deriving sexp]
type t('annot) =
  | Text(string)
  | HBox(list(t('annot)))
  | VBox(list(t('annot)))
  | Annot('annot, t('annot));

module Make = (MemoTbl: MemoTbl.S) => {
  let height_tbl: MemoTbl.t(t(unit), int) = MemoTbl.mk();
  let rec height = (box: t('annot)) =>
    switch (MemoTbl.get(height_tbl, Obj.magic(box))) {
    | Some(h) => h
    | None =>
      let h =
        switch (box) {
        | Text(_) => 1
        | Annot(_, b) => height(b)
        | HBox(bs) => bs |> List.map(height) |> List.fold_left(max, 1) // Note: 1 is HBox([]) height
        | VBox(bs) => bs |> List.map(height) |> List.fold_left((+), 0)
        };
      MemoTbl.set(height_tbl, Obj.magic(box), h);
      h;
    };

  // Note: annots are inside-out (i.e. List.hd(annots) is the inner-most annot)
  let rec annot = (annots: list('annot), box: t('annot)): t('annot) => {
    switch (annots) {
    | [] => box
    | [ann, ...anns] => annot(anns, Annot(ann, box))
    };
  };

  let rec append_box =
          (~annots: list('annot)=[], box1: t('annot), box2: t('annot))
          : t('annot) =>
    if (height(box1) <= 1) {
      HBox([annot(annots, box1), box2]);
    } else {
      let rec append_last = (bs1: list(t('annot))): list(t('annot)) => {
        switch (bs1) {
        | [] => failwith("impossible due to `box_height` guard")
        | [b1] => [append_box(~annots, b1, box2)]
        | [b1, ...bs1] => [annot(annots, b1), ...append_last(bs1)]
        };
      };
      switch (box1) {
      | Text(_) => failwith("impossible due to `box_height` guard")
      | HBox(bs1) => HBox(append_last(bs1))
      | VBox(bs1) => VBox(append_last(bs1))
      | Annot(annot, b) => append_box(~annots=[annot, ...annots], b, box2)
      };
    };

  let append_hbox = (boxes1: list(t('annot)), boxes2: list(t('annot))) => {
    switch (ListUtil.split_last_opt(boxes1)) {
    | None => boxes2
    | Some((leading, last)) => leading @ [append_box(last, HBox(boxes2))]
    };
  };

  let table: MemoTbl.t(Layout.t(unit), t(unit)) = MemoTbl.mk();
  let mk = (l: Layout.t('annot)): t('annot) => {
    let mk = (boxes: list(list(t(_)))) =>
      VBox(List.map(row => HBox(row), boxes));
    let rec go = (l: Layout.t(_)) => {
      switch (MemoTbl.get(table, Obj.magic(l))) {
      | Some(box) => Obj.magic(box)
      | None =>
        let box =
          switch (l) {
          | Linebreak => [[], []]
          | Text(s) => [[Text(s)]]
          | Align(l) => [[mk(go(l))]]
          | Annot(ann, l) =>
            go(l) |> List.map(row => [Annot(ann, HBox(row))])
          | Cat(l1, l2) =>
            let (leading, last) = ListUtil.split_last(go(l1));
            let (first, trailing) = ListUtil.split_first(go(l2));
            leading @ [append_hbox(last, first), ...trailing];
          };
        MemoTbl.set(table, Obj.magic(l), Obj.magic(box));
        box;
      };
    };
    mk(go(l));
  };
};
