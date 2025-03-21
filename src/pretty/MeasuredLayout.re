open Util;

[@deriving sexp]
type box = {
  height: int,
  width: int,
};

[@deriving sexp]
type t('annot) = {
  layout: t'('annot),
  metrics: list(box),
}
and t'('annot) =
  | Linebreak
  | Text(string)
  | Align(t('annot))
  | Cat(t('annot), t('annot))
  | Annot('annot, t('annot));

type with_offset('annot) = (int, t('annot));

let height = (m: t(_)) =>
  m.metrics |> List.map(box => box.height) |> List.fold_left((+), 0);

let width = (~offset=0, m: t(_)) =>
  List.tl(m.metrics)
  |> List.map(box => box.width)
  |> List.fold_left(max, offset + List.hd(m.metrics).width);

let fold =
    (
      ~linebreak: 'acc,
      ~text: string => 'acc,
      ~align: 'acc => 'acc,
      ~cat: ('acc, 'acc) => 'acc,
      ~annot:
         // allow client to control recursion based on annotation
         (t('annot) => 'acc, 'annot, t('annot)) => 'acc,
      m: t('annot),
    )
    : 'acc => {
  let rec go = (m: t(_)) =>
    switch (m.layout) {
    | Linebreak => linebreak
    | Text(s) => text(s)
    | Align(m) => align(go(m))
    | Cat(m1, m2) =>
      let acc1 = go(m1);
      let acc2 = go(m2);
      cat(acc1, acc2);
    | Annot(ann, m) => annot(go, ann, m)
    };
  go(m);
};

let next_position =
    (~indent: int, {row, col}: MeasuredPosition.t, m: t(_))
    : MeasuredPosition.t => {
  let updated_row = row + height(m) - 1;
  let updated_col = {
    let (leading, last) = ListUtil.split_last(m.metrics);
    last.width
    + (
      switch (leading) {
      | [] => col
      | [_, ..._] => indent
      }
    );
  };
  {row: updated_row, col: updated_col};
};

let pos_fold =
    (
      ~linebreak: MeasuredPosition.t => 'acc,
      ~text: (MeasuredPosition.t, string) => 'acc,
      ~align: (MeasuredPosition.t, 'acc) => 'acc,
      ~cat: (MeasuredPosition.t, 'acc, 'acc) => 'acc,
      ~annot:
         // let client control recursion based on annotation
         (
           ~go: t('annot) => 'acc,
           ~indent: int,
           ~start: MeasuredPosition.t,
           'annot,
           t('annot)
         ) =>
         'acc,
      ~indent=0,
      ~start: MeasuredPosition.t=MeasuredPosition.zero,
      m: t('annot),
    )
    : 'acc => {
  let rec go = (indent: int, start: MeasuredPosition.t, m: t(_)) =>
    switch (m.layout) {
    | Linebreak => linebreak(start)
    | Text(s) => text(start, s)
    | Align(m) => align(start, go(start.col, start, m))
    | Cat(m1, m2) =>
      let mid = next_position(~indent, start, m1);
      cat(start, go(indent, start, m1), go(indent, mid, m2));
    | Annot(ann, m) => annot(~go=go(indent, start), ~indent, ~start, ann, m)
    };
  go(indent, start, m);
};

module Make = (MemoTbl: MemoTbl.S) => {
  let table: MemoTbl.t(Layout.t(unit), t(unit)) = MemoTbl.mk();
  let rec mk = (l: Layout.t('annot)): t('annot) => {
    switch (MemoTbl.get(table, Obj.magic(l))) {
    | Some(m) => Obj.magic(m)
    | None =>
      let m =
        switch (l) {
        | Linebreak =>
          let box = {height: 1, width: 0};
          {metrics: [box, box], layout: Linebreak};
        | Text(s) => {
            metrics: [{height: 1, width: Unicode.length(s)}],
            layout: Text(s),
          }
        | Align(l) =>
          let m = mk(l);
          let bounding_box =
            m.metrics
            |> List.fold_left(
                 ({height: bh, width: bw}, {height, width}) =>
                   {height: bh + height, width: max(bw, width)},
                 {height: 0, width: 0},
               );
          {metrics: [bounding_box], layout: Align(m)};
        | Cat(l1, l2) =>
          let m1 = mk(l1);
          let m2 = mk(l2);
          let (leading, last) = ListUtil.split_last(m1.metrics);
          let (first, trailing) = ListUtil.split_first(m2.metrics);
          let mid_box = {
            height: max(last.height, first.height),
            width: last.width + first.width,
          };
          {metrics: leading @ [mid_box, ...trailing], layout: Cat(m1, m2)};
        | Annot(annot, l) =>
          let m = mk(l);
          {...m, layout: Annot(annot, m)};
        };
      MemoTbl.set(table, Obj.magic(l), Obj.magic(m));
      m;
    };
  };
};
