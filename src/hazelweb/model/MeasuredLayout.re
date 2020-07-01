open Sexplib.Std;

[@deriving sexp]
type box = {
  height: int,
  width: int,
};

[@deriving sexp]
type t = {
  layout: t',
  metrics: list(box),
}
and t' =
  | Linebreak
  | Text(string)
  | Align(t)
  | Cat(t, t)
  | Annot(UHAnnot.t, t);

let update_position = ((row: int, col: int), m: t): (int, int) => {
  let (leading, last) = ListUtil.split_last(m.metrics);
  let total_height =
    leading |> List.map(box => box.height) |> List.fold_left((+), last.height);
  let updated_row = row + total_height - 1;
  let updated_col =
    switch (leading) {
    | [] => col + last.width
    | [_, ..._] => last.width
    };
  (updated_row, updated_col);
};

// flattens away Linebreak and Cat nodes
let flatten = (m: t): list(list(t)) => {
  let rec go = (~tail: list(list(t)), m: t): list(list(t)) => {
    switch (m.layout) {
    | Text(_)
    | Align(_)
    | Annot(_) =>
      switch (tail) {
      | [] => [[m]]
      | [row, ...rows] => [[m, ...row], ...rows]
      }
    | Linebreak => [[], ...tail]
    | Cat(m1, m2) => go(~tail=go(~tail, m2), m1)
    };
  };
  go(~tail=[], m);
};

let table: WeakMap.t(UHLayout.t, t) = WeakMap.mk();
let rec mk = (l: UHLayout.t): t => {
  switch (WeakMap.get(table, l)) {
  | Some(m) => m
  | None =>
    let m =
      switch (l) {
      | Linebreak =>
        let box = {height: 1, width: 0};
        {metrics: [box, box], layout: Linebreak};
      | Text(s) => {
          metrics: [{height: 1, width: StringUtil.utf8_length(s)}],
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
    ignore(WeakMap.set(table, l, m));
    m;
  };
};
