open Sexplib.Std;

// Invariant for HBox: all but last element must be height <= 1
[@deriving sexp]
type t('annot) =
  | Text(string)
  | HBox(list(t('annot))) // note: due to alignment, HBox([]) is not a zero (or maybe it is?)
  | VBox(list(t('annot))) // note: due to alignment, VBox([]) is not a zero
  | Annot('annot, t('annot)); // Annotations

let rec box_height: 'annot. t('annot) => int =
  layout =>
    Obj.magic(snd(Lazy.force(box_height_memo_table), Obj.magic(layout)))

and box_height_memo_table:
  Lazy.t((Memoize.WeakPoly.Table.t(int), t(unit) => int)) =
  lazy(Memoize.WeakPoly.make(box_height'))

and box_height' = (box: t('annot)): int => {
  switch (box) {
  | Text(_) => 1
  | Annot(_, b) => box_height(b)
  | HBox(bs) => List.fold_right(max, List.map(box_height, bs), 1) // Note: 1 is HBox([]) height
  | VBox(bs) => List.fold_right((+), List.map(box_height, bs), 0)
  };
};

let rec flatten = (box: t('annot)): t('annot) => {
  let flatten_box_list =
      (
        bs: list(t('annot)),
        ctor: list(t('annot)) => t('annot),
        pattern: t('annot) => option(list(t('annot))),
      )
      : t('annot) => {
    let rec go = (b: t('annot)): list(t('annot)) => {
      switch (pattern(b)) {
      | Some(bs) => List.concat(List.map(go, bs))
      | None => [b]
      };
    };
    switch (List.concat(List.map(go, List.map(flatten, bs)))) {
    | [b] => b
    | bs => ctor(bs)
    };
  };

  switch (box) {
  | Text(_) => box
  | Annot(annot, b) => Annot(annot, flatten(b))
  | HBox(bs) =>
    flatten_box_list(
      bs,
      b => HBox(b),
      fun
      | HBox(bs) => Some(bs)
      | _ => None,
    )
  | VBox(bs) =>
    flatten_box_list(
      bs,
      b => VBox(b),
      fun
      | VBox(bs) => Some(bs)
      | _ => None,
    )
  };
};
