open Sexplib.Std;

// Invariant for HBox: all but last element must be height <= 1
[@deriving sexp]
type t('tag) =
  | Text(string)
  | HBox(list(t('tag))) // note: due to alignment, HBox([]) is not a zero (or maybe it is?)
  | VBox(list(t('tag))) // note: due to alignment, VBox([]) is not a zero
  | Tagged('tag, t('tag));

let rec box_height: 'tag. t('tag) => int =
  layout => Obj.magic(Lazy.force(box_height_memo_table, Obj.magic(layout)))

and box_height_memo_table: Lazy.t(t(unit) => int) =
  lazy(Memoize.WeakPoly.make(box_height'))

and box_height' = (box: t('tag)): int => {
  switch (box) {
  | Text(_) => 1
  | Tagged(_, b) => box_height(b)
  | HBox(bs) => List.fold_right(max, List.map(box_height, bs), 1) // Note: 1 is HBox([]) height
  | VBox(bs) => List.fold_right((+), List.map(box_height, bs), 0)
  };
};

let rec flatten = (box: t('tag)): t('tag) => {
  let flatten_box_list =
      (
        bs: list(t('tag)),
        ctor: list(t('tag)) => t('tag),
        pattern: t('tag) => option(list(t('tag))),
      )
      : t('tag) => {
    let rec go = (b: t('tag)): list(t('tag)) => {
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
  | Tagged(tag, b) => Tagged(tag, flatten(b))
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
