// Invariant for HBox: all but last element must be height <= 1
type t('annot) =
  | Text(string)
  | HBox(list(t('annot))) // note: due to alignment, HBox([]) is not a zero (or maybe it is?)
  | VBox(list(t('annot))) // note: due to alignment, VBox([]) is not a zero
  | Annot('annot, t('annot)); // Annotations

module Key: Hashtbl.HashedType = {
  // We use `Obj.magic` to convert all value to `unit`.  This allows
  // functions that use the memoization table to have a polymorphic key.
  type t = unit;
  let hash = Hashtbl.hash;
  let equal = (==);
};

module MemoTable = Ephemeron.K1.Make(Key);

// Memoization on polymorphic key types
let memoize: ('a => 'b, 'a) => 'b =
  f => {
    let table = MemoTable.create(0);
    let f' = (key: 'a): 'b => {
      let key': Key.t = Obj.magic(key);
      switch (MemoTable.find_opt(table, key')) {
      | Some(value) => value
      | None =>
        let value = f(Obj.magic(key'));
        MemoTable.add(table, key', value);
        value;
      };
    };
    f';
  };

let rec box_height: 'annot. t('annot) => int =
  layout =>
    Obj.magic((Lazy.force(box_height_memo_table), Obj.magic(layout)))

and box_height_memo_table: Lazy.t(t(unit) => int) =
  lazy(memoize(box_height'))

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
