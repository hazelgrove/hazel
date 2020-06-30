type t('annot) =
  | Text(string)
  | HBox(list(t('annot)))
  | VBox(list(t('annot)))
  | Annot('annot, t('annot));

let hbox = boxes => HBox(boxes);
let vbox = boxes => VBox(boxes);

// Note: annots are inside-out (i.e. List.hd(annots) is the inner-most annot)
let rec annot = (annots: list('annot), box: t('annot)): t('annot) => {
  switch (annots) {
  | [] => box
  | [ann, ...anns] => annot(anns, Annot(ann, box))
  };
};

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

let rec height: 'annot. t('annot) => int =
  layout => Obj.magic((Lazy.force(height_memo_table), Obj.magic(layout)))
and height_memo_table: Lazy.t(t(unit) => int) = lazy(memoize(height'))
and height' = (box: t('annot)): int => {
  switch (box) {
  | Text(_) => 1
  | Annot(_, b) => height(b)
  | HBox(bs) => bs |> List.map(height) |> List.fold_left(max, 1) // Note: 1 is HBox([]) height
  | VBox(bs) => bs |> List.map(height) |> List.fold_left((+), 0)
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
  let (leading, last) = ListUtil.split_last(boxes1);
  let (first, trailing) = ListUtil.split_first(boxes2);
  leading @ [append_box(last, first), ...trailing];
};

let mk = (l: Pretty.Layout.t('annot)): t('annot) => {
  let mk = (boxes: list(list(t(_)))) =>
    VBox(List.map(row => HBox(row), boxes));
  let rec go = (l: Pretty.Layout.t(_)) => {
    switch (l) {
    | Linebreak => [[], []]
    | Text(s) => [[Text(s)]]
    | Align(l) => [[mk(go(l))]]
    | Annot(ann, l) => go(l) |> List.map(row => [Annot(ann, HBox(row))])
    | Cat(l1, l2) =>
      let (leading, last) = ListUtil.split_last(go(l1));
      let (first, trailing) = ListUtil.split_first(go(l2));
      leading @ [append_hbox(last, first), ...trailing];
    };
  };
  mk(go(l));
};
