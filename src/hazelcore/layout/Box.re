open Layout;

type t('tag) =
  | Text(string)
  | HBox(list(t('tag))) // note: due to alignment, HBox([]) is not a zero (or maybe it is?)
  | VBox(list(t('tag))) // note: due to alignment, VBox([]) is not a zero
  | Tagged('tag, t('tag));

/*
 let vzero = VZero;
 let vcat = (b1: t('tag), b2: t('tag)): t('tag) =>
   switch (b1, b2) {
   | (VZero, b2) => b2
   | (b1, VZero) => b1
   | _ => VCat(b1, b2)
   };
 let vcats = (bs: list(t('tag))): t('tag) =>
   List.fold_right(vcat, bs, vzero);
 let hzero = Text("");
 let hcat = (b1: t('tag), b2: t('tag)): t('tag) =>
   switch (b1, b2) {
   | (Text(""), b2) => b2
   | (b1, Text("")) => b1
   | _ => HCat(b1, b2)
   };
   */

// TODO: tag(vzero) = vzero; tag(hzero) != hzero
let tagged = (tag: 'tag, b: t('tag)): t('tag) => Tagged(tag, b);

type tag('tag) =
  | First('tag)
  | Middle('tag)
  | Last('tag);

type tail('tag) =
  | Partial(t('tag) => t('tag))
  | Complete(t('tag));

type head('tag) =
  | Atomic(tail('tag))
  | Divisible(t('tag), t('tag), tail('tag));

// TODO: rename to atomicize?
let atomize = (h: head('tag)): tail('tag) => {
  switch (h) {
  | Atomic(t) => t
  | Divisible(t1, t2, Partial(f)) => Partial(b => VBox([t1, t2, f(b)]))
  | Divisible(t1, t2, Complete(t3)) => Complete(VBox([t1, t2, t3]))
  };
};

let headCat = (h1: head('tag), h2: head('tag)): head('tag) => {
  // TODO: is there a better way to factor these branches?
  switch (h1, h2) {
  | (Atomic(Partial(f1)), Atomic(Partial(f2))) =>
    Atomic(Partial(b => f1(f2(b))))
  | (Atomic(Partial(f1)), Atomic(Complete(b2))) =>
    Atomic(Complete(f1(b2)))
  | (Atomic(Partial(f1)), Divisible(b2, b2d, Partial(f2))) =>
    Divisible(f1(b2), b2d, Partial(f2))
  | (Atomic(Partial(f1)), Divisible(b2, b2d, Complete(b3))) =>
    Divisible(f1(b2), b2d, Complete(b3))

  | (Atomic(Complete(b1)), Atomic(Partial(f2))) =>
    Atomic(Partial(b => HBox([b1, f2(b)])))
  | (Atomic(Complete(b1)), Atomic(Complete(b2))) =>
    Atomic(Complete(HBox([b1, b2])))
  | (Atomic(Complete(b1)), Divisible(b2, b2d, Partial(f2))) =>
    Divisible(HBox([b1, b2]), b2d, Partial(f2))
  | (Atomic(Complete(b1)), Divisible(b2, b2d, Complete(b3))) =>
    Divisible(HBox([b1, b2]), b2d, Complete(b3))

  | (Divisible(b1, b1d, Partial(f1)), Atomic(Partial(f2))) =>
    Divisible(b1, b1d, Partial(b => f1(f2(b))))
  | (Divisible(b1, b1d, Partial(f1)), Atomic(Complete(b2))) =>
    Divisible(b1, b1d, Complete(f1(b2)))
  | (Divisible(b1, b1d, Partial(f1)), Divisible(b2, b2d, Partial(f2))) =>
    Divisible(b1, VBox([b1d, f1(b2), b2d]), Partial(f2))
  | (Divisible(b1, b1d, Partial(f1)), Divisible(b2, b2d, Complete(b2c))) =>
    Divisible(b1, VBox([b1d, f1(b2), b2d]), Complete(b2c))

  | (Divisible(b1, b1d, Complete(b1c)), Atomic(Partial(f2))) =>
    Divisible(b1, b1d, Partial(b => HBox([b1c, f2(b)])))
  | (Divisible(b1, b1d, Complete(b1c)), Atomic(Complete(b2))) =>
    Divisible(b1, b1d, Complete(HBox([b1c, b2])))
  | (Divisible(b1, b1d, Complete(b1c)), Divisible(b2, b2d, Partial(f2))) =>
    Divisible(b1, VBox([b1d, HBox([b1c, b2]), b2d]), Partial(f2))
  | (Divisible(b1, b1d, Complete(b1c)), Divisible(b2, b2d, Complete(b2c))) =>
    Divisible(b1, VBox([b1d, HBox([b1c, b2]), b2d]), Complete(b2c))
  };
};

let box_of_head = (head: head('tag)): t('tag) => {
  let zero = HBox([]); // TODO: is this a real zero?
  switch (head) {
  | Atomic(Partial(f)) => f(zero)
  | Atomic(Complete(b)) => b
  | Divisible(b, bd, Partial(f)) => VBox([b, bd, f(zero)])
  | Divisible(b, bd, Complete(bc)) => VBox([b, bd, bc])
  };
};

let box_of_layout = (layout: Layout.t('tag)): t('tag) => {
  let rec go = (layout: Layout.t('tag)): head('tag) => {
    switch (layout.layout) {
    | Text(string) => Atomic(Complete(Text(string)))
    | Cat(l1, l2) => headCat(go(l1), go(l2))
    | Linebreak => Divisible(Text(""), VBox([]), Complete(Text("")))
    | Align(l) =>
      Atomic(
        switch (atomize(go(l))) {
        | Complete(b1) => Partial(b2 => HBox([b1, b2]))
        | Partial(f) => Partial(b => VBox([f(b)]))
        },
      )
    | Tagged(_, l) => go(l) // TODO: handle tags
    };
  };
  box_of_head(go(layout));
};
