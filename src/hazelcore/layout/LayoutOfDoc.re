open Sexplib.Std;

[@deriving sexp]
type memo('tag) = {
  left: int,
  right: int,
  first_left: int,
  last_right: int,
  doc: Doc.t('tag),
};

// TODO: move `cost` to own module?
// TODO: is there already a type for this?
type cost =
  | Inf
  | Fin(int);

// TODO: does Reason have 'type classes'? operators? infix?
let cost_plus = (c1: cost, c2: cost): cost => {
  switch (c1, c2) {
  | (Inf, _) => Inf
  | (_, Inf) => Inf
  | (Fin(i1), Fin(i2)) => Fin(i1 + i2)
  };
};

let cost_le = (c1: cost, c2: cost): bool => {
  switch (c1, c2) {
  | (Inf, Inf) => true
  | (Inf, Fin(_)) => false
  | (Fin(_), Inf) => true
  | (Fin(i1), Fin(i2)) => i1 <= i2
  };
};

let cost_sub1 = (c: cost): cost => {
  switch (c) {
  | Inf => Inf
  | Fin(i) => Fin(i - 1)
  };
};

let range = (_: int, _: int): list(int) => {
  failwith("unimplemented");
};

let min = (_: list('a)): 'a => {
  failwith("unimplemented");
};

module type Tag = {
  type t;
  let sexp_of_t: t => Sexplib.Sexp.t;
};

let rec all: Doc.t('tag) => list(Layout.t('tag)) =
  fun
  | Doc.Empty => [Layout.Empty]
  | Doc.Text(string) => [Layout.Text(string)]
  | Doc.Align(d) => List.map(l => Layout.Align(l), all(d))
  | Doc.Tagged(tag, d) => List.map(l => Layout.Tagged(tag, l), all(d))
  | Doc.VCat(d1, d2) => {
      let ls1 = all(d1);
      let ls2 = all(d2);
      List.concat(
        List.map(l1 => List.map(l2 => Layout.VCat(l1, l2), ls2), ls1),
      );
    }
  | Doc.HCat(d1, d2) => {
      let ls1 = all(d1);
      let ls2 = all(d2);
      List.concat(
        List.map(l1 => List.map(l2 => Layout.HCat(l1, l2), ls2), ls1),
      );
    }
  | Doc.Choice(d1, d2) => all(d1) @ all(d2);

// TODO: optimize duplicates in memoization table
// TODO: is there a simpler way to do this
// TODO: use ptr equality for Doc.t but structural equality for shape in hash table
// TODO: move `width` to an inner parameter
module Make = (Tag: Tag) => {
  module Key = {
    type t = memo(Tag.t);
    let hash = (_: memo(Tag.t)): int => 0;
    let compare = (_: memo(Tag.t), _: memo(Tag.t)): int => 0;
    let sexp_of_t = sexp_of_memo(Tag.sexp_of_t);
  };
  let memo_table: Weak_hashtbl.t(memo(Tag.t), (cost, Layout.t(Tag.t))) =
    Weak_hashtbl.create((module Key));

  let layout_of_doc =
      (width: int): (Doc.t(Tag.t) => (cost, Layout.t(Tag.t))) => {
    let rec go = (memo: memo(Tag.t)): (cost, Layout.t(Tag.t)) => {
      Core_kernel.Heap_block.value(
        Weak_hashtbl.find_or_add(memo_table, memo, ~default=() =>
          Core_kernel.Heap_block.create_exn(
            switch (memo.doc) {
            | Doc.Empty => (Fin(0), Layout.Empty)
            | Doc.Text(string) => (Fin(1), Layout.Text(string)) // TODO: overlength strings
            | Doc.Align(d) => go({...memo, left: memo.first_left, doc: d})
            | Doc.Tagged(tag, d) =>
              let (c, l) = go({...memo, doc: d});
              (c, Layout.Tagged(tag, l));
            | Doc.VCat(d1, d2) =>
              let (c1, l1) = go({...memo, doc: d1});
              let (c2, l2) = go({...memo, doc: d2});
              (cost_plus(c1, c2), Layout.VCat(l1, l2));
            | Doc.HCat(d1, d2) =>
              let go2 = (i: int): (cost, Layout.t(Tag.t)) => {
                let (c1, l1) = go({...memo, last_right: i, doc: d1});
                let (c2, l2) = go({...memo, first_left: i, doc: d2});
                (cost_sub1(cost_plus(c1, c2)), Layout.HCat(l1, l2));
              };
              // TODO: left biased
              // TODO: if same length, prefer ones that end at earlier column
              // TODO: do we allow zero width last and first lines?
              // TODO: what about when left >= right?
              min(List.map(go2, range(memo.left, memo.right)));
            | Doc.Choice(d1, d2) =>
              let (c1, l1) = go({...memo, doc: d1});
              let (c2, l2) = go({...memo, doc: d2});
              // TODO: left biased
              if (cost_le(c1, c2)) {
                (c1, l1);
              } else {
                (c2, l2);
              };
            },
          )
        ),
      );
    };
    d =>
      go({left: 0, first_left: 0, right: width, last_right: width, doc: d});
  };
};
