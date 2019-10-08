open Sexplib.Std;

[@deriving sexp]
type memo('tag) = {
  left: int,
  right: int,
  first_left: int,
  last_right: int,
  doc: Doc.t('tag),
};

// TODO: does Reason have 'type classes'? operators?

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

type m('t) = option((int, 't));

module Let_syntax = {
  let return = (x: 'a): m('a) => Some((0, x));
  let map = (x: m('a), ~f: 'a => 'b): m('b) =>
    switch (x) {
    | None => None
    | Some((x_c, x_t)) => Some((x_c, f(x_t)))
    };
  let bind = (x: m('a), ~f: 'a => m('b)): m('b) =>
    switch (x) {
    | None => None
    | Some((x_c, x_t)) =>
      switch (f(x_t)) {
      | None => None
      | Some((f_c, f_t)) => Some((x_c + f_c, f_t))
      }
    };
};
let return = Let_syntax.return;
let sub1: m(unit) = Some(((-1), ()));
// TODO: left biased
let min_cost = (x: m('a), y: m('a)): m('a) =>
  switch (x, y) {
  | (None, _) => y
  | (_, None) => x
  | (Some((x_i, _)), Some((y_i, _))) =>
    if (x_i <= y_i) {
      x;
    } else {
      y;
    }
  };

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
  let memo_table: Weak_hashtbl.t(memo(Tag.t), m(Layout.t(Tag.t))) =
    Weak_hashtbl.create((module Key));

  let layout_of_doc = (width: int): (Doc.t(Tag.t) => m(Layout.t(Tag.t))) => {
    let rec go = (memo: memo(Tag.t)): m(Layout.t(Tag.t)) => {
      Core_kernel.Heap_block.value(
        Weak_hashtbl.find_or_add(memo_table, memo, ~default=() =>
          Core_kernel.Heap_block.create_exn(
            switch (memo.doc) {
            | Doc.Empty => Some((0, Layout.Empty))
            | Doc.Text(string) => Some((1, Layout.Text(string))) // TODO: overlength strings
            | Doc.Align(d) => go({...memo, left: memo.first_left, doc: d})
            | Doc.Tagged(tag, d) =>
              let%bind l = go({...memo, doc: d});
              return(Layout.Tagged(tag, l));
            | Doc.VCat(d1, d2) =>
              let%bind l1 = go({...memo, doc: d1});
              let%bind l2 = go({...memo, doc: d2});
              return(Layout.VCat(l1, l2));
            | Doc.HCat(d1, d2) =>
              let go2 = (i: int): m(Layout.t(Tag.t)) => {
                let%bind l1 = go({...memo, last_right: i, doc: d1});
                let%bind l2 = go({...memo, first_left: i, doc: d2});
                let%bind _ = sub1;
                return(Layout.HCat(l1, l2));
              };
              // TODO: if same length, prefer ones that end at earlier column
              // TODO: do we allow zero width last and first lines?
              // TODO: what about when left >= right?
              let choices =
                List.map(go2, GeneralUtil.range(~lo=memo.left, memo.right));
              List.fold_left(min_cost, None, choices);
            | Doc.Choice(d1, d2) =>
              min_cost(go({...memo, doc: d1}), go({...memo, doc: d2}))
            },
          )
        ),
      );
    };
    // TODO: check if result has infinite cost
    d =>
      go({left: 0, first_left: 0, right: width, last_right: width, doc: d});
  };
};
