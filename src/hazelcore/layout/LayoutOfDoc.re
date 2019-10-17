open Sexplib.Std;

let rec all: 'tag. Doc.t('tag) => list(Layout.t('tag)) =
  fun
  | Fail => failwith("unimplemented: all Fail")
  | Empty => [Layout.Empty]
  | Text(string) => [Layout.Text(string)]
  | Align(d) => List.map(l => Layout.Align(l), all(d))
  | Tagged(tag, d) => List.map(l => Layout.Tagged(tag, l), all(d))
  | VCat(d1, d2) => {
      let ls1 = all(d1);
      let ls2 = all(d2);
      List.concat(
        List.map(l1 => List.map(l2 => Layout.VCat(l1, l2), ls2), ls1),
      );
    }
  | HCat(d1, d2) => {
      let ls1 = all(d1);
      let ls2 = all(d2);
      List.concat(
        List.map(l1 => List.map(l2 => Layout.HCat(l1, l2), ls2), ls1),
      );
    }
  | Choice(d1, d2) => all(d1) @ all(d2);

// TODO: does Reason have 'type classes'? operators?
// TODO: unions are left biased

module type Tag = {
  type t;
  let sexp_of_t: t => Sexplib.Sexp.t;
};

module OrderedInt = {
  type t = int;
  let compare = (x: t, y: t): int =>
    if (x < y) {
      (-1);
    } else if (x == y) {
      0;
    } else {
      0;
    };
};

// Maps keyed by an end position
//module PosMap = Map.Make(OrderedInt);

// Maps keyed by an end position
module PosMap = {
  // Invarient: keys are assending and unique
  type t('a) = list((int, 'a));
  type key = int;
  let empty: 'a. t('a) = [];
  let singleton: 'a. (int, 'a) => t('a) = (pos, x) => [(pos, x)];
  let rec union: 'a. (('a, 'a) => 'a, t('a), t('a)) => t('a) =
    (f, t1, t2) =>
      switch (t1, t2) {
      | ([], t_other) => t_other
      | (t_other, []) => t_other
      | ([(p1, x1), ...xs1], [(p2, x2), ...xs2]) =>
        if (p1 < p2) {
          [(p1, x1), ...union(f, xs1, [(p2, x2), ...xs2])];
        } else if (p1 > p2) {
          [(p2, x2), ...union(f, [(p1, x1), ...xs1], xs2)];
        } else {
          [(p1, f(x1, x2)), ...union(f, xs1, xs2)];
        }
      };
  let rec map: 'a 'b. ('a => 'b, t('a)) => t('b) =
    f =>
      fun
      | [] => []
      | [(pos, x), ...rest] => [(pos, f(x)), ...map(f, rest)];
  let rec fold_left: 'a 'b. ((int, 'b, 'a) => 'b, 'b, t('a)) => 'b =
    (f, z) =>
      fun
      | [] => z
      | [(pos, x), ...rest] => fold_left(f, f(pos, z, x), rest);
};

// NOTE: pos is relative to most recent `Align`
type m'('a) = PosMap.t((int /*cost*/, 'a));
type m('a) = (~width: int, ~pos: int) => m'('a);

let add_cost: 'a. (int, m'('a)) => m'('a) =
  (cost, m) => {
    PosMap.map(((x_cost, x)) => (cost + x_cost, x), m);
  };

let cost_union = ((cost1, _) as t1, (cost2, _) as t2) =>
  if (cost1 <= cost2) {
    t1;
  } else {
    t2;
  };

let m'_union: 'a. (m'('a), m'('a)) => m'('a) =
  (p1, p2) => PosMap.union(cost_union, p1, p2);

let m_union: 'a. (m('a), m('a)) => m('a) =
  (m1, m2, ~width: int, ~pos: int) =>
    m'_union(m1(~width, ~pos), m2(~width, ~pos));

module Let_syntax = {
  let return = (x: 'a): m('a) =>
    (~width as _: int, ~pos: int) => PosMap.singleton(pos, (0, x));
  let map = (m: m('a), ~f: 'a => 'b): m('b) =>
    (~width, ~pos: int) =>
      m(~width, ~pos) |> PosMap.map(((cost, x)) => (cost, f(x)));
  let bind: 'a 'b. (m('a), ~f: 'a => m('b)) => m('b) =
    (_m, ~f as _, ~width as _: int, ~pos as _: int) => {
      /*
             let u: (int, (int, 'a), PosMap.t((int, 'a))) => PosMap.t((int, 'b)) =
               (pos, (cost, x), pos_map) => {
                 m'_union(
                   f(x, ~width, ~pos)
                   |> PosMap.map(((f_cost, f_x)) => (cost + f_cost, f_x)),
                   pos_map,
                 );
               };
       */
      // (pos, (cost, x), z) => {m'_union(z, f(x, ~width, ~pos))},
      failwith(
        "...",
        /*
               PosMap.fold_left(
                 (_pos, z, _x) => z,
                 PosMap.empty,
                 m(~width, ~pos),
               );
         */
      );
    };
};
let return = Let_syntax.return;
let (>>=) = (m, f) => Let_syntax.bind(m, ~f);
let fail: m('a) = (~width as _: int, ~pos as _: int) => PosMap.empty;
let cost = (c: int): m(unit) =>
  (~width as _: int, ~pos: int) => PosMap.singleton(pos, (c, ()));
let advance_position = (amount: int): m(unit) =>
  (~width: int, ~pos: int) =>
    if (pos + amount > width) {
      PosMap.empty;
    } else {
      PosMap.singleton(pos + amount, (0, ()));
    };
let reset_position: m(unit) =
  (~width as _: int, ~pos as _: int) => PosMap.singleton(0, (0, ()));
// TODO: line = reset+cost(1)
// TODO: getWidth

/*
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
   */

// TODO: optimize duplicates in memoization table
// TODO: is there a simpler way to do this
// TODO: use ptr equality for Doc.t but structural equality for shape in hash table
// TODO: move `width` to an inner parameter
module Make = (Tag: Tag) => {
  [@deriving sexp]
  type memo('tag) = {
    width: int,
    pos: int,
    doc: Doc.t('tag),
  };

  module Key = {
    type t = memo(Tag.t);
    let hash = (_: memo(Tag.t)): int => 0;
    let compare = (_: memo(Tag.t), _: memo(Tag.t)): int => 0;
    let sexp_of_t = sexp_of_memo(Tag.sexp_of_t);
  };
  let memo_table: Weak_hashtbl.t(Key.t, m'(Layout.t(Tag.t))) =
    Weak_hashtbl.create((module Key));

  let rec go = (doc: Doc.t(Tag.t)): m(Layout.t(Tag.t)) =>
    (~width: int, ~pos: int) => {
      Core_kernel.Heap_block.value(
        Weak_hashtbl.find_or_add(
          memo_table,
          {width, pos, doc},
          ~default=() => {
            let ret: m(Layout.t(Tag.t)) = {
              switch (doc) {
              | Fail => fail
              | Empty => return(Layout.Empty)
              | Text(string) =>
                let%bind () = advance_position(String.length(string));
                return(Layout.Text(string));
              | Align(_d) => failwith("Unimplemented: " ++ __LOC__)
              | Tagged(tag, d) =>
                let%bind l: m(Layout.t(Tag.t)) = go(d);
                return(Layout.Tagged(tag, l));
              | VCat(d1, d2) =>
                let%bind l1 = go(d1);
                let%bind () = reset_position;
                let%bind () = cost(1);
                let%bind l2 = go(d2);
                return(Layout.VCat(l1, l2));
              | HCat(d1, d2) =>
                let%bind l1 = go(d1);
                let%bind l2 = go(d2);
                return(Layout.HCat(l1, l2));
              | Choice(d1, d2) => m_union(go(d1), go(d2))
              };
            };
            Core_kernel.Heap_block.create_exn(ret(~width, ~pos));
          },
        ),
        //  PosMap.t((int, Layout.t(Tag.t))) //go({...memo, left: memo.first_left, doc: d})
        // TODO: if same length, prefer ones that end at earlier column
        // TODO: do we allow zero width last and first lines?
        // TODO: what about when left >= right?
      );
    };
  // TODO: check if result has infinite cost
  let layout_of_doc =
      (doc: Doc.t(Tag.t), ~width: int, ~pos: int): option(Layout.t(Tag.t)) => {
    let rec minimum =
            ((cost, acc): (int, option('a)))
            : (list((int, 'a)) => option('a)) => {
      fun
      | [] => acc
      | [(x_cost, x), ...xs] =>
        if (x_cost < cost) {
          minimum((x_cost, Some(x)), xs);
        } else {
          minimum((cost, acc), xs);
        };
    };
    // TODO: use options instead of max_int
    minimum((max_int, None), List.map(snd, go(doc, ~width, ~pos)));
  };
};
