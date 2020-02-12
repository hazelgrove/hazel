open Sexplib.Std;

// TODO: compute actual layout size and use instead of t_of_layout
let rec all: 'annot. Doc.t('annot) => list(Layout.t('annot)) = {
  fun
  | Text(string) => [Layout.Text(string)]
  | Cat(d1, d2) => {
      let ls1 = all(d1);
      let ls2 = all(d2);
      List.concat(
        List.map(l1 => List.map(l2 => Layout.Cat(l1, l2), ls2), ls1),
      );
    }
  | Linebreak => [Layout.Linebreak]
  | Align(d) => List.map(l => Layout.Align(l), all(d))
  | Annot(annot, d) => List.map(l => Layout.Annot(annot, l), all(d))
  | Fail => []
  | Choice(d1, d2) => all(d1) @ all(d2);
};

// TODO: does Reason have 'type classes'? operators?
// TODO: unions are left biased

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
  let rec mapi: 'a 'b. ((int, 'a) => 'b, t('a)) => t('b) =
    f =>
      fun
      | [] => []
      | [(pos, x), ...rest] => [(pos, f(pos, x)), ...mapi(f, rest)];
  let rec fold_left: 'a 'b. ((int, 'b, 'a) => 'b, 'b, t('a)) => 'b =
    (f, z) =>
      fun
      | [] => z
      | [(pos, x), ...rest] => fold_left(f, f(pos, z, x), rest);
};

// NOTE: pos is relative to most recent `Align`
type m'('a) = PosMap.t((int /*cost*/, 'a));
type m('a) = (~width: int, ~pos: int) => m'('a);

// Functions for m'
let add_cost: 'a. (int, m'('a)) => m'('a) =
  (cost, m) => {
    PosMap.map(((x_cost, x)) => (cost + x_cost, x), m);
  };

let m'_union: 'a. (m'('a), m'('a)) => m'('a) =
  (p1, p2) => {
    let cost_union = ((cost1, _) as t1, (cost2, _) as t2) =>
      if (cost1 <= cost2) {
        t1;
      } else {
        t2;
      };
    PosMap.union(cost_union, p1, p2);
  };

// Monad interface
module Let_syntax = {
  let return = (x: 'a): m('a) =>
    (~width as _: int, ~pos: int) => PosMap.singleton(pos, (0, x));
  let map = (m: m('a), ~f: 'a => 'b): m('b) =>
    (~width, ~pos: int) =>
      m(~width, ~pos) |> PosMap.map(((cost, x)) => (cost, f(x)));
  let bind: 'a 'b. (m('a), ~f: 'a => m('b)) => m('b) =
    (m, ~f, ~width: int, ~pos: int) => {
      PosMap.fold_left(
        (pos, z, (cost, x)) =>
          m'_union(z, add_cost(cost, f(x, ~width, ~pos))),
        PosMap.empty,
        m(~width, ~pos),
      );
    };
};
let return = Let_syntax.return;

// Choice (a non-determinism monad)
let fail: m('a) = (~width as _: int, ~pos as _: int) => PosMap.empty;
let union: 'a. (m('a), m('a)) => m('a) =
  (m1, m2, ~width: int, ~pos: int) =>
    m'_union(m1(~width, ~pos), m2(~width, ~pos));

// Cost (a writer monad)
let tell_cost = (c: int): m(unit) =>
  (~width as _: int, ~pos: int) => PosMap.singleton(pos, (c, ()));

// Width (a reader monad)
let ask_width: m(int) =
  (~width: int, ~pos: int) => PosMap.singleton(pos, (0, width));
let with_width: 'a. (int, m('a)) => m('a) =
  (width, m, ~width as _: int, ~pos: int) => m(~width, ~pos);

// Position (a state monad)
let get_position: m(int) =
  (~width as _: int, ~pos: int) => PosMap.singleton(pos, (0, pos));
let set_position = (pos: int): m(unit) =>
  (~width: int, ~pos as _: int) =>
    if (pos > width) {
      PosMap.empty;
    } else {
      PosMap.singleton(pos, (0, ()));
    };
let modify_position = (delta: int): m(unit) => {
  let%bind pos = get_position;
  set_position(pos + delta);
};

module WidthPosKey = {
  type t = (int, int);
  let hash = ((width, pos)) => 256 * 256 * width + pos;
  let equal = ((w1, p1), (w2, p2)) => w1 == w2 && p1 == p2;
};
module StrongWidthPosKey = Memoize.Strong(WidthPosKey);

let rec layout_of_doc': 'annot. Doc.t('annot) => m(Layout.t('annot)) =
  (doc, ~width: int, ~pos: int) => {
    Obj.magic(snd(Lazy.force(memo_table), Obj.magic(doc), ~width, ~pos));
  }

and memo_table:
  Lazy.t(
    (
      Memoize.WeakPoly.Table.t(m(Layout.t(unit))),
      Doc.t(unit) => m(Layout.t(unit)),
    ),
  ) =
  lazy(Memoize.WeakPoly.make(layout_of_doc''))

and layout_of_doc'': Doc.t(unit) => m(Layout.t(unit)) =
  doc => {
    let g = ((width, pos): (int, int)): m'(Layout.t(unit)) => {
      let ret: m(Layout.t(unit)) = {
        switch (doc) {
        | Text(string) =>
          let%bind () = modify_position(StringUtil.utf8_length(string));
          return(Layout.Text(string));
        | Cat(d1, d2) =>
          let%bind l1 = layout_of_doc'(d1);
          let%bind l2 = layout_of_doc'(d2);
          return(Layout.Cat(l1, l2));
        | Linebreak =>
          let%bind () = tell_cost(1);
          let%bind () = set_position(0);
          return(Layout.Linebreak);
        | Align(d) =>
          let%bind pos = get_position;
          let%bind width = ask_width;
          let%bind l =
            with_width(
              width - pos,
              {
                let%bind () = set_position(0);
                layout_of_doc'(d);
              },
            );
          let%bind () = modify_position(pos);
          return(Layout.Align(l));
        | Annot(annot, d) =>
          let%bind l: m(Layout.t(unit)) = layout_of_doc'(d);
          return(Layout.Annot(annot, l));
        | Fail => fail
        | Choice(d1, d2) => union(layout_of_doc'(d1), layout_of_doc'(d2))
        };
      };
      ret(~width, ~pos);
    };
    let (_clear, h) = StrongWidthPosKey.make(g);
    (~width, ~pos) => h((width, pos));
  };

// Change pos to first_width?
let layout_of_doc =
    (doc: Doc.t('annot), ~width: int, ~pos: int): option(Layout.t('annot)) => {
  let rec minimum =
          ((pos, (cost, t)): (int, (int, option('a))))
          : (list((int, (int, 'a))) => option('a)) => {
    fun
    | [] => t
    | [(x_pos, (x_cost, x)), ...rest] =>
      // Prefer lowest cost, or if same cost, prefer ending at an earlier column
      // (Columns are unique by construction of PosMap.)
      if (x_cost < cost || x_cost == cost && x_pos < pos) {
        minimum((x_pos, (x_cost, Some(x))), rest);
      } else {
        minimum((pos, (cost, t)), rest);
      };
  };
  // TODO: use options instead of max_int
  let l =
    minimum((max_int, (max_int, None)), layout_of_doc'(doc, ~width, ~pos));
  Memoize.WeakPoly.Table.clear(fst(Lazy.force(memo_table)));
  l;
};
