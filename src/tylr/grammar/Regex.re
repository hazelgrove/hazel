open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t =
  | Atom(Atom.t)
  | Star(t)
  | Seq(s)
  | Alt(s)
and s = list(t);
// for internal use in modules below
type regex = t;

let seq = rs => Seq(rs);
let alt = rs => Alt(rs);

let eps = Seq([]);
let opt = r => Alt([eps, r]);

let rec atoms =
  fun
  | Atom(a) => [a]
  | Star(r) => atoms(r)
  | Seq(rs)
  | Alt(rs) => List.concat_map(atoms, rs);

let rec map = f =>
  fun
  | Atom(a) => Atom(f(a))
  | Star(r) => Star(map(f, r))
  | Seq(rs) => Seq(List.map(map(f), rs))
  | Alt(rs) => Alt(List.map(map(f), rs));

let rec fold =
        (
          ~atom: Atom.t => 'acc,
          ~star: 'acc => 'acc,
          ~seq: list('acc) => 'acc,
          ~alt: list('acc) => 'acc,
        )
        : (t => 'acc) => {
  let fold = fold(~atom, ~star, ~seq, ~alt);
  fun
  | Atom(a) => atom(a)
  | Star(r) => star(fold(r))
  | Seq(rs) => seq(List.map(fold, rs))
  | Alt(rs) => alt(List.map(fold, rs));
};

let nullable = r =>
  r
  |> fold(
       // assuming all atoms are non-nullable
       // but could change this in future
       ~atom=_ => false,
       ~star=_ => true,
       ~seq=List.for_all(Fun.id),
       ~alt=List.exists(Fun.id),
     );

module Frame = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t =
    | Star_
    | Seq_(s, s)
    | Alt_(s, s);
  let zip = (f: t, r: regex) =>
    switch (f) {
    | Star_ => Star(r)
    | Alt_(ls, rs) => Alt(List.rev(ls) @ [r, ...rs])
    | Seq_(ls, rs) => Seq(List.rev(ls) @ [r, ...rs])
    };
  let nullable = (side: Dir.t, f: t) =>
    switch (side, f) {
    | (_, Star_ | Alt_(_)) => true
    | (L, Seq_(gs_l, _)) => nullable(Seq(gs_l))
    | (R, Seq_(_, gs_r)) => nullable(Seq(gs_r))
    };
};

module Ctx = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = list(Frame.t);
  let empty: t = [];
  let push = (~onto: Dir.t, r: regex, ctx: t): t => {
    let rs =
      switch (r) {
      | Seq(rs) => rs
      | _ => [r]
      };
    switch (onto, ctx) {
    | (L, [Seq_(ls, r), ...uz]) => [Seq_(ls @ rs, r), ...uz]
    | (R, [Seq_(ls, r), ...uz]) => [Seq_(ls, rs @ r), ...uz]
    | (L, _) => [Seq_(rs, []), ...ctx]
    | (R, _) => [Seq_([], rs), ...ctx]
    };
  };
  let push_seq = (~onto: Dir.t, rs: list(regex), ctx: t) => {
    let push = push(~onto);
    switch (onto) {
    | L => List.fold_left(Fun.flip(push), ctx, rs)
    | R => List.fold_right(push, rs, ctx)
    };
  };
  let nullable = (side: Dir.t) => List.for_all(Frame.nullable(side));
};

module Zipper = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t('x) = ('x, Ctx.t);

  let rec enter = (~ctx=Ctx.empty, ~from: Dir.t, r: regex): list(t(Atom.t)) => {
    let go = enter(~from);
    switch (r) {
    | Atom(a) => [(a, ctx)]
    | Star(r) => go(~ctx=[Star_, ...ctx], r)
    | Alt(rs) =>
      ListUtil.elem_splits(rs)
      |> List.concat_map(((ls, r, rs)) =>
           go(~ctx=[Alt_(ls, rs), ...ctx], r)
         )
    | Seq(rs) =>
      switch (from) {
      | L =>
        switch (rs) {
        | [] => []
        | [hd, ...tl] =>
          let go_hd = go(~ctx=Ctx.push_seq(~onto=R, tl, ctx), hd);
          let go_tl =
            nullable(hd)
              ? go(~ctx=Ctx.push(~onto=L, hd, ctx), Seq(tl)) : [];
          // prioritize tl in case hd nullable, assuming null by first choice.
          // may need to revisit this in case grammar author manually includes
          // epsilon but does not make it first element of disjunction.
          go_tl @ go_hd;
        }
      | R =>
        switch (ListUtil.split_last_opt(rs)) {
        | None => []
        | Some((tl, hd)) =>
          let go_hd = go(~ctx=Ctx.push_seq(~onto=L, tl, ctx), hd);
          let go_tl =
            nullable(hd)
              ? go(~ctx=Ctx.push(~onto=R, hd, ctx), Seq(tl)) : [];
          go_tl @ go_hd;
        }
      }
    };
  };
};

// currently assuming:
// (1) no consecutive kids
// (2) no consecutive tokens
// (3) every sort is convex
// only (1) fundamentally necessary
exception Ill_typed;

let tok = (lbl: Label.t) => Atom(Atom.Tok(lbl));
let tokc = (t: Token.t) => tok(Const(t));
let kid = s => Atom(Atom.Kid(s));

let kids = r =>
  r
  |> fold(
       ~atom=a => Option.to_list(Atom.is_kid(a)),
       ~star=Fun.id,
       ~seq=List.concat,
       ~alt=List.concat,
     );
