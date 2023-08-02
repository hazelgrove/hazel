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

module Unzipped = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t =
    | Star_
    | Seq_(s, s)
    | Alt_(s, s);
  let zip = (r: regex, uz: t) =>
    switch (uz) {
    | Star_ => Star(r)
    | Alt_(ls, rs) => Alt(List.rev(ls) @ [r, ...rs])
    | Seq_(ls, rs) => Seq(List.rev(ls) @ [r, ...rs])
    };
  let nullable = (side: Dir.t) =>
    List.for_all(uz =>
      switch (side, uz) {
      | (_, Star_ | Alt_(_)) => true
      | (L, Seq_(gs_l, _)) => nullable(Seq(gs_l))
      | (R, Seq_(_, gs_r)) => nullable(Seq(gs_r))
      }
    );
};

module Ctx = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = list(Unzipped.t);
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
  let push_seq = (~onto: Dir.t, rs: list(regex), uz: t) => {
    let push = push(~onto);
    switch (onto) {
    | L => List.fold_left(Fun.flip(push), uz, rs)
    | R => List.fold_right(push, rs, uz)
    };
  };
};

module Zipper = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t('x) = ('x, Ctx.t);
};

let fold_zipper = (~atom, ~star, ~seq, ~alt, r: t): 'acc => {
  let rec go = (ctx: Ctx.t, r) =>
    switch (r) {
    | Atom(a) => atom(ctx, a)
    | Star(r) => star(ctx, go([Star_, ...ctx], r))
    | Seq(rs) =>
      ListUtil.elem_splits(rs)
      |> List.map(((ls, r, rs)) => go([Seq_(ls, rs), ...ctx], r))
      |> seq(ctx)
    | Alt(rs) =>
      ListUtil.elem_splits(rs)
      |> List.map(((ls, r, rs)) => go([Alt_(ls, rs), ...ctx], r))
      |> alt(ctx)
    };
  go(Ctx.empty, r);
};

let rec enter =
        (~skip_nullable=true, ~from: Dir.t, r: t, uz: Ctx.t)
        : list(Zipper.t(Atom.t)) => {
  let go = enter(~skip_nullable, ~from);
  switch (r) {
  | Atom(a) => [(a, uz)]
  | Star(r) => skip_nullable ? [] : go(r, [Star_, ...uz])
  | Alt(rs) =>
    ListUtil.elem_splits(rs)
    |> List.concat_map(((ls, r, rs)) => go(r, [Alt_(ls, rs), ...uz]))
  | Seq(rs) =>
    switch (from) {
    | L =>
      switch (rs) {
      | [] => []
      | [hd, ...tl] =>
        let go_hd = go(hd, Ctx.push_seq(~onto=R, tl, uz));
        let go_tl =
          nullable(hd) ? go(Seq(tl), Ctx.push(~onto=L, hd, uz)) : [];
        go_hd @ go_tl;
      }
    | R =>
      switch (ListUtil.split_last_opt(rs)) {
      | None => []
      | Some((tl, hd)) =>
        let go_hd = go(hd, Ctx.push_seq(~onto=L, tl, uz));
        let go_tl =
          nullable(hd) ? go(Seq(tl), Ctx.push(~onto=R, hd, uz)) : [];
        go_hd @ go_tl;
      }
    }
  };
};

// let rec step =
//         (~skip_nullable: bool, d: Dir.t, (r, uz): Zipper.t(t))
//         : list(Zipper.t(Atom.t)) => {
//   let go = step(~skip_nullable, d);
//   let enter = enter(~skip_nullable);
//   switch (uz) {
//   | [] => []
//   | [r', ...uz] =>
//     switch (d, r') {
//     | (_, Star_)
//     | (_, Alt_(_)) => go((Unzipped.zip(r, r'), uz))
//     | (L, Seq_(ls, rs)) =>
//       let uz_ls = uz |> Ctx.push_seq(~onto=R, rs) |> Ctx.push(~onto=R, r);
//       let enter_ls = enter(~from=R, Seq(ls), uz_ls);
//       let go_beyond =
//         List.for_all(nullable, ls) ? go((Unzipped.zip(r, r'), uz)) : [];
//       enter_ls @ go_beyond;
//     | (R, Seq_(ls, rs)) =>
//       let uz_rs =
//         uz |> Ctx.push_seq(~onto=L, List.rev(ls)) |> Ctx.push(~onto=L, r);
//       let enter_rs = enter(~from=L, Seq(rs), uz_rs);
//       let go_beyond =
//         List.for_all(nullable, rs) ? go((Unzipped.zip(r, r'), uz)) : [];
//       enter_rs @ go_beyond;
//     }
//   };
// };
// let step = (~skip_nullable=true, d, (a, uz)) =>
//   step(~skip_nullable, d, (Atom(a), uz));

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

// let end_toks = (side: Dir.t, r: t): list(Zipper.t(Token.Shape.t)) =>
//   enter(~from=side, r, Unzipped.empty)
//   |> List.concat_map((z: Zipper.t(Atom.t)) =>
//        switch (z) {
//        | (Tok(tok), uz) => [(tok, uz)]
//        | (Kid(_), _) => move(~until=Atom.is_tok, Dir.toggle(side), z)
//        }
//      );
