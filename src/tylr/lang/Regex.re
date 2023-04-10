open Sexplib.Std;
open Util;

module Atom = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t('s) =
    | Tok(Label.t)
    | Kid('s);

  let is_tok =
    fun
    | Kid(_) => None
    | Tok(t) => Some(t);
  let is_kid =
    fun
    | Tok(_) => None
    | Kid(s) => Some(s);
};

[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t('s) =
  | Atom(Atom.t('s))
  | Star(t('s))
  | Seq(s('s))
  | Alt(s('s))
and s('s) = list(t('s));
// for internal use in modules below
type regex('s) = t('s);

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

let rec fold = (~atom, ~star, ~seq, ~alt) => {
  let fold = fold(~atom, ~star, ~seq, ~alt);
  fun
  | Atom(a) => atom(a)
  | Star(r) => star(fold(r))
  | Seq(rs) => seq(List.map(fold, rs))
  | Alt(rs) => alt(List.map(fold, rs));
};

let nullable = r =>
  fold(
    // assuming all atoms are non-nullable
    // but could change this in future
    ~atom=_ => false,
    ~star=_ => true,
    ~seq=List.for_all(Fun.id),
    ~alt=List.exists(Fun.id),
    r,
  );

module Unzipped = {
  type t('s) =
    | Star_
    | Seq_(s('s), s('s))
    | Alt_(s('s), s('s));
  type s('s) = list(t('s));

  let empty = [];

  let zip = (r: regex(_), uz: t(_)) =>
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

  let push = (~onto: Dir.t, r: regex(_), uz: s(_)): s(_) => {
    let rs =
      switch (r) {
      | Seq(rs) => rs
      | _ => [r]
      };
    switch (onto, uz) {
    | (L, [Seq_(ls, r), ...uz]) => [Seq_(ls @ rs, r), ...uz]
    | (R, [Seq_(ls, r), ...uz]) => [Seq_(ls, rs @ r), ...uz]
    | (L, _) => [Seq_(rs, []), ...uz]
    | (R, _) => [Seq_([], rs), ...uz]
    };
  };
  let push_seq = (~onto: Dir.t, rs: list(regex(_)), uz: s(_)) => {
    let push = push(~onto);
    switch (onto) {
    | L => List.fold_left(Fun.flip(push), uz, rs)
    | R => List.fold_right(push, rs, uz)
    };
  };
};

module Zipper = {
  type t('x, 's) = ('x, Unzipped.s('s));
};

let rec enter =
        (~skip_nullable=true, ~from: Dir.t, r: t(_), uz: Unzipped.s(_))
        : list(Zipper.t(_)) => {
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
        let go_hd = go(hd, Unzipped.push_seq(~onto=R, tl, uz));
        let go_tl =
          nullable(hd) ? go(Seq(tl), Unzipped.push(~onto=L, hd, uz)) : [];
        go_hd @ go_tl;
      }
    | R =>
      switch (ListUtil.split_last_opt(rs)) {
      | None => []
      | Some((tl, hd)) =>
        let go_hd = go(hd, Unzipped.push_seq(~onto=L, tl, uz));
        let go_tl =
          nullable(hd) ? go(Seq(tl), Unzipped.push(~onto=R, hd, uz)) : [];
        go_hd @ go_tl;
      }
    }
  };
};

let rec step =
        (~skip_nullable: bool, d: Dir.t, (r, uz): Zipper.t(t(_), _))
        : list(Zipper.t(Atom.t(_), _)) => {
  let go = step(~skip_nullable, d);
  let enter = enter(~skip_nullable);
  switch (uz) {
  | [] => []
  | [r', ...uz] =>
    switch (d, r') {
    | (_, Star_)
    | (_, Alt_(_)) => go((Unzipped.zip(r, r'), uz))
    | (L, Seq_(ls, rs)) =>
      let uz_ls =
        uz |> Unzipped.push_seq(~onto=R, rs) |> Unzipped.push(~onto=R, r);
      let enter_ls = enter(~from=R, Seq(ls), uz_ls);
      let go_beyond =
        List.for_all(nullable, ls) ? go((Unzipped.zip(r, r'), uz)) : [];
      enter_ls @ go_beyond;
    | (R, Seq_(ls, rs)) =>
      let uz_rs =
        uz
        |> Unzipped.push_seq(~onto=L, List.rev(ls))
        |> Unzipped.push(~onto=L, r);
      let enter_rs = enter(~from=L, Seq(rs), uz_rs);
      let go_beyond =
        List.for_all(nullable, rs) ? go((Unzipped.zip(r, r'), uz)) : [];
      enter_rs @ go_beyond;
    }
  };
};
let step = (~skip_nullable=true, d, (a, uz)) =>
  step(~skip_nullable, d, (Atom(a), uz));

module Walk = {
  type t('s) = Chain.t(option(Zipper.t('s, 's)), Zipper.t(Label.t, 's));
  let empty = Chain.of_loop(None);
  let add_kid = (side: Dir.t, z: Zipper.t(_)) =>
    // lossy in general but not for well-typed walks
    switch (side) {
    | L => Chain.map_fst(_ => Some(z))
    | R => Chain.map_lst(_ => Some(z))
    };
  let add_lbl = (side: Dir.t, z: Zipper.t(Label.t, _), w) =>
    switch (side) {
    | L => Chain.link(None, z, w)
    | R => Chain.knil(w, z, None)
    };
  let add_step = (side: Dir.t, (a, uz): Zipper.t(Atom.t(_), _), w: t(_)) =>
    switch (a) {
    | Kid(s) => add_kid(side, (s, uz))
    | Tok(lbl) => add_lbl(side, (lbl, uz))
    };
};

let rec walk =
        (
          ~skip_nullable=true,
          ~until: Atom.t(_) => option('a),
          d: Dir.t,
          ~seen=Walk.empty,
          (a, uz): Zipper.t(Atom.t(_), _),
        )
        : list((Walk.t(_), Zipper.t(Atom.t(_), _))) => {
  let go = walk(~skip_nullable, ~until, d);
  let step = step(~skip_nullable, d);
  let (found_now, found_later) =
    step((a, uz))
    |> List.partition_map(((a, uz) as z: Zipper.t(_)) =>
         switch (until(a)) {
         | Some(found) => Left((seen, (found, uz)))
         | None => Right(go(~seen=Walk.add_step(d, z, seen), z))
         }
       );
  found_now @ List.concat(found_later);
};

let move_to_tok = move(~until=Atom.is_tok);
let move_to_kid = move(~until=Atom.is_kid);

// currently assuming:
// (1) no consecutive kids
// (2) no consecutive tokens
// (3) every sort is convex
// only (1) fundamentally necessary
exception Ill_typed;

let tok_shape = (t: Token.Shape.t) => Atom(Atom.Tok(t));
let tok = (s: string) => tok_shape(Const(s));
let kid = s => Atom(Atom.Kid(s));

let kids =
  fold(
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
