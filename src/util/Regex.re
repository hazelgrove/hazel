open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t('a) =
  | Atom('a)
  | Star(t('a))
  | Seq(s('a))
  | Alt(s('a))
and s('a) = list(t('a));
// for internal use in modules below
type regex('a) = t('a);

let seq = rs => Seq(rs);
let alt = rs => Alt(rs);

let eps = Seq([]);
let opt = r => Alt([eps, r]);

let rec map = r' =>
  fun
  | Atom(a) => Atom(r'(a))
  | Star(r) => Star(map(r', r))
  | Seq(rs) => Seq(List.map(map(r'), rs))
  | Alt(rs) => Alt(List.map(map(r'), rs));

let rec fold:
  'a.
  (~atom: 'a => 'acc, ~star: _, ~seq: _, ~alt: _, t('a)) => 'acc
 =
  (~atom, ~star, ~seq, ~alt) => {
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
  type t('a) =
    | Star_
    | Seq_(s('a), s('a))
    | Alt_(s('a), s('a));
  type s('a) = list(t('a));

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
  type t('x, 'a) = ('x, Unzipped.s('a));

  let rec enter =
          (
            ~skip_nullable=true,
            ~from: Dir.t,
            r: regex(_),
            uz: Unzipped.s(_),
          )
          : list(t(_)) => {
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
            nullable(hd)
              ? go(Seq(tl), Unzipped.push(~onto=L, hd, uz)) : [];
          go_hd @ go_tl;
        }
      | R =>
        switch (ListUtil.split_last_opt(rs)) {
        | None => []
        | Some((tl, hd)) =>
          let go_hd = go(hd, Unzipped.push_seq(~onto=L, tl, uz));
          let go_tl =
            nullable(hd)
              ? go(Seq(tl), Unzipped.push(~onto=R, hd, uz)) : [];
          go_hd @ go_tl;
        }
      }
    };
  };

  let rec step =
          (~skip_nullable=true, d: Dir.t, (r, uz): t(regex('a), _))
          : list(t('a, _)) => {
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

  let rec move =
          (
            ~skip_nullable=true,
            ~until: 'a => option('b),
            d: Dir.t,
            (a, uz): t('a, 'a),
          )
          : list(t('b, 'a)) => {
    let go = move(~skip_nullable, ~until, d);
    let step = step(~skip_nullable, d);
    let (found_now, found_later) =
      step((Atom(a), uz))
      |> List.partition_map(((a, uz): t(_)) =>
           switch (until(a)) {
           | Some(found) => Left((found, uz))
           | None => Right(go((a, uz)))
           }
         );
    found_now @ List.concat(found_later);
  };
};
