open Util;

[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t('focus, 'atom) = ('focus, RCtx.t('atom));

let rec enter =
        (~ctx=RCtx.empty, ~from: Dir.t, r: Regex.t('a))
        : list(Bound.t(t('a, 'a))) => {
  let go = enter(~from);
  switch (r) {
  | Atom(a) => [Node((a, ctx))]
  | Star(r) => [Root, ...go(~ctx=[Star_, ...ctx], r)]
  | Alt(s) =>
    ListUtil.elem_splits(s)
    |> List.concat_map(((ls, r, rs)) =>
         go(~ctx=[Alt_(ls, rs), ...ctx], r)
       )
  | Seq(s) =>
    switch (from == L ? s : List.rev(s)) {
    | [] => [Root]
    | [hd, ...tl] =>
      hd
      |> go(~ctx=RCtx.push_s(~onto=Dir.toggle(from), tl, ctx))
      |> List.concat_map(
           fun
           | Bound.Node(_) as n => [n]
           | Root => go(~ctx=RCtx.push(~onto=from, hd, ctx), Seq(tl)),
         )
    }
  };
};

let step = (d: Dir.t, (a, ctx): t('a, 'a)): list(Bound.t(t('a, 'a))) => {
  let enter = enter(~from=Dir.toggle(d));
  let rec go = (r: Regex.t('a), ctx: RCtx.t(_)) =>
    switch (ctx) {
    | [] => []
    | [f, ...fs] =>
      switch (d, f) {
      | (_, Star_) => go(Star(r), fs) @ enter(r, ~ctx)
      | (_, Alt_(ls, rs)) => go(Alt(List.rev(ls) @ [r, ...rs]), fs)
      | (L, Seq_([], rs)) => go(Seq([r, ...rs]), fs)
      | (R, Seq_(ls, [])) => go(Seq(List.rev([r, ...ls])), fs)
      | (L, Seq_([hd, ...tl], rs)) =>
        enter(hd, ~ctx=[Seq_(tl, [r, ...rs]), ...fs])
      | (R, Seq_(ls, [hd, ...tl])) =>
        enter(hd, ~ctx=[Seq_([r, ...ls], tl), ...fs])
      }
    };
  go(Atom(a), ctx);
};

let framed_elems = _ => failwith("todo");

let map = (f: t('a, 'a) => 'b, rgx: Regex.t('a)): Regex.t('b) => {
  let rec go = (rgx: Regex.t(_), ctx: RCtx.t(_)): Regex.t(_) =>
    switch (rgx) {
    | Atom(a) => Atom(f((a, ctx)))
    | Star(r) => Star(go(r, [Star_, ...ctx]))
    | Seq(rs) =>
      framed_elems(rs)
      |> List.map(((ls, r, rs)) => go(r, [Seq_(ls, rs), ...ctx]))
      |> Regex.seq
    | Alt(rs) =>
      framed_elems(rs)
      |> List.map(((ls, r, rs)) => go(r, [Alt_(ls, rs), ...ctx]))
      |> Regex.alt
    };
  go(rgx, RCtx.empty);
};
