open Util;

[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t('focus, 'atom) = ('focus, RCtx.t('atom));

let rec enter =
        (~ctx=RCtx.empty, ~from: Dir.t, r: Regex.t('a)): list(t('a, 'a)) => {
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
        let go_hd = go(~ctx=RCtx.push_seq(~onto=R, tl, ctx), hd);
        let go_tl =
          Regex.nullable(hd)
            ? go(~ctx=RCtx.push(~onto=L, hd, ctx), Seq(tl)) : [];
        // prioritize tl in case hd nullable, assuming null by first choice.
        // may need to revisit this in case grammar author manually includes
        // epsilon but does not make it first element of disjunction.
        go_tl @ go_hd;
      }
    | R =>
      switch (ListUtil.split_last_opt(rs)) {
      | None => []
      | Some((tl, hd)) =>
        let go_hd = go(~ctx=RCtx.push_seq(~onto=L, tl, ctx), hd);
        let go_tl =
          Regex.nullable(hd)
            ? go(~ctx=RCtx.push(~onto=R, hd, ctx), Seq(tl)) : [];
        go_tl @ go_hd;
      }
    }
  };
};

let step = (d: Dir.t, (a, ctx): t('a, 'a)): list(t('a, 'a)) => {
  let enter = enter(~from=Dir.toggle(d));
  let rec go = (r: Regex.t('a), ctx: RCtx.t(_)) =>
    switch (ctx) {
    | [] => []
    | [f, ...fs] =>
      switch (d, f) {
      | (_, Star_) => go(Star(r), fs)
      | (_, Alt_(ls, rs)) => go(Alt(List.rev(ls) @ [r, ...rs]), fs)
      | (L, Seq_([], rs)) => go(Seq([r, ...rs]), fs)
      | (R, Seq_(ls, [])) => go(Seq(List.rev([r, ...ls])), fs)
      | (L, Seq_([hd, ...tl], rs)) =>
        enter(hd, ~ctx=[Seq_(tl, [r, ...rs]), ...fs])
      | (R, Seq_(ls, [hd, ...tl])) =>
        enter(hd, ~ctx=[Seq_(List.rev([r, ...ls]), tl), ...fs])
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
