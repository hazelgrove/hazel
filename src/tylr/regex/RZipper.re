[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t('x) = ('x, RContext.t);

let rec enter = (~ctx=RContext.empty, ~from: Dir.t, r: Regex.t): list(t(Atom.t)) => {
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
