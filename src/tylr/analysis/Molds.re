open Util;

include Label.Map;
type t = Label.Map.t(list(Mold.t));

let union2 = union((_, ms_l, ms_r) => Some(ms_l @ ms_r));
let union = List.fold_left(union2, empty);

let of_regex = (s: Sort.t, p: Prec.t, g: Regex.t): t => {
  let mk = ctx => Mold.mk(~ctx, s, p);
  let rec go = (uz: Regex.Ctx.t, g: Regex.t) =>
    switch (g) {
    | Atom(Kid(_)) => empty
    | Atom(Tok(t)) => singleton(t, [mk(uz)])
    | Star(g) => go([Star_, ...uz], g)
    | Alt(gs) =>
      ListUtil.elem_splits(gs)
      |> List.map(((pre, g, suf)) => go([Alt_(pre, suf), ...uz], g))
      |> union
    | Seq(gs) =>
      ListUtil.elem_splits(gs)
      |> List.map(((pre, g, suf)) => go([Seq_(pre, suf), ...uz], g))
      |> union
    };
  go(Regex.Ctx.empty, g);
};
let v: t =
  Sort.Map.bindings(Grammar.v)
  |> List.map(((s, tbl)) =>
       tbl |> Prec.Table.map((p, _, regex) => of_regex(s, p, regex)) |> union
     )
  |> union;

let get = lbl =>
  switch (find_opt(lbl, v)) {
  | None => []
  | Some(ms) => ms
  };
