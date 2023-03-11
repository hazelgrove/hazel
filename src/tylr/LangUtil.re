open Util;

type lang = list((Sort.t, list((Gram.t(Sort.t), Assoc.t))));

module Molds = {
  include Token.Map;
  type t = Token.Map.t(list(Mold.t));

  let union2 = union((_, ms_l, ms_r) => Some(ms_l @ ms_r));
  let union = List.fold_left(union2, empty);

  let molds_of_gram = (s: Sort.t, p: Prec.t, g: Gram.t(Sort.t)): t => {
    let rec go = (m: Mold.t, g: Gram.t(Sort.o)) =>
      switch (g) {
      | Atom(Kid(_)) => empty
      | Atom(Tok(t)) => singleton(t, [m])
      | Star(g) => go(Mold.push(Star_, m), g)
      | Alt(gs) =>
        ListUtil.elem_splits(gs)
        |> List.map(((pre, g, suf)) =>
             go(Mold.push(Alt_(pre, suf), m), g)
           )
        |> union
      | Seq(gs) =>
        ListUtil.elem_splits(gs)
        |> List.map(((pre, g, suf)) =>
             go(Mold.push(Seq_(pre, suf), m), g)
           )
        |> union
      };
    g
    |> Gram.map_atoms(
         fun
         | Tok(_) as a => a
         | Kid(s) => Kid(Some(s)),
       )
    |> go(Mold.init(Some(s), p));
  };
  let t: t =
    List.to_seq(Lang.t)
    |> Seq.concat_map(((s, prec_lvls)) =>
         List.to_seq(prec_lvls)
         |> Seq.mapi((p, lvl) => (p, lvl))
         |> Seq.concat_map(((p, (g, _))) =>
              to_seq(molds_of_gram(s, p, g))
            )
       )
    |> Token.Map.of_seq;

  let of_token = token =>
    switch (find_opt(token, t)) {
    | None => []
    | Some(ms) => ms
    };
};

module Tokens = {
  include Mold.Map;
  type t = Mold.Map.t(list(Token.Shape.t));

  let union2 = union((_, ts_l, ts_r) => Some(ts_l @ ts_r));
  let union = List.fold_left(union2, empty);

  let t =
    Molds.bindings(Molds.t)
    |> List.concat_map(((t, ms)) =>
         ms |> List.map(m => singleton(m, [t]))
       )
    |> union;

  let of_mold = m =>
    switch (find_opt(m, t)) {
    | None => []
    | Some(ts) => ts
    };
};

let sorts = List.map(fst, Lang.t);

let keywords: list(Token.t) =
  Molds.bindings(Molds.t)
  |> List.map(fst)
  |> List.filter_map(
       fun
       | Token.Shape.Const(t) when CharUtil.is_alpha(t.[0]) => Some(t)
       | _ => None,
     );
let shape_of_token = t =>
  List.mem(t, keywords) ? Token.Shape.Const(t) : Token.shape(t);
let molds_of_token = t => Molds.of_token(shape_of_token(t));
// todo: reimplement in terms of precedence bounds
let mold_of_token = (in_l: option(Sort.o), out: Sort.o, t: Token.t) => {
  let out_consistent =
    molds_of_token(t)
    |> List.filter((m: Mold.t) => Sort.compare_o(m.sort, out) <= 0);
  switch (out_consistent) {
  | [] => None
  | [m] => Some(m)
  | [_, _, ..._] =>
    let in_l_consistent =
      out_consistent
      |> List.filter(m =>
           switch (in_l, Mold.tip(L, m)) {
           | (None, Concave(_))
           | (Some(_), Convex) => false
           | (None, Convex) => true
           | (Some(actual), Concave(expected, _)) =>
             Sort.compare_o(actual, expected) <= 0
           }
         );
    switch (in_l_consistent) {
    | [] => None
    | [m, ..._] => Some(m) // unspecified choice
    };
  };
};

let tokens_of_mold = Tokens.of_mold;

let assoc = (s, p) => {
  open OptUtil.Syntax;
  let* s = s;
  let* (_, a) = p < 0 ? None : List.nth_opt(List.assoc(s, Lang.t), p);
  a;
};

module Edge = {
  type t = {
    l: bool,
    r: bool,
  };

  let mk = (~l=false, ~r=false, ()) => {l, r};

  let cat = (l, r) => {l: l.l && r.l, r: l.r && r.r};

  let merge = (l, r) => {l: l.l || r.l, r: l.r || r.r};
};

module Edges = {
  module To = {
    include Sort.Map;
    type t = Sort.Map.t(Edge.t);

    let add = (s: Sort.t, e: Edge.t) =>
      Sort.Map.update(
        s,
        fun
        | None => Some(e)
        | Some(f) => Some(Edge.merge(e, f)),
      );

    let union = Sort.Map.union((_, e, f) => Some(Edge.merge(e, f)));

    let of_gram = (g: Gram.t(Sort.t)): Sort.Map.t(Edge.t) => {
      let set_l = s =>
        Sort.Map.update(
          s,
          fun
          | None => Some(Edge.mk(~l=true, ()))
          | Some(Edge.{l: _, r}) => Some({l: true, r}),
        );
      let set_r = s =>
        Sort.Map.update(
          s,
          fun
          | None => Some(Edge.mk(~r=true, ()))
          | Some(Edge.{l, r: _}) => Some({l, r: true}),
        );
      Gram.kids(g)
      |> List.to_seq
      |> Seq.map(s => (s, Edge.mk()))
      |> Sort.Map.of_seq
      |> List.fold_right(
           fun
           | None
           | Some(Gram.Atom.Tok(_)) => Fun.id
           | Some(Kid(s)) => set_l(s),
           Gram.exterior(L, g),
         )
      |> List.fold_right(
           fun
           | None
           | Some(Gram.Atom.Tok(_)) => Fun.id
           | Some(Kid(s)) => set_r(s),
           Gram.exterior(R, g),
         );
    };

    let of_sort = (gs: list(Gram.t(Sort.t))): t =>
      gs |> List.map(of_gram) |> List.fold_left(union, empty);
  };

  include Sort.Map;
  type t = Sort.Map.t(To.t);
  let t: t =
    Lang.t
    |> List.to_seq
    |> Seq.map(((s, gs)) => (s, To.of_sort(List.map(fst, gs))))
    |> Sort.Map.of_seq;

  let from = s => Sort.Map.find(s, t);

  let add = (s_from, s_to, e) =>
    Sort.Map.update(
      s_from,
      fun
      | None => Some(Sort.Map.singleton(s_to, e))
      | Some(to_) => Some(To.add(s_to, e, to_)),
    );

  let union = Sort.Map.union((_, l, r) => Some(To.union(l, r)));
};

module Paths = {
  let rec from = (~seen=[], s): Edges.To.t =>
    if (List.mem(s, seen)) {
      Edges.To.empty;
    } else {
      Edges.from(s)
      |> Edges.To.to_seq
      |> Seq.map(((s1, e1)) =>
           from(~seen=[s, ...seen], s1)
           |> Edges.To.mapi((_, e2) => Edge.cat(e1, e2))
           |> Edges.To.add(s1, e1)
         )
      |> Seq.fold_left(Edges.To.union, Edges.To.empty);
    };

  let t = List.to_seq(sorts) |> Seq.map(s => (s, from(s))) |> Edges.of_seq;

  let from_to = (r, s) => {
    open OptUtil.Syntax;
    let* es_to = Sort.Map.find_opt(r, t);
    Sort.Map.find_opt(s, es_to);
  };
};

let takes = (r, s) =>
  switch (r, s) {
  | (_, None) => true
  | (None, _) => false
  | (Some(r), Some(s)) => Option.is_some(Paths.from_to(r, s))
  };
