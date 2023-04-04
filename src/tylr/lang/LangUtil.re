open Util;

type lang = list((Sort.t, list((Gram.t(Sort.t), Assoc.t))));

let sorts = List.map(fst, Lang.t);

let keywords: list(Token.t) =
  Prototiles.bindings(Prototiles.t)
  |> List.map(fst)
  |> List.filter_map(
       fun
       | Token.Shape.Const(t) when CharUtil.is_alpha(t.[0]) => Some(t)
       | _ => None,
     );
let shape_of_token = t =>
  List.mem(t, keywords) ? Token.Shape.Const(t) : Token.shape(t);
let molds_of_token = t => Prototiles.of_token(shape_of_token(t));
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
  | (_, None) => Some(Edge.{l: true, r: true})
  | (None, _) => None
  | (Some(r), Some(s)) => Paths.from_to(r, s)
  };
