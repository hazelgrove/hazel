module Tok = {
  type t = Molded.Label.t;
};

module Rel = {
  type t =
    | Lt(Bound.t(Molded.Sort.t), Molded.Sort.t)
    | Eq(Bound.t(Molded.Sort.t))
    | Gt(Molded.Sort.t, Bound.t(Molded.Sort.t));

  let eq = s => Eq(Node(s));
  let neq = (~src: Dir.t, bound, sort) =>
    Dir.choose(src, Lt(bound, sort), Gt(sort, bound));
};

type t = Chain.t(Rel.t, Tok.t);

exception Not_operator_form;

let mk_list = (~root=false, ~src: Dir.t, w: Walk.t): list(Sym.t(Tok.t, Rel.t)) => {
  let stepped = List.map(Sym.map_nt(Rel.eq));
  let entered = (steps, bound) =>
    switch (ListUtil.split_last_opt(steps)) {
    | Some((steps, Sym.NT(sort))) =>
      stepped(steps) @ [Rel.neq(~src, bound, sort)]
    | _ => raise(Not_operator_form)
    };
  Walk.group_entry(w)
  |> Chain.fold_right(
    (steps, enters, acc) =>
      entered(steps, Bound.Node(Chain.lst(enters))) @ acc,
    steps => root ? entered(steps, Bound.Root) : stepped(steps),
  );
};

let mk = (~root=false, ~src: Dir.t, w: Walk.t) =>
  switch (mk_list(~root, ~src, w)) {
  | [NT(rel), ...tl] =>
    let rec go = cmpr =>
      fun
      | [] => cmpr
      | [T(tok), NT(rel), ...rest] =>
        go(Chain.knil(cmpr, tok, rel), rest)
      | _ => raise(Not_operator_form);
    go(Chain.of_loop(rel));
  | _ => raise(Not_operator_form)
  };

let compare = (~src as d: Dir.t, l: Molded.Label.t, r: Molded.Label.t): list(t) => {
  let (l, r) = Molded.Sym.(t(l), t(r));
  let (src, dst) = Dir.choose(d, l, r);
  src
  |> Walker.walk(Dir.toggle(d))
  |> Walk.Set.find(r)
  |> List.map(mk(~src=d));
};
