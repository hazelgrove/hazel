open Walk;

let expect_lbl =
  fun
  | Sym.NT(_) => failwith("expected alternating form")
  | T(mlbl) => mlbl;
let expect_srt =
  fun
  | Sym.T(_) => failwith("expected alternating form")
  | NT(msrt) => msrt;

module Grammar = {
  // materialized grammar
  type t = Mtrl.Sorted.Map.t(Prec.Table.t(Mtrl.Regex.t));

  let mtrlize_tiles = (g: Grammar.t): t =>
    Sort.Map.to_seq(g)
    |> Seq.map(((s, tbl)) => {
         let mtrlize_sym =
           Sym.map(Mtrl.tile, ((pad, s)) => (pad, Mtrl.tile(s)));
         let mtbl = Prec.Table.map(Regex.map(mtrlize_sym), tbl);
         (Mtrl.Tile(s), mtbl);
       })
    |> Mtrl.Sorted.Map.of_seq;

  // intention here is, given grammar in operator form (no consecutive NTs),
  // insert a space-mtrl NT between any consecutive NTs and before/following
  // any Ts at the left/right end of each prec level.
  // currently assumes input grammar is in alternating form (neither consecutive
  // NTs nor Ts) and only inserts space NTs at the ends of prec levels as needed.
  let mtrlize_space = (g: t) =>
    g
    |> Mtrl.Sorted.Map.map(
         Prec.Table.map(rgx => {
           let (ls, rs) =
             RZipper.(enter(~from=L, rgx), enter(~from=R, rgx));
           let exists_t =
             List.exists(
               fun
               | Bound.Root => false
               | Node((sym, _)) => Sym.is_t(sym),
             );
           let spc = Regex.atom(Space.Sym.nt);

           rgx
           |> (exists_t(ls) ? Regex.push(~from=L, spc) : Fun.id)
           |> (exists_t(rs) ? Regex.push(~from=R, spc) : Fun.id);
         }),
       )
    |> Mtrl.Sorted.Map.add(Space, Prec.Table.singleton(Space.Regex.v));

  let mtrlize = (g: Grammar.t): t =>
    // grout not explicitly materialized in grammar form,
    // instead generated as needed when baking walks
    g |> mtrlize_tiles |> mtrlize_space;

  let v = mtrlize(Grammar.v);

  let enter =
      (~from: Dir.t, ~l=Bound.Root, ~r=Bound.Root, s: Mtrl.Sorted.t)
      : list(Molded.NT.t) => {
    v
    |> Mtrl.Sorted.Map.find(s)
    |> Prec.Table.mapi(((p, a), rgx) => {
         // need to check for legal bounded entry from both sides
         let go = (from: Dir.t, bounded) =>
           // currently filtering without assuming single operator form
           // for each prec level. this may need to change.
           RZipper.enter(~from, rgx)
           |> List.filter_map(
                fun
                | Bound.Root => None
                | Node((msym, rctx)) => {
                    let (pad, msrt) = expect_srt(msym);
                    let mold = Mold.{sort: s, prec: p, rctx};
                    bounded || Mtrl.is_space(msrt)
                      ? Some(((pad, msrt), mold)) : None;
                  },
              );
         switch (go(L, Prec.lt(~a, l, p)), go(R, Prec.gt(~a, p, r))) {
         | ([], _)
         | (_, []) => []
         | ([_, ..._] as ent_l, [_, ..._] as ent_r) =>
           Dir.pick(from, (ent_l, ent_r))
         };
       })
    |> List.concat;
  };
};

let swing_over = (~from: Dir.t, sort: Bound.t(Molded.NT.t)): Index.t =>
  switch (sort) {
  | Root => Index.singleton(Root, [singleton(Swing.mk_eq(Bound.Root))])
  | Node((mtrl, mold)) =>
    (Sym.NT(mtrl), mold.rctx)
    |> RZipper.step(Dir.toggle(from))
    |> List.map(
         Bound.map(((msym, rctx)) => (expect_lbl(msym), {...mold, rctx})),
       )
    |> List.map(lbl => (lbl, [singleton(Swing.mk_eq(sort))]))
    |> Index.of_list
  };

let swing_into = (~from: Dir.t, sort: Bound.t(Molded.NT.t)): Index.t => {
  let bounds = (s: Molded.NT.t) => {
    let (l_sort, r_sort) = Molded.NT.bounds(sort);
    let (l_s, r_s) = Molded.NT.bounds(Bound.Node(s));
    Dir.pick(from, ((l_sort, r_s), (l_s, r_sort)));
  };
  let seen = Hashtbl.create(10);
  let rec go = (s: Bound.t(Molded.NT.t)) => {
    let (mtrl, (l, r)) =
      switch (s) {
      | Root => (Mtrl.Sorted.root, Bound.(Root, Root))
      | Node(((_, mtrl), _) as nt) => (mtrl, bounds(nt))
      };
    switch (Hashtbl.find_opt(seen, mtrl)) {
    | Some () => Index.empty
    | None =>
      Hashtbl.add(seen, mtrl, ());
      Grammar.enter(~from, ~l, ~r, mtrl)
      |> List.map((s: Molded.NT.t) =>
           Index.union(swing_over(~from, Node(s)), go(Node(s)))
         )
      |> Index.union_all
      |> Index.map(Walk.cons(s));
    };
  };
  go(sort);
};
let swing = (~from: Dir.t, sort: Bound.t(Molded.NT.t)) =>
  Index.union(swing_over(~from, sort), swing_into(~from, sort));

let step = (~from: Dir.t, src: End.t) =>
  switch (src) {
  | Root => swing(~from, Root)
  | Node((mtrl, mold)) =>
    (Sym.T(mtrl), mold.rctx)
    |> RZipper.step(Dir.toggle(from))
    |> List.map(
         Bound.map(((msym, rctx)) => (expect_srt(msym), {...mold, rctx})),
       )
    |> List.map(swing(~from))
    |> Index.union_all
  };

let lt = (l: End.t, r: End.t): list(Bound.t(Molded.NT.t)) =>
  step(~from=L, l)
  |> Index.find(r)
  |> List.filter_map(walk => {
       let swing = fst(walk);
       Swing.is_eq(swing) ? None : Some(Swing.bot(swing));
     });
let gt = (l: End.t, r: End.t) =>
  step(~from=R, r)
  |> Index.find(l)
  |> List.filter_map(walk => {
       let swing = fst(walk);
       Swing.is_eq(swing) ? None : Some(Swing.bot(swing));
     });
let eq = (l: End.t, r: End.t) =>
  step(~from=L, l)
  |> Index.find(r)
  |> List.filter(walk => Swing.is_eq(fst(walk)));

let walk = (~from: Dir.t, src: End.t) => {
  let seen = Hashtbl.create(100);
  let rec go = (src: Bound.t(Molded.T.t)) =>
    switch (Hashtbl.find_opt(seen, src)) {
    | Some () => Index.empty
    | None =>
      Hashtbl.add(seen, src, ());
      let stepped = step(~from, src);
      let walked = {
        open Index.Syntax;
        let* (src_mid, mid) = stepped;
        switch (mid) {
        | Root => Index.empty
        | Node(mid) =>
          let* (mid_dst, dst) = go(Node(mid));
          return(append(src_mid, mid, mid_dst), dst);
        };
      };
      Index.union(stepped, walked);
    };
  go(src);
};

let walk_into = (~from: Dir.t, sort: Bound.t(Molded.NT.t)) => {
  open Index.Syntax;
  let* (src_mid, mid) = swing_into(~from, sort);
  switch (mid) {
  | Root => Index.empty
  | Node(mid) =>
    let* (mid_dst, dst) = walk(~from, Node(mid));
    return(append(src_mid, mid, mid_dst), dst);
  };
};

let step = (~from: Dir.t, src: End.t, dst: End.t): list(t) =>
  Index.find(dst, step(~from, src));
let walk = (~from: Dir.t, src: End.t, dst: End.t): list(t) =>
  Index.find(dst, walk(~from, src));

let walk_eq = (~from, src, dst) =>
  List.filter(is_eq, walk(~from, src, dst));
let walk_neq = (~from, src, dst) =>
  List.filter(is_neq, walk(~from, src, dst));

let enter = (~from: Dir.t, sort: Bound.t(Molded.NT.t), dst: End.t) =>
  Index.find(dst, walk_into(~from, sort));
let exit = (~from: Dir.t, src: End.t) => walk_eq(~from, src, Root);