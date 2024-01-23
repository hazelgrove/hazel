open Util;
open Walk;

let expect_lbl =
  fun
  | Sym.NT(_) => failwith("expected alternating form")
  | T(mlbl) => mlbl;
let expect_srt =
  fun
  | Sym.T(_) => failwith("expected alternating form")
  | NT(msrt) => msrt;

let enter =
    (~from: Dir.t, ~l=Bound.Root, ~r=Bound.Root, s: Mtrl.Sort.t)
    : list(Molded.Sort.t) => {
  MGrammar.v
  |> Mtrl.Sort.Map.find(s)
  |> Prec.Table.mapi(((p, a), rgx) => {
       // need to check for legal bounded entry from both sides
       let go = (from: Dir.t, bounded): list(Molded.Sort.t) =>
         // currently filtering without assuming single operator form
         // for each prec level. this may need to change.
         RZipper.enter(~from, rgx)
         |> List.filter_map(
              fun
              | Bound.Root => None
              | Node((msym, rctx)) => {
                  let msrt = expect_srt(msym);
                  let mold = Mold.{sort: s, prec: p, rctx};
                  bounded || Mtrl.is_space(msrt)
                    ? Some(Molded.{mtrl: msrt, mold}) : None;
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

let stride_over = (~from: Dir.t, sort: Bound.t(Molded.Sort.t)): Index.t =>
  switch (sort) {
  | Root => Index.singleton(Root, [singleton(Stride.eq(Bound.Root))])
  | Node(s) =>
    (Sym.NT(s.mtrl), s.mold.rctx)
    |> RZipper.step(Dir.toggle(from))
    |> List.map(
         Bound.map(((msym, rctx)) => {
           let mlbl = expect_lbl(msym);
           let mold = {...s.mold, rctx};
           Molded.{mold, mtrl: mlbl};
         }),
       )
    |> List.map(lbl => (lbl, [singleton(Stride.eq(sort))]))
    |> Index.of_list
  };

let stride_into = (~from: Dir.t, sort: Bound.t(Molded.Sort.t)): Index.t => {
  let bounds = (s: Molded.Sort.t) => {
    let (l_sort, r_sort) = Molded.Sort.bounds(sort);
    let (l_s, r_s) = Molded.Sort.bounds(Bound.Node(s));
    Dir.pick(from, ((l_sort, r_s), (l_s, r_sort)));
  };
  let seen = Hashtbl.create(10);
  let rec go = (s: Bound.t(Molded.Sort.t)) => {
    let (mtrl, (l, r)) =
      switch (s) {
      | Root => (Mtrl.Sort.root, Bound.(Root, Root))
      | Node(s) => (s.mtrl, bounds(s))
      };
    switch (Hashtbl.find_opt(seen, mtrl)) {
    | Some () => Index.empty
    | None =>
      Hashtbl.add(seen, mtrl, ());
      enter(~from, ~l, ~r, mtrl)
      |> List.map((s: Molded.Sort.t) =>
           Index.union(stride_over(~from, Node(s)), go(Node(s)))
         )
      |> Index.union_all
      |> Index.map(bound(s));
    };
  };
  go(sort);
};
let stride = (~from: Dir.t, sort: Bound.t(Molded.Sort.t)) =>
  Index.union(stride_over(~from, sort), stride_into(~from, sort));

let step = (~from: Dir.t, src: End.t) =>
  switch (src) {
  | Root => stride(~from, Root)
  | Node(lbl) =>
    (Sym.T(lbl.mtrl), lbl.mold.rctx)
    |> RZipper.step(Dir.toggle(from))
    |> List.map(
         Bound.map(((msym, rctx)) => {
           let msrt = expect_srt(msym);
           let mold = {...lbl.mold, rctx};
           Molded.{mold, mtrl: msrt};
         }),
       )
    |> List.map(stride(~from))
    |> Index.union_all
  };

let lt = (l: End.t, r: End.t): list(Bound.t(Molded.Sort.t)) =>
  step(~from=L, l)
  |> Index.find(r)
  |> List.filter_map(walk => {
       let stride = fst(walk);
       Stride.is_eq(stride) ? None : Some(Stride.base(stride));
     });
let gt = (l: End.t, r: End.t) =>
  step(~from=R, r)
  |> Index.find(l)
  |> List.filter_map(walk => {
       let stride = fst(walk);
       Stride.is_eq(stride) ? None : Some(Stride.base(stride));
     });
let eq = (l: End.t, r: End.t) =>
  step(~from=L, l)
  |> Index.find(r)
  |> List.filter(walk => Stride.is_eq(fst(walk)));

let walk = (~from: Dir.t, src: End.t) => {
  let seen = Hashtbl.create(100);
  let rec go = (src: Bound.t(Molded.Label.t)) =>
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
let walk_into = (~from: Dir.t, sort: Bound.t(Molded.Sort.t)) => {
  open Index.Syntax;
  let* (src_mid, mid) = stride_into(~from, sort);
  switch (mid) {
  | Root => Index.empty
  | Node(mid) =>
    let* (mid_dst, dst) = walk(~from, Node(mid));
    return(append(src_mid, mid, mid_dst), dst);
  };
};

let walk = (~from: Dir.t, src: End.t, dst: End.t): list(t) =>
  walk(~from, src) |> Index.find(dst);
let walk_eq = (~from, src, dst) =>
  List.filter(is_eq, walk(~from, src, dst));
let walk_neq = (~from, src, dst) =>
  List.filter(is_neq, walk(~from, src, dst));

let walk_into = (~from: Dir.t, sort: Bound.t(Molded.Sort.t), dst: End.t) =>
  walk_into(~from, sort) |> Index.find(dst);

let exit = (~from: Dir.t, src: End.t) => walk_eq(~from, src, Root);

let enter = (~from: Dir.t, sort: Bound.t(Molded.Sort.t), dst: End.t) =>
  Index.find(dst, stride_into(~from, sort));
