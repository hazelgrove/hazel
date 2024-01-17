open Util;

let expect_lbl =
  fun
  | Sym.NT(_) => raise(MGrammar.Non_alternating_form)
  | T(mlbl) => mlbl;
let expect_srt =
  fun
  | Sym.T(_) => raise(MGrammar.Non_alternating_form)
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
         |> List.filter_map(((msym, rctx)) => {
              let msrt = expect_sort(msym);
              let mold = Mold.{sort: s, prec: p, rctx};
              bounded || Mtrl.is_space(msrt)
                ? Some(Molded.{mtrl: msrt, mold}) : None;
            });
       switch (go(L, Prec.lt(~a, l, p)), go(R, Prec.gt(~a, p, r))) {
       | ([], _)
       | (_, []) => []
       | ([_, ..._] as ent_l, [_, ..._] as ent_r) =>
         Dir.choose(from, ent_l, ent_r)
       };
     })
  |> List.concat;
};

let stride_over = (~from: Dir.t, sort: Molded.Sort.t): Walk.Index.t => {
  let eq = Walk.unit(Stride.Eq(sort));
  (Sym.NT(sort.mtrl), sort.mold.rctx)
  |> RZipper.step(Dir.toggle(from))
  |> List.map(
       Bound.map(((msym, rctx)) => {
         let mlbl = expect_lbl(msym);
         Molded.{
           mtrl: mlbl,
           mold: {
             ...mold,
             rctx,
           },
         };
       }),
     )
  |> List.map(lbl => (lbl, [eq]))
  |> Walk.Index.of_list;
};

let stride_into = (~from: Dir.t, sort: Bound.t(Molded.Sort.t)): Walk.Index.t => {
  let bounds = (s: Molded.Sort.t) => {
    let (l_sort, r_sort) = Molded.Sort.bounds(sort);
    let (l_s, r_s) = Molded.Sort.bounds(Node(s));
    Dir.choose(from, (l_sort, r_s), (l_s, r_sort));
  };
  let seen = Hashtbl.create(10);
  let rec go = (s: Molded.Sort.t) =>
    switch (Hashtbl.find_opt(seen, s.mtrl)) {
    | Some () => Walk.Index.empty
    | None =>
      Hashtbl.add(seen, s.mtrl, ());
      let (l, r) = bounds(s);
      enter(~from, ~l, ~r, s.mtrl)
      |> List.map(s => Walk.Index.union(stride_over(~from, s), go(s)))
      |> Walk.Index.union_all;
    };
  let entered =
    switch (sort) {
    | Node(s) => go(s)
    | Root =>
      Hashtbl.add(seen, Mtrl.Sort.root, ());
      enter(~from, Mtrl.Sort.root)
      |> List.map(s => Walk.Index.union(stride_over(~from, s), go(s)))
      |> Walk.Index.union_all;
    };
  Walk.Index.map(Walk.bound(sort), entered);
};

let step = (~from: Dir.t, src: End.t) =>
  switch (src) {
  | Root => stride_into(~from, Root)
  | Node(lbl) =>
    (Sym.T(lbl.mtrl), lbl.mold.rctx)
    |> RZipper.step(Dir.toggle(from))
    |> List.map(
         Bound.map(((msym, rctx)) => {
           let msrt = expect_srt(msym);
           Molded.{
             mtrl: msrt,
             mold: {
               ...lbl.mold,
               rctx,
             },
           };
         }),
       )
    |> List.concat_map(sort => {
         let into = stride_into(~from, sort);
         let over =
           Bound.to_option(sort)
           |> Option.map(stride_over(~from))
           |> Option.value(~default=Walk.Index.empty);
         [over, into];
       })
    |> Walk.Index.union_all
  };

let walk = (~from: Dir.t, src: End.t) => {
  let seen = Hashtbl.create(100);
  let rec go = (src: Bound.t(Molded.Label.t)) =>
    switch (Hashtbl.find_opt(seen, src)) {
    | Some () => Walk.Index.empty
    | None =>
      Hashtbl.add(seen, src, ());
      let stepped = step(~from, src);
      let walked = {
        open Walk.Index.Syntax;
        let* (src_mid, mid) = stepped;
        switch (mid) {
        | Root => Walk.Index.empty
        | Node(mid) =>
          let* (mid_dst, dst) = go(Node(mid));
          return(Walk.append(src_mid, mid, mid_dst), dst);
        };
      };
      Walk.Index.union(stepped, walked);
    };
  go(src);
};
let walk_into = (~from: Dir.t, sort: Bound.t(Molded.Sort.t)) => {
  open Walk.Index.Syntax;
  let* (src_mid, mid) = stride_into(~from, sort);
  switch (mid) {
  | Root => Walk.Index.empty
  | Node(mid) =>
    let* (mid_dst, dst) = go(Node(mid));
    return(Walk.append(src_mid, mid, mid_dst), dst);
  };
};

let walk = (~from: Dir.t, src: End.t, dst: End.t): list(Walk.t) =>
  walk(~from, src) |> Walk.Index.find(dst);
let walk_eq = (~from, src, dst) =>
  List.filter(Walk.is_eq, walk(~from, src, dst));
let walk_neq = (~from, src, dst) =>
  List.filter(Walk.is_neq, walk(~from, src, dst));

let walk_into = (~from: Dir.t, sort: Bound.t(Molded.Sort.t), dst: End.t) =>
  walk_into(~from, sort) |> Walk.Index.find(dst);

let exit = (~from: Dir.t, src: End.t) => walk_eq(~from, src, Root);

let enter = (~from: Dir.t, sort: Molded.Sort.Bound.t, dst: End.t) =>
  Walk.Index.find(dst, stride_into(~from, sort));
