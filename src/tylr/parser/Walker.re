module Step = Walk.Step;

// let step = (d, src: Step.t): list(Step.t) =>
//   switch (src) {
//   | T((mt, mold)) =>
//     RZipper.step(d, (Sym.T(mt), mold.rctx))
//     |> List.map(((msym, rctx)) => (msym, {...mold, rctx}));
//   }

let step = (d, (msym, mold): Molded.Sym.t): Walk.Set.t =>
  RZipper.step(d, (msym, mold.rctx))
  |> List.map(((msym, rctx)) => (msym, {...mold, rctx}))
  |> Walk.Set.init;

let first_step = (~from: Dir.t, ~l=?, ~r=?, s: Mtrl.Sort.t): Walk.Set.t => {
  MGrammar.v
  |> Mtrl.Sort.Map.find(s)
  |> Prec.Table.mapi(((p, a), rgx) => {
       // need to check for legal entry from both sides
       let _ = failwith("todo: fix prec comparison using assoc");
       // currently filtering without assuming single operator form for each prec level
       // this may need to change
       let entered_l =
         RZipper.enter(~from=L, rgx)
         |> List.filter(((sym, _)) => Mtrl.Sym.is_t(sym) || Prec.lt(~a, l, p))
         |> List.map(((sym, rctx)) => Molded.(sym, {sort, prec, ctx}));
       let entered_r =
         RZipper.enter(~from=R, rgx)
         |> List.filter(((sym, _)) => Mtrl.Sym.is_t(sym) || Prec.gt(~a, p, r))
         |> List.map(((sym, rctx)) => Molded.(sym, {sort, prec, ctx}));
       List.(is_empty(entered_l) || is_empty(entered_r))
         ? [] : Dir.choose(from, entered_l, entered_r);
     })
  |> List.concat
  |> Walk.Set.init;
};

let enter = (~from: Dir.t, sort: Molded.Sort.t): Walk.Set.t => {
  let seen = Hashtbl.create(10);
  let rec go = (s: Molded.Sort.t, entered: Walk.t) =>
    switch (Hashtbl.find_opt(seen, s.mtrl)) {
    | Some () => Walk.Set.single(Molded.Sym.t(s), entered)
    | None =>
      Hashtbl.add(seen, s.mtrl, ());
      open Walk.Set.Syntax;
      let (l, r) =
        Mtrl.Sort.eq(s.mtrl, sort.mtrl)
        ? Mold.prec_bounds(sort.mold)
        : (None, None);
      let* msym = first_step(~from, ~l?, ~r?, s.mtrl);
      switch (Molded.Sym.is_sort(msym)) {
      | None => Walk.Set.single(msym, entered)
      | Some(s') =>
        go(s', Walk.link(s, entered))
        |> Walk.Set.add(msym, entered)
      };
    };
  go(sort, Walk.empty);
};
// let enter_root = (~from: Dir.t) => {
//   open Walk.Set.Syntax;
//   let* (sym, _) = first_step(~from, Sort.root);
//   switch (sym.mtrl) {
//   | T(_) => Walk.Set.init([sym])
//   | NT(s) => Walk.Set.(add(msym, Walk.empty, ))
//   };
// };

let walk = (d: Dir.t, src: Step.t): Walk.Set.t => {
  let seen = Hashtbl.create(100);
  let rec go = (dst, walked) =>
    let rec go = (dst, walked) =>
      switch (Hashtbl.find_opt(seen, dst)) {
      | Some () => Walk.Set.single(dst, walked)
      | None =>
        Hashtbl.add(seen, dst, ());
        open Walk.Set.Syntax;
        let stepped = {
          let* (stepped, _empty) = step(d, dst);
          go(stepped, Walk.step(dst, walked));
        };
        let entered =
          switch (Molded.Sym.is_sort(dst)) {
          | None => Walk.Set.empty
          | Some(sort) =>
            let* (dst', entered) = enter(~from=Dir.toggle(d), sort);
            go(dst', Walk.cat(entered, walked));
          }
        Walk.Set.union(stepped, entered);
      };
  go(src, Walk.empty);
};

let eq = (~src as d: Dir.t, l: Molded.Label.t, r: Molded.Label.t) => {
  let (l, r) = Molded.Sym.(t(l), t(r));
  let (src, dst) = Dir.choose(d, l, r);
  src |> walk(Dir.toggle(d)) |> Walk.Set.eq(dst);
};

let neq = (~src as d: Dir.t, l: Molded.Label.t, r: Molded.Label.t) => {
  let (l, r) = Molded.Sym.(t(l), t(r));
  let (src, dst) = Dir.choose(d, l, r);
  src |> walk(Dir.toggle(d)) |> Walk.Set.neq(dst);
};
let lt = neq(~src=L);
let gt = neq(~src=R);
