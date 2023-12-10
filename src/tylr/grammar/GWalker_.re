let rec step_to_mold =
        (~slot=GSlot.Empty, d: Dir.t, a: GZipper.t(Atom.t)): GWalk.Set.t =>
  switch (GMold.of_atom(a)) {
  | Ok(m) =>
    GWalk.Set.single(Eq(GTerr.singleton(~slot, Tile(Molded(m)))))
  | Error(s) =>
    Regex.step(d, a.zipper)
    |> List.map(zipper =>
      step_to_mold(~slot=Full(Some(s)), d, {...a, zipper})
    )
    |> GWalk.Set.union_all
  };

let step_eq = (d: Dir.t, m: GMtrl.t): GWalk.Set.t =>
  switch (m) {
  | Space => GWalk.Set.empty
  | Grout(tips) =>
    switch (Dir.choose(d, tips)) {
    | Convex => GWalk.Set.empty
    | Concave =>
      let slot = GSlot.Full(None);
      let infix = GMtrl.Grout((Concave, Concave));
      let affix =
        GMtrl.Grout(
          Dir.choose(d, ((Convex, Concave), (Concave, Convex))),
        );
      GWalk.Set.mk([
        Eq(GTerr.singleton(~slot, infix)),
        Eq(GTerr.singleton(~slot, affix)),
      ]);
    }
  | Tile(Unmolded(tips)) =>
    switch (Dir.choose(d, tips)) {
    | Convex => GWalk.Set.empty
    | Concave =>
      let slot = GSlot.Full(None);
      let infix = GMtrl.Tile(Unmolded((Concave, Concave)));
      let affix =
        GMtrl.Tile(
          Unmolded(Dir.choose(d, ((Convex, Concave), (Concave, Convex))))
        );
      GWalk.Set.mk([
        Eq(GTerr.singleton(~slot, infix)),
        Eq(GTerr.singleton(~slot, affix)),
      ])
    }
  | Tile(Molded(m)) =>
    GMold.to_atom(m)
    |> Regex.step(d)
    |> List.map(zipper => step_to_mold(d, {...a, zipper}))
    |> GWalk.Set.union_all;
  };

let walk_eq = (d: Dir.t, m: GMtrl.t): GWalk.Set.t => {
  let seen = Hashtbl.create(100);
  let rec go = m =>
    switch (Hashtbl.find_opt(seen, m)) {
    | Some () => GWalk.Set.empty
    | None =>
      Hashtbl.add(seen, m, ());
      open GWalk.Set.Syntax;
      let* t_hd = step_eq(d, m);
      {
        let* t_tl = go(GTerr.face(t_hd));
        return(cat_t(d, t_hd, t_tl));
      }
      |> GWalk.Set.add(t_hd);
    };
  go(m);
};

// shallow precedence-bounded entry into given sort, stepping to
// all possible atoms at the entered edge across alternatives
let step_enter = (~from: Dir.t, ~l=?, ~r=?, s: Sort.t): list(GZipper.t(Atom.t)) =>
  Grammar.v
  |> Sort.Map.find(s)
  |> Prec.Table.map((p, a, rgx) => {
       let has_kid = List.exists(((atom, _)) => Atom.is_kid(atom));
       let l_bounded =
         switch (l) {
         | Some(p_l) => Prec.lt(~a, p_l, p)
         | _ => true
         };
       let r_bounded =
         switch (r) {
         | Some(p_r) => Prec.gt(~a, p, p_r)
         | _ => true
         };

       let (ls, rs) =
         RZipper.(enter(~from=L, rgx), enter(~from=R, rgx));
       has_kid(ls) && !l_bounded || has_kid(rs) && !r_bounded
         ? []
         : Dir.choose(from, (ls, rs))
           |> List.map(GZipper.mk(~sort=s, ~prec=p))
     })
  |> List.concat;

// deep precedence-bounded entry into given sort and its unidelimited
// dependencies, stepping to nearest token (cf G)
let enter = (~from: Dir.t, ~l=?, ~r=?, s: Sort.t): GWalk.Set.t => {
  let seen = Hashtbl.create(100);
  let rec go = (~l=?, ~r=?, s: Sort.t) => {
    switch (Hashtbl.find_opt(seen, (l, r, s))) {
    | Some(_) => GWalk.Set.empty
    | None =>
      Hashtbl.add(seen, (l, r, s), ());
      let _ = failwith("todo: add entry to space/grout/unmolded");
      // todo: add entry to grout/unmolded tiles
      step_enter(~from, ~l?, ~r?, s)
      |> List.map(z =>
           switch (Mold.of_atom(z)) {
           | Ok(m) => GWalk.Set.single(GTerr.singleton(m))
           | Error(s') =>
             let entered_here = step_to_mold(Dir.toggle(from), z);
             let entered_deeper = {
               let (l, r) =
                 switch (from) {
                 | L when Sort.eq(s', s) => (l, Some(z.prec))
                 | R when Sort.eq(s', s) => (Some(z.prec), r)
                 | _ => (None, None)
                 };
               go(~l?, ~r?, s');
             };
             GWalk.Set.union_all([entered_here, entered_deeper]);
           }
         )
      |> GWalk.Set.union_all;
    };
  };
  go(~l?, ~r?, s);
};

let step_neq = (d: Dir.t, bound: Bound.t(GMtrl.t)): GWalk.Set.t => {
  let b = Dir.toggle(d);
  switch (bound) {
  | Root => enter(~from=b, Sort.root)
  | Node(Space) => GWalk.Set.empty
  | Node(Grout(tips)) =>
    switch (Dir.choose(d, tips)) {
    | Convex => GWalk.Set.empty
    | Concave =>
      // todo: come back and add more fine-grained filtering
      // on entry walks for dynamic matching forms
      Sort.all |> List.map(enter(~from=b)) |> GWalk.Set.union_all
    }
  | Node(Tile(Unmolded(tips))) =>
    switch (Dir.choose(d, tips)) {
    | Convex => GWalk.Set.empty
    | Concave =>
      Sort.all |> List.map(enter(~from=b, ~bound=Prec.top)) |> GWalk.Set.union_all
    }
  | Node(Tile(Molded(m))) =>
    GMold.to_atom(m).zipper
    |> Regex.step(d)
    |> List.map(
         fun
         | (Atom.Tok(_), _) => GWalk.Set.empty
         | (Kid(s), ctx) => {
             let (l, r) =
               switch (from) {
               | L when Sort.eq(s, m.sort) => (Some(m.prec), None)
               | R when Sort.eq(s, m.sort) => (None, Some(m.prec))
               | _ => (None, None)
               };
             enter(~from=b, ~l?, ~r?, s);
           },
       )
    |> GWalk.Set.union_all
  }
};

let walk_neq = (d: Dir.t, m: GMtrl.t): GWalk.Set.t => {
  let seen = Hashtbl.create(100);
  let rec go = m =>
    switch (Hashtbl.find_opt(seen, m)) {
    | Some () => GWalk.Set.empty
    | None =>
      Hashtbl.add(seen, m, ());
      open GWalk.Set.Syntax;
      let* t_hd = step_neq(d, m);
      {
        let* t_tl = go(GTerr.face(t_hd));
        return(cat_t(d, t_hd, t_tl));
      }
      |> GWalk.Set.add(t_hd);
    };
  go(m);
};