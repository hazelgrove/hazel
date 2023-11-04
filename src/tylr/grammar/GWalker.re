module Walk = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = {
    eq: option(GTerrace.t),
    // top-down
    neq: GSlope.t,
  };

  let eq = t => {eq: Some(t), neq: []};

  let steps_eq = w => w.eq |> Option.map(GTerrace.root) |> Option.value(~default=[]);
  let steps_neq = w => List.map(GTerrace.root, w.neq);
  let steps = w => [steps_eq(w), ...steps_neq(w)];

  let no_tiles_above_last = w =>
    switch (ListUtil.split_last_opt(steps(w))) {
    | None => true
    | Some((above, _)) =>
      List.concat(above)
      |> List.filter_map(Material.is_tile)
      |> List.is_empty
      |> Bool.not
    };

  let no_tiles_across_last = w =>
    switch (ListUtil.split_last_opt(steps(w))) {
    | None => true
    | Some((_, last)) =>
      switch (ListUtil.split_last_opt(last)) {
      | None => true
      | Some((across, _)) =>
        across
        |> List.filter_map(Material.is_tile)
        |> List.is_empty
        |> Bool.not
      }
    };

  let eq = w => Option.is_some(w.eq) && List.is_empty(w.neq);
  let neq = w => !List.is_empty(w.neq);

  let lt = w => neq(w) && no_tiles_above_last(w) && no_tiles_across_last(w);
  let gt = w => neq(w) && no_tiles_above_last(w);

  let leq = w => lt(w) || eq(w);
  let geq = w => gt(w) || eq(w);

  let face = walk => {
    open OptUtil.Syntax;
    let/ () = GSlope.face(walk.neq);
    Option.map(GTerrace.face, walk.eq);
  };
};

module Walked = {
  include Material.Map;
  type t = Material.Map(list(Walk.t));

  let add = (walk) =>
    update(
      Walk.face(walk),
      fun
      | None => [walk]
      | Some(walks) => ListUtil.dedup([walk, ...walks]),
    );

  let single = (walk: Walk.t) =>
    switch (Walk.face(walk)) {
    | None => empty
    | Some(m) => singleton(m, [walk])
    };
  let many = List.fold_left(Fun.flip(add), empty);

  let find = (m, map) =>
    switch (find_opt(m, map)) {
    | None => []
    | Some(ws) => ws
    };

  let union = union((_, l, r) => ListUtil.dedup(l @ r));
  let union_all = List.fold_left(union, empty);

  let filter = (p: Walk.t => bool) => map(List.filter(p));

  let bind = (stepped: t, f: Walk.t => t) =>
    {
      open ListUtil.Syntax;
      let* (_, ws) = bindings(stepped);
      let* w = ws;
      f(w);
    }
    |> union_all;

  module Syntax = {
    let return = single;
    let ( let* ) = bind;
  };
};

let rec step_to_mold =
        (~slot=GSlot.Empty, d: Dir.t, a: GZipper.t(Atom.t)): Walked.t =>
  switch (Mold.of_atom(a)) {
  | Ok(m) =>
    Walked.single(Walk.eq(GTerrace.singleton(~slot, Tile(Molded(m)))))
  | Error(s) =>
    Regex.step(d, a.zipper)
    |> List.map(zipper =>
      find_tok(~slot=Full(Some(s)), {...a, zipper})
    )
    |> Walked.union_all
  };

let step_eq = (d: Dir.t, b: Bound.t(Material.t)): Walked.t =>
  switch (b) {
  | Root
  | Node(Space) => Walked.empty
  | Node(Grout(tips)) =>
    switch (Dir.choose(d, tips)) {
    | Convex => Walked.empty
    | Concave =>
      let slot = GSlot.Full(None);
      let infix = Material.Grout((Concave, Concave));
      let affix =
        Material.Grout(
          Dir.choose(d, ((Convex, Concave), (Concave, Convex))),
        );
      Walked.many([
        Walk.eq(GTerrace.singleton(~slot, infix)),
        Walk.eq(GTerrace.singleton(~slot, affix)),
      ]);
    }
  | Node(Tile(Unmolded(tips))) =>
    switch (Dir.choose(d, tips)) {
    | Convex => Walked.empty
    | Concave =>
      let slot = GSlot.Full(None);
      let infix = Material.Tile(Unmolded((Concave, Concave)));
      let affix =
        Material.Tile(
          Unmolded(Dir.choose(d, ((Convex, Concave), (Concave, Convex))))
        );
      Walked.many([
        Walk.eq(GTerrace.singleton(~slot, infix)),
        Walk.eq(GTerrace.singleton(~slot, affix)),
      ])
    }
  | Node(Tile(Molded(m))) =>
    let a = Mold.to_atom(m);
    Regex.step(d, a)
    |> List.map(zipper => step_to_mold(d, {...a, zipper}))
    |> Walked.union_all;
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
         Regex.Zipper.(enter(~from=L, rgx), enter(~from=R, rgx));
       has_kid(ls) && !l_bounded || has_kid(rs) && !r_bounded
         ? []
         : Dir.choose(from, (ls, rs))
           |> List.map(GZipper.mk(~sort=s, ~prec=p))
     })
  |> List.concat;

// deep precedence-bounded entry into given sort and its unidelimited
// dependencies, stepping to nearest token (cf G)
let enter = (~from: Dir.t, ~l=?, ~r=?, s: Sort.t): Walked.t => {
  let seen = Hashtbl.create(100);
  let rec go = (~l=?, ~r=?, s: Sort.t) => {
    switch (Hashtbl.find_opt(seen, (l, r, s))) {
    | Some(_) => Walked.empty
    | None =>
      Hashtbl.add(seen, (l, r, s), ());
      let _ = failwith("todo: add entry to space/grout/unmolded");
      // todo: add entry to grout/unmolded tiles
      step_enter(~from, ~l?, ~r?, s)
      |> List.map(z =>
           switch (Mold.of_atom(z)) {
           | Ok(m) => Walked.single(GTerrace.singleton(m))
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
             Walked.union([entered_here, entered_deeper]);
           }
         )
      |> Walked.union;
    };
  };
  go(~l?, ~r?, s);
};

let step_neq = (d: Dir.t, bound: Bound.t(Material.t)): Walked.t => {
  let b = Dir.toggle(d);
  switch (bound) {
  | Root => enter(~from=b, Sort.root)
  | Node(Space) => Walked.empty
  | Node(Grout(tips)) =>
    switch (Dir.choose(d, tips)) {
    | Convex => Walked.empty
    | Concave =>
      // todo: come back and add more fine-grained filtering
      // on entry walks for dynamic matching forms
      Sort.all |> List.map(enter(~from=b)) |> Walked.union_all
    }
  | Node(Tile(Unmolded(tips))) =>
    switch (Dir.choose(d, tips)) {
    | Convex => Walked.empty
    | Concave =>
      Sort.all |> List.map(enter(~from=b, ~bound=Prec.top)) |> Walked.union_all
    }
  | Node(Tile(Molded(m))) =>
    Mold.to_atom(m).zipper
    |> Regex.step(d)
    |> List.map(
         fun
         | (Atom.Tok(_), _) => Walked.empty
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
    |> Walked.union
  }
};

let step = (d: Dir.t, bound: Bound.t(Material.t)) =>
  Walked.union(step_eq(d, bound), step_neq(d, bound));

let walk = (d: Dir.t, bound: Bound.t(Material.t)) => {
  let seen = Hashtbl.create(100);
  let rec go = bound =>
    switch (Hashtbl.find_opt(seen, bound)) {
    | Some () => Walked.empty
    | None =>
      open Walked.Syntax;
      let* stepped = step(d, bound);
      Walked.add(stepped, {
        let* walked =
          Walk.face(stepped)
          |> Option.map(m => go(Node(m)))
          |> Option.value(~default=Walked.empty);
        return(Walk.cat(stepped, walked));
      });
    }
};

// OLD BELOW

module Walked = {
  include GMaterial.Map;
  // here i mean molded material
  type t = GMaterial.Map.t(GTerrace.Set.t);

  let find = (m, map) =>
    switch (find_opt(m, map)) {
    | Some(set) => set
    | None => GTerrace.Set.empty
    };

  let union =
    List.fold_left(union(GTerrace.Set.union), empty);

  let add = (t: GTerrace.t) =>
    update(
      GTerrace.face(t),
      fun
      | None => GTerrace.Set.singleton(t)
      | Some(set) => GTerrace.Set.add(t, set),
    );

  let single = (t: GTerrace.t) =>
    singleton(GTerrace.face(t), GTerrace.Set.singleton(t));

  let bind = (stepped: t, f: GTerrace.t => t) =>
    ListUtil.Syntax.(
      {
        let* (_, ts) = bindings(stepped);
        let* t = GTerrace.Set.elements(ts);
        f(t);
      }
    )
    |> union;

  module Syntax = {
    let return = single;
    let ( let* ) = bind;
  };
};

module Descended = {
  include GMaterial.Map;
  type t = GMaterial.Map.t(GSlope.Set.t);

  let of_walked = GMaterial.Map.map(ts => GTerrace.Set.map(t => [t], ts));

  let find = (m, map) =>
    switch (find_opt(m, map)) {
    | None => GSlope.Set.empty
    | Some(set) => set
    };

  // empty if s empty
  let single = (s: GSlope.t) =>
    switch (GSlope.face(s)) {
    | None => empty
    | Some(m) => singleton(m, GSlope.Set.singleton(s))
    };

  let bind = (desc: t, f: GSlope.t => t) =>
    {
      open ListUtil.Syntax;
      let* (_, ss) = bindings(desc);
      let* s = GSlope.Set.elements(ss);
      f(s);
    }
    |> union;
};

let rec find_tok =
        (~slot=Slot.Empty, d: Dir.t, a: GZipper.t(Atom.t)): Walked.t =>
  switch (Mold.of_atom(a)) {
  | Ok(m) => Walked.single(GTerrace.singleton(~slot, m))
  | Error(s) =>
    Regex.step(d, a)
    |> List.map(find_tok(~slot=Full(Tile(s))))
    |> Walked.union
  };

let step_eq = (d: Dir.t, m: Material.Molded.t): Walked.t =>
  switch (m) {
  | Space => Walked.empty
  | Grout(tips) =>
    switch (Dir.choose(d, tips)) {
    | Convex => Walked.empty
    | Concave =>
      let infix = Material.Grout((Concave, Concave));
      let affix =
        Material.Grout(
          Dir.choose(d, ((Convex, Concave), (Concave, Convex))),
        );
      let slot = GSlot.Full(Grout());
      Walked.single(GTerrace.singleton(~slot, infix))
      |> Walked.add(GTerrace.singleton(~slot, affix));
    }
  | Tile(Unmolded(tips)) =>
    switch (Dir.choose(d, tips)) {
    | Convex => Walked.empty
    | Concave =>
      let infix = Material.Tile(Unmolded((Concave, Concave)));
      let affix =
        Material.Tile(
          Unmolded(Dir.choose(d, ((Convex, Concave), (Concave, Convex)))),
        );
      let slot = GSlot.Full(None);
      Walked.single(GTerrace.singleton(~slot, infix))
      |> Walked.add(GTerrace.singleton(~slot, affix));
    }
  | Tile(Molded(m)) =>
    Mold.to_atom(m).zipper
    |> Regex.step(d)
    |> List.map(find_tok(d))
    |> Walked.union
  };

let cat_t = (d: Dir.t, t_hd, t_tl) =>
  switch (d) {
  | L => GTerrace.L.cat(t_tl, t_hd)
  | R => GTerrace.R.cat(t_hd, t_tl)
  };

let walk_eq = ((d: Dir.t, m: Material.Molded.t)): Walked.t => {
  let seen = Hashtbl.create(100);
  let rec go = m =>
    switch (Hashtbl.find_opt(seen, m)) {
    | Some () => Walked.empty
    | None =>
      Hashtbl.add(seen, m, ());
      open Walked.Syntax;
      let* t_hd = step_eq(d, m);
      {
        let* t_tl = go(GTerrace.face(t_hd));
        return(cat_t(d, t_hd, t_tl));
      }
      |> Walked.add(t_hd);
    };
  go(m);
};
let walk = Core.Memo.general(walk_eq) |> FunUtil.curry2;

let step_neq = (d: Dir.t, bound: Bound.t(Material.Molded.t)): Walked.t => {
  let b = Dir.toggle(d);
  switch (bound) {
  | Root => enter(~from=b, Sort.root)
  | Piece(Space)
  | Piece(Grout((_, Convex))) => Walked.empty
  | Piece(Grout((_, Concave))) =>
    // todo: come back and add more fine-grained filtering
    // on entry walks for dynamic matching forms
    Sort.all |> List.map(enter(~from=b)) |> Walked.union
  | Piece(Tile(Unmolded((_, Convex)))) => Walked.empty
  | Piece(Tile(Unmolded((_, Concave)))) =>
    Sort.all |> List.map(enter(~from=b, ~bound=Prec.top)) |> Walked.union
  | Piece(Tile(Molded(m))) =>
    Mold.to_atom(m).zipper
    |> Regex.step(d)
    |> List.map(
         fun
         | (Atom.Tok(_), _) => Walked.empty
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
    |> Walked.union
  };
};

let walk_neq = (d: Dir.t, bound: Bound.t(Material.Molded.t)): Walked.t => {
  open Walked.Syntax;
  let* t_hd = step_neq(d, bound);
  {
    let* t_tl = walk_eq(d, GTerrace.face(t_hd));
    cat_t(d, t_hd, t_tl);
  }
  |> Walked.add(t_hd);
};

let rec descend = (d: Dir.t, bound: Bound.t(Material.Molded.t)): Descended.t => {
  let seen = Hashtbl.create(100);
  let rec go = bound =>
    switch (Hashtbl.find_opt(seen, bound)) {
    | Some () => Descended.empty
    | None =>
      open Descended.Syntax;
      let* s_upper = Descended.of_walked(walk_neq(d, bound));
      {
        let* s_lower = go(Piece(m_hd));
        GSlope.cat(s_lower, s_upper);
      }
      |> Descended.add(s_upper);
    };
  go(bound);
};
