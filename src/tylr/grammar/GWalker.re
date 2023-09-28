module Walked = {
  include Material.Map;
  // here i mean molded material
  type t = Material.Map.t(GTerrace.Set.t);

  let union =
    List.fold_left(Material.Map.union(GTerrace.Set.union), Mold.Map.empty);

  let add = (t: GTerrace.t) =>
    update(
      GTerrace.face(t),
      fun
      | None => Set.singleton(t)
      | Some(set) => Set.add(t, set),
    );

  let single = (t: GTerrace.t) =>
    singleton(GTerrace.face(t), Set.singleton(t));

  let bind = (stepped: t, f: GTerrace.t => t) =>
    ListUtil.Syntax.(
      {
        let* (_, ts) = bindings(stepped);
        let* t = GTerrace.Set.to_list(ts);
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
  include Material.Map;
  type t = Material.Map.t(GSlope.Set.t);

  let of_walked = Material.Map.map(GTerrace.Set.map(GSlope.single));

  let find = (m, map) =>
    switch (find_opt(m, map)) {
    | None => GSlope.Set.Empty
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
      let* s = GSlope.Set.to_list(ss);
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

// shallow precedence-bounded entry into given sort, stepping to
// all possible atoms at the entered edge across disjunctions
let enter = (~from: Dir.t, ~l=?, ~r=?, s: Sort.t): list(GZipper.t(Atom.t)) =>
  Grammar.v
  |> Sort.Map.find(s)
  |> Prec.Table.map((p, a, rgx) => {
       let has_kid = List.exists(((atom, _)) => Atom.is_kid(atom));
       let l_bounded =
         switch (l) {
         | Some((s_l, p_l)) when Sort.eq(s_l, s) => Prec.lt(~a, p_l, p)
         | _ => true
         };
       let r_bounded =
         switch (r) {
         | Some((s_r, p_r)) when Sort.eq(s, s_r) => Prec.gt(~a, p, p_r)
         | _ => true
         };

       let (ls, rs) =
         Regex.Zipper.(enter(~from=L, rgx), enter(~from=R, rgx));
       has_kid(ls) && !l_bounded || has_kid(rs) && !r_bounded
         ? [] : Dir.choose(from, (ls, rs));
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
      GZipper.enter(~from, ~l?, ~r?, s)
      |> List.map(z =>
           switch (Mold.of_atom(z)) {
           | Ok(m) => Walked.single(GTerrace.singleton(m))
           | Error(s') =>
             let entered_here = find_tok(Dir.toggle(from), z);
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
      let slot = GSlot.Full(Grout());
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
