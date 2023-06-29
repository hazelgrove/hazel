// walk criteria
// - overall length
// - height
// - if there's a middle kid, whether there's a slot that accommodates

module GZipper = {
  // a zipper into a Grammar.t
  type t('subj) = {
    sort: Sort.t,
    prec: Prec.t,
    zipper: Regex.Zipper.t('subj),
  };

  let map = failwith("todo");
  let map_opt = failwith("todo");
};

module Mold = {
  [@deriving (sexp, ord)]
  type t = GZipper.t(Label.t);
  module Ord = {
    type nonrec t = t;
    let compare = compare;
  };
  module Map = Map.Make(Ord);
  module Set = Set.Make(Ord);

  // mold imposes bound on neighbors
  let bound: (Dir.t, t) => Prec.Bound.t = failwith("todo");
};

// module Slot = {
//   type t = GZipper.t(Regex.t);

//   let empty = (~sort, ~prec) => GZipper.{sort, prec, zipper: (Regex.empty, Regex.Ctx.empty)};

//   // slot is bounded by neighboring molds
//   // let bound: (Dir.t, t) => option(Prec.Bound.t) = failwith("todo");
// };

module Piece = {
  // todo rename
  type t =
    | Grout
    | Tile(Mold.t);
};

// module Walk = {
//   type t = Chain.t(Slot.t, Piece.t);
// };

module Result = {
  include Result;
  type t = Result.t(Slope.Dn.t, Meld.t);
};

let union = Mold.Map.union((_, l, r) => Some(choose(~over?, l, r)));
let union_all = List.fold_left(union, Mold.Map.empty);

let is_operator = (operand_side: Dir.t, r: Regex.t) =>
  Regex.enter(~from=operand_side, r)
  |> List.exists(((a, _)) => Regex.Atom.is_kid(a));

// let walk = (l: Mold.t, r: Mold.t): list(Slope.Dn.t) => {
//   let p = failwith("todo");
//   let rec go = (~seen=Mold.Set.empty, m: Mold.t) =>
//     if (Mold.Set.mem(m, seen)) {
//       [];
//     } else if (Mold.eq(m, r)) {
//       // important to assume m is unseen, can't switch order
//       [Slope.of_piece(p(m))];
//     } else {
//       let stepped_eq =
//         step_eq(R, m)
//         |> List.concat_map(terr => {
//           let seen =
//             Terrace.molds(terr)
//             |> Mold.Set.of_list
//             |> Mold.Set.union(seen);
//           let (walked_eq, walked_lt) = go(~seen, )

//         })
//     };
//   go(l);
// }

module Stepped = {
  type t = (list(Terrace.R.t), list(Terrace.R.t));
  let cat = ((eq, lt), (eq', lt')) => (eq @ eq', lt @ lt');
  let concat = List.fold_left(cat, ([], []));
  let filter = f => TupleUtil.map2(List.filter(f));
};

let step_leq = (m: Mold.t): Stepped.t => {
  let rec step = (~lt=false, z: GZipper.t(Atom.t)) => {
    let last_kid =
      switch (z) {
      | (Tok(_), _) => Meld.empty()
      | (Kid(s), _) => failwith("todo mk grout with s");
      };
    Regex.step(R, z.zipper)
    |> List.map(stop_or_step(~lt, ~last_kid))
    |> Stepped.concat
  }
  and stop_or_step = (~lt, ~last_kid, z: GZipper.t(Atom.t)) =>
    switch (z.zipper) {
    | (Kid(s), _) =>
      let bound = Sort.eq(l.sort, s) ? bound : Prec.min;
      Grammar.enter(~from=L, ~bound, s)
      |> List.map(stop_or_step(~lt=true, ~last_kid))
      |> Stepped.concat
      |> Stepped.cat(step(~lt, z))
    | (Tok(lbl), ctx) =>
      let m = failwith("todo of lbl ctx sort prec");
      let p = failwith("todo using m");
      let ts = [Terrace.{mel: last_kid, wal: Wald.of_piece(p)}];
      lt ? ([], ts) : (ts, []);
    };
  step(Mold.to_atom(z));
};
// todo: maybe incorporate kid sort to sort results
let leq = (l: Mold.t, r: Mold.t): Stepped.t =>
  step_leq(l)
  |> Stepped.filter(t => Mold.eq(r, Terrace.R.face(t).mold));

let rec walk_leq = (~stepped_to=Mold.Set.empty, m: Mold.t): list(Slope.Dn.t) =>
  if (Mold.Set.mem(m, stepped_to)) {
    [];
  } else {
    let p = failwith("todo mk piece from m");
    let (eq, lt) = step_leq(m);
    let walk_tl = t => {
      let n = Terrace.face(t).mold;
      walk_leq(~stepped_to=Mold.Set.add(n, stepped_to), n);
    };
    let walked_eq =
      eq
      |> List.concat_map(walk_tl)
      |> List.map(Slope.Dn.map_top(Terrace.R.link(p)));
    let walked_lt =
      lt
      |> List.concat_map(walk_tl)
      |> List.map(Slope.Dn.cons(Terrace.of_piece(p)));
    walked_eq @ walked_lt;
  };
// todo: maybe incorporate kid sort to sort results
let leq_trans = (l: Mold.t, r: Mold.t): list(Slope.t) =>
  walk_leq(l)
  |> List.filter(s => Mold.eq(r, Slope.Dn.face(s).mold));

// doesn't include molder
let leq = (l: Mold.t, r: Mold.t): list(Terrace.R.t) => {
  let (_, bound) = Mold.bounds(l);
  let rec step = (z: GZipper.t(Atom.t)) =>
    Regex.step(R, z.zipper)
    |> List.concat_map(stop_or_continue(~k=step))
  and stop_or_step = (z: GZipper.t(Atom.t)) =>
    switch (z.zipper) {
    | (Kid(s), _) =>
      let bound = Sort.eq(l.sort, s) ? bound : Prec.min;
      Grammar.enter(~from=L, ~bound, s)
      |> List.concat_map(stop_or_step)
      |> List.append(step(z))
    | (Tok(lbl), ctx) =>
      let m = failwith("todo of lbl ctx sort prec");
      if (!Mold.eq(m, r)) {
        []
      } else {
        let mel =
          switch (z) {
          | (Tok(_), _) => Meld.empty()
          | (Kid(s), _) => failwith("todo mk grout with s");
          };
        let p = failwith("todo using m");
        [Terrace.{mel, wal: Wald.of_piece(p)}];
      };
    };
  step(Mold.to_atom(l));
};

let leq_trans = (l: Mold.t, r: Mold.t): list(Terrace.R.t) => {
  let rec go = (z: GZipper.t(Atom.t)) =>

}

let lt = (l: Mold.t, r: Mold.t): list(Terrace.R.t) => {
  let rec go = (z: Regex.Zipper.t(Atom.t)) =>
    Regex.step(R, z)
    |> List.concat_map(z' =>
      switch (z') {
      | (Tok(_), _) => []
      | (Kid(s), ctx) =>
        let (_, bound) = Mold.bounds(l);
        Grammar.enter(~from=L, ~bound, s)
        |> List.concat_map(z'' =>

        )
      }
    )
};


let walk = (l: Mold.t, r: Mold.t): list(Slope.Dn.t) => {
  let rec step = (~stepped=Mold.Set.empty, m: Mold.t) =>
    if (!Mold.eq(m, r)) {

    } else {

    }
};


let walk = (l: Mold.t, r: Mold.t): list(Slope.Dn.t) => {
  // processed contains all molds we've previously popped and processed
  // m has just been popped from the bfs queue, now need to process (we know it hasn't been processed)


  // l plus molding r plus
  // case 1: m is l (processed is empty)
  //   want to take a step
  // case 2: m is r (so l is in processed)
  //   want to return singleton slope r

  // l != r
  // case a: m is l and l is in processed
  //   want to return []

  // ---

  // case 1: m != r
  //     m is in processed
  //       ==> []
  //     m not in processed
  //       ==> take a step
  // case 2: m == r
  //   m == l
  //     m is in processed
  //       ==> singleton
  //     m is not in processed
  //       ==> take a step
  //   m != l
  //     m is in processed
  //       ==> []
  //     m is not in processed
  //       ==> singleton


  let rec process = (~processed=Mold.Set.empty, m: Mold.t) => {
    if (Mold.eq(m, r)) {
      [Slope.of_piece(p(m))]
    }
  };
  process(l);
};

let walk = (l: Mold.t, r: Mold.t): list(Slope.Dn.t) => {
  let p = failwith("todo");
  // assumes slope is nonempty and top terrace has empty meld
  // (thus safe to be overwritten by kid)
  let link_top = (~p=?, kid, slope) => failwith("todo");
  let rec go = (~seen=Mold.Set.empty, m: Mold.t) =>
    if (Mold.Set.mem(m, seen)) {
      [];
    } else if (Mold.eq(m, r)) {
      // important to assume m is unseen, can't switch order
      [Slope.of_piece(p(m))];
    } else {
      let stepped_eq =
        step_eq(R, m)
        |> List.concat_map(((over, n)) => {
          let kid = failwith("todo using over");
          n
          |> go(~seen=Mold.Set.add(m, seen))  // walk tl
          |> List.map(link_top(~p=p(m), kid)); // cons hd
        });
      let stepped_lt =
        step_lt(R, m)
        |> List.concat_map(((over, n)) => {
          let kid = failwith("todo using over");
          n
          |> go(~seen=Mold.Set.add(m, seen))
          |> List.map(link_top(kid))
          |> List.map(Slope.Dn.cons(Terrace.of_piece(p(m))))
        });
      stepped_eq @ stepped_lt;
    };
  go(l);
};





let step_eq = (
  z: GZipper.t(Atom.t)
): Mold.Map.t(list(Slope.t)) =>
  Regex.step(R, z.zipper)
  |> List.map(
    fun
    | (T)
  )
  |> union_all(over?);

let rec walks = (~seen=Mold.Set.empty, m: Mold.t): Mold.Map.t(list(Slope.t)) =>
  if (Mold.Set.mem(m, seen)) {
    Mold.Map.empty
  } else {
    let walk_tls = m =>
      m
      |> walks(~seen=Mold.Set.add(m, seen))
      |> Mold.Map.map(List.map())
    failwith("todo");
  };

let walk = (t: Terrace.R.t, ~kid=Meld.empty(), m: Mold.t): option(Slope.t) =>
  walks(t)
  |> Walks.get(m) // todo
  |> List.map(plug(kid))
  |> List.stable_sort((l, r) =>
    switch (l, r) {
    | (Error(l), Error(r)) => Slope.compare_size(l, r)
    | (Ok(l), Ok(r)) => Slope.compare_size(l, r)
    | (Error(_), Ok(_)) => 1
    | (Ok(_), Error(_)) => -1
    }
  )
  |> ListUtil.hd_opt;

// step to nearest molds within same regex, returning
// map from destination molds to slopes representing walk
// from starting mold
let rec step_eq = (
  ~stepped: Slope.t,
  ~over=?,
  d: Dir.t,
  z: GZipper.t(Atom.t),
): Mold.Map.t(Slope.t) => {
  let kid =
    switch (z) {
    // only "last" kid matters bc operator form
    | (Kid(s), _) => failwith("todo mk grout")
    | (Tok(_), _) => Meld.empty()
    };
  Regex.step(d, z.zipper)
  |> List.map(arrive(~stepped, ~kid, ~over?, ~from=Dir.toggle(d)))
  |> union_all(over?);
}
// stop walk at z if at a mold, otherwise continue walk
and arrive = (~stepped: Slope.t, ~kid=Meld.empty(), ~over=?, ~from: Dir.t, z: Regex.Zipper.t(Atom.t)) =>
  switch (z') {
  | (Kid(_), _) => step_eq(~stepped, ~over?, Dir.toggle(from), z')
  | (Tok(lbl), ctx) =>
    let m = {sort: z.sort, prec: z.prec, zipper: (lbl, ctx)};
    let p = Piece.mk(~id=Id.Gen.dummy, T(Tile.mk(m)));
    let stepped = Slope.push(stepped, ~kid, p);
    Mold.Map.singleton(m, stepped);
  };

// enter regexes of given sort if precedence-correct wrt stepped slope
let rec enter = (
  ~stepped: Slope.t,
  ~over=?,
  ~from: Dir.t,
  s: Sort.t,
): Mold.Map.t(Slope.t) => {
  let face = Slope.face(seen);
  let (_, bound) = Mold.bounds(face);
  Grammar.get(s)
  |> Prec.Table.filter((p, a, r) =>
    !is_operator(from, r)
    || Prec.lower_bounded(~a, ~side=from, p, bound)
  )
  |> Prec.Table.map((p, _, r) =>
    Regex.enter(~from, r)
    |> List.map(arrive(~stepped, ~over?, ~from))
    |> union_all(~over?)
  )
  |> union_all(~over?);
};



let step_lt = (
  ~stepped: Slope.t,
  ~over=?,
  d: Dir.t,
  z: GZipper.t(Atom.t),
): Mold.Map.t(Slope.t) => {




  let rec go = (z: GZipper.t(Atom.t)) => {
    Regex.step(d, z.zipper)
    |> List.map((z': Regex.Zipper.t(Atom.t)) =>
      fun
      | ()
      | (Tok(lbl), ctx) =>
    )
  }

}

let step = (
  ~seen: Slope.t,
  ~over: option(Sort.t)=?,
  d: Dir.t,
  z: GZipper.t(Atom.t),
): Mold.Map.t(Slope.t) => {
  let meld_since_seen =
    switch (z) {
    | (Kid(s), _) => failwith("todo mk grout")
    | (Tok(_), _) => Meld.empty
    };
  Regex.step(d, z.zipper)
  |> List.map((z: Regex.Zipper.t(Atom.t)) =>
    switch (z) {
    | (Tok(lbl), ctx) =>
      let m = {...mold, zipper: (lbl, ctx)};
      let p = Piece.mk(~id=Id.Gen.dummy, T(Tile.mk(m)));
      let slope = Slope.push(seen, meld_since_seen, p);
      Mold.Map.singleton(m, slope);
    | (Kid(s), _) =>
      let entered = enter(~seen, ~from=Dir.toggle(d), s);
      let stepped = step(~seen, ~over?, d, z);
      failwith("todo union entered and stepped using over");
    }
  );
}
and enter = (
  ~seen: Slope.t,
  ~from: Dir.t,
  s: Sort.t,
): Mold.Map.t(Slope.t) => {
  let face = Slope.face(seen);
  let (_, bound) = Mold.bounds(face);
  Grammar.get(s)
  |> Prec.Table.filter((p, a, r) =>
    is_operator(from, r)
    && Prec.lower_bounded(~a, ~side=from, p, bound)
  )
  |> Prec.Table.map((p, _, r) =>
    Regex.enter(~from, r)
    |> List.map((z: Regex.Zipper.t(Atom.t)) =>
      switch (z) {
      | (Tok(lbl), ctx) =>
        let m = {sort: s, prec: p, zipper: (lbl, ctx)};
        let p = Piece.mk(~id=Id.Gen.dummy, T(Tile.mk(m)));
        let t = Terrace.of_piece(p);
        let slope = {...slope, terrs: [t, ...slope.terrs]};
        Mold.Map.singleton(m, slope);
      | (Kid(_), _) =>


      }
    )
  )


  Grammar.enter(~from: ~l=bound, s)
  |> List.map(r)
}


let rec step = (
  ~seen: Slope.t,
  d: Dir.t,
  z: Regex.Zipper.t(Atom.t),
  ~over: option(Sort.t),
): Mold.Map.t(Slope.t) => {
  let meld =
    switch (z) {
    | (Tok(_), _) => Meld.empty
    | (Kid(s), _) => failwith("todo grout")
    };
  Regex.step(d, z)
  |> List.map(process_step(
    ~step=step(~seen, d),
    ~enter=enter(~seen, ~from=Dir.toggle(d))
  ))
  |> union_all;
}
and enter = (
  ~seen: Slope.t,
  ~from: Dir.t,
  s: Sort.t,
): Mold.Map.t(Slope.t) => {
  Grammar.get(~l=Slope.face(seen).mold.prec, s)
  |> List.map(r =>
    Regex.enter(~from, r)
    |> List.map(process_step(
      ~enter=enter(~seen, ~from),
      ~step=step(~seen, Dir.toggle(from))
    ))
    |> union_all
  )
  |> union_all
}
and process_step = (
  ~seen: Slope.t,
  ~enter,
  ~step,
  (a, ctx) as z: Regex.Zipper.t(Atom.t),
) => {
  switch (a) {
  | Tok(lbl) =>
    let m = {...mold, zipper: (lbl, ctx)};
    let p = Piece.mk(~id=Id.Gen.dummy, T(Tile.mk(m)));
    let slope = Slope.push(seen, meld, p);
    Mold.Map.singleton(m, slope);
  | Kid(s) =>
    let stepped_lt = enter(s);
    let stepped_eq = step(z, ~over=Some(s));
    union(stepped_lt, stepped_eq);
  };
};


let walk = (
  ~seen=Mold.Set.empty,
  ~over=?,
  d: Dir.t,
  m: Mold.t,
): Mold.Map.t(Walk.t) =>
  if (Mold.Set.mem(m, seen)) {
    Mold.Map.empty
  } else {
    let walk_tl = m =>
      m
      |> walk(~seen=Mold.Set.add(m, seen), ~d)
  }

let step = (
  d: Dir.t,
  mold: Mold.t,
  // to guide choice between competing slopes
  kid: option(Sort.t),
): Mold.Map.t(Slope.t) => {
  let mk_meld: option(Sort.t) => Meld.t = failwith("todo");
  let union = Mold.Map.union((m, l, r) => Some(choose(~kid?, l, r)));
  let rec go = (~seen: option(Sort.t)=?, ~slope, zipper: Regex.Zipper.t(Atom.t)) =>
    Regex.step(d, zipper)
    |> List.map(
      ((a, ctx) as z: Regex.Zipper.t(Atom.t)) =>
        switch (a) {
        | Tok(lbl) =>
          let m = {...mold, zipper: (lbl, ctx)};
          let meld = mk_meld(seen);
          let p = Piece.mk(~id=Id.Gen.dummy, T(Tile.mk(m)));
          let slope = Slope.push(slope, meld, p);
          Mold.Map.singleton(m, slope);
        | Kid(s) =>
          let stepped_lt = enter(~from=Dir.toggle(d), s);
          let stepped_eq = go(~seen=s, ~slope, z);
          union(stepped_lt, stepped_eq);
        }
    )
    |> List.fold_left(union, Mold.Map.empty);

};


// let mk_slope = (terr: Terrace.R.t, kid: Meld.t, walk: Walk.t, m: Mold.t) => {
//   let init = Terrace.Acc.init(~mel=terr.mel, ~wal=terr.wal);
//   let fit_and_
//   walk
//   |> List.fold_left(
//     slot =>

//   )
// }

module Terrace = {
  let mold =
      (terr: Terrace.R.t, ~kid=Meld.empty, t: Token.t)
      : Result.t => {
    let face = Terrace.face(terr);
    let sort = Mold.sort(kid);
    let walks =
      Molds.get(t)
      |> List.filter_map(Walks.get(face.mold, ~toward=R, ~kid=?sort))
      |>

  }
};


module Terrace = {
  let mold =
      (terr: Terrace.R.t, ~kid=Meld.empty, t: Token.t)
      : Result.t => {
    open Result.Syntax;
    // need to merge terr and kid and possibly complete when kid empty
    let r = Result.of_option(~error=failwith("todo"));
    // let walks = Walks.from(R, Terrace.face(terr).mold);
    let molds = Molds.get(t);

    // for each possible mold, look up whether there are any walks to it from terr
    // if none, then rm from consideration bc precedence-incorrect
    // if some,
      // find the first that matches the kid
      // if none, return the first

    // sort the resulting walk-mold pairs according to
      // whether the walk matches the kid
      // the height of the walk (maybe taking into account concave grouting)
    // return the first one if available

    let face = Terrace.face(terr);
    let sort = Meld.sort(kid);
    let walks =
      Molds.get(t)
      |> List.filter_map(
        Walks.get(face.mold, ~toward=R, ~kid=?Meld.sort(kid))
      )
      |> List.stable_sort(Walk.compare(~kid=?sort));

    ListUtil.hd_opt(walks)
    |> Result.of_option(~error=)


    let candidate_walks =
      molds
      |> List.filter_map(m =>
        walks
        |> Walks.get(m)
        |>

        // some annoyance about wanting to return ziggurats
        // but wanting to still use walks for choosing optimal...
        // but maybe can choose based on ziggurat...?
        // no... the kid may be turned into terraces and inflate height
        |> List.find_opt(Walk.takes())


        |> Walk.choose(~kid=?Meld.sort(kid))
        |> Option.map(w => (w, m))
      )
  }
};

module Slope = {
  let rec mold =
      (slope: Slope.Dn.t, ~candidates=[], ~kid=Meld.empty, t: Token.t)
      : Result.t => {
    // recurse through slope terraces
    // if none left, then check saved walk-mold pairs and pick best one
    // for each one, call `Terrace.mold`
      // if successful result with empty walk (or kid-consistent walk), then return that walk and mold
      // else, save walk-mold pair if successful, wrap kid with terrace, and recurse
    switch (slope.terrs) {
    | [] =>
      ListUtil.hd_opt(candidates)
      |> Result.of_option(~error=kid)
    | [terr, ...terrs] =>
      // want to pass sort of kid into Walk.matches, but don't think I've
      // considered all the "None" cases
      // eg when the meld is empty so has no sort
      // vs when the meld is something like an unrecognized token...
      // ...well actually I guess I'm always assigning sorts to those atm...
      // ...so the only None case should be when the meld is empty
      let sort = Meld.sort(kid);
      switch (Terrace.mold(terr, ~kid, t)) {
      | Ok((walk, mold)) when Walk.matches(~exact=true, sort, walk) =>
        Ok((walk, mold))
      | Ok((walk, mold)) =>
        let slope = {...slope, terrs};
        // worst case quadratic
        let candidates = List.merge(((l, _), (r, _)) => Walk.compare(~sort?, l, r), [(walk, mold)], candidates);
        let kid = Terrace.R.unmk(terr, kid);
        mold(slope, ~candidates, ~kid, t);
      };
    };
  };
};
