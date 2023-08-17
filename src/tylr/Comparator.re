module Ineq = {
  type t = {
    eq: list(Slope.t(Material.molded)),
    neq: list(Slope.t(Material.molded)),
  };
  let empty = {eq: [], lt: []};
  let cat = ({eq, neq}, {eq: eq', neq: neq'}) => {
    eq: eq @ eq',
    neq: neq @ neq',
  };
  let concat = List.fold_left(cat, empty);
  let filter = f => TupleUtil.map2(List.filter(f));
  let concat_map: (Slope.t => list(Slope.t), t) => t = failwith("todo");
  // let complete: (~from: Terrace.R.t, t) => list(Slope.t) = failwith("todo");
};

module Leq = {
  include Ineq; // slopes are dn
  let pick: (Material.molded, t) => option(Slope.Dn.t(Material.molded)) =
    failwith("todo");
};

module Geq = {
  include Ineq; // slopes are up
  let pick:
    (
      ~to_: Material.molded,
      ~over: Material.sorted,
      ~from: Material.molded,
      t
    ) =>
    option(Slope.Up.t(Material.molded)) =
    failwith("todo");
};

let step = (d: Dir.t, m: Material.molded): Ineq.t => {
  let rec go = (z: Gram.Zipper.t(Atom.t)) =>
    z.zipper |> Regex.step(d) |> List.map(stop_or_go) |> Result.concat
  and stop_or_go = z =>
    switch (z.zipper) {
    // stop
    | (Tok(lbl), ctx) =>
      let m = failwith("todo mold of lbl ctx sort prec");
      let p = failwith("todo piece of m");
      {lt: [], eq: [Terrace.of_piece(p)]};
    // keep going
    | (Kid(s), _) =>
      // todo: fix unbound `bound`, compute from kid + ctx
      let bound = Sort.eq(l.sort, s) ? bound : Prec.min;
      let stepped_lt =
        Gram.enter(~from=L, ~bound, s)
        |> List.map(stop_or_go)
        |> Result.concat
        |> Result.newline;
      let g = failwith("todo mk grout meld for s");
      let stepped_eq = go(z) |> Result.map(Terrace.R.put_mel(g));
      Result.cat(stepped_eq, stepped_lt);
    };
  // todo: need to handle grout
  go(Mold.to_atom(m));
};

module Result = {
  type t('lt, 'eq, 'gt) =
    | Lt('lt)
    | Eq('eq)
    | Gt('gt);

  let map = (f_lt, f_eq, f_gt) =>
    fun
    | Lt(lt) => f_lt(lt)
    | Eq(eq) => f_eq(eq)
    | Gt(gt) => f_gt(gt);
};

// `leq(l, ~kid, r)` steps right from `l` and filters the result
// to those concluding with `r` and accommodating `kid`
// postcond: slope is nonempty, top terrace has empty mold
let leq =
    (l: Material.molded, ~kid: option(Material.sorted)=?, r: Material.molded)
    : option(Slope.Dn.m) =>
  step(R, l)
  |> Leq.filter(failwith("todo only steps ending with r"))
  |> Leq.filter(failwith("todo only slopes that take kid"))
  |> Leq.pick(l);

// `geq(l, ~kid, r)` steps left from `r` and filters the result
// to those concluding with `l` and accommodating `kid`
// postcond: slope is nonempty, top terrace has empty mold
let geq =
    (l: Material.molded, ~kid: option(Material.sorted)=?, r: Material.molded)
    : option(Slope.Up.m) =>
  step(L, r)
  |> Geq.filter(failwith("todo only steps ending with r"))
  |> Geq.filter(failwith("todo only slopes that take kid"))
  |> Geq.pick(r);

let cmp =
    (l: Material.molded, ~kid: option(Material.sorted)=?, r: Material.molded)
    : option(Result.t(Slope.Dn.m, Wald.m, Slope.Up.m)) =>
  switch (leq(l, ~kid?, r), geq(l, ~kid?, r)) {
  | (None, None) => None
  | (
      Some({terrs: [{wal: leq, _}], _}),
      Some({terrs: [{wal: geq, _}], _}),
    )
      when Wald.eq(leq, geq) =>
    Some(Eq(leq))
  // may need to validate some unique precedence relation invariant here
  | (Some(lt), _) => Some(Lt(lt))
  | (_, Some(gt)) => Some(Gt(gt))
  };
