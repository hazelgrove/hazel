include Slope;
[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t = Slope.t(GMaterial.t, GMaterial.Sorted.t);

let consistent = (tips: Tip.s, a: Either.t(Molded.t, GSlot.t)) =>
  switch (a) {
  | Left(m) =>
    switch (m) {
    | Grout(tips') => tips' == tips
    | Space
    | Tile(_) => false
    }
  | Right(slot) =>
    switch (slot) {
    | Empty
    | Full(Space) => false
    | Full(Grout () | Tile(_)) => tips == (Convex, Convex)
    }
  };
let rec starts_with = (gs, atoms) =>
  switch (gs, atoms) {
  | ([], _) => true
  | ([_, ..._], []) => false
  | ([g, ...gs], [a, ...atoms]) =>
    consistent(g, a) && starts_with(gs, atoms)
  };

let accommodates = (slot: GSlot.t, slope: t) =>
  switch (slot) {
  | Empty => true
  | Full(s) => List.exists(Sorted.consistent(s), slots(slope))
  };

module Dn = {
  include Dn;

  let atoms = s => s |> List.rev_map(GTerrace.R.atoms) |> List.concat;

  let starts_with = (gs, s) => starts_with(gs, atoms(s));
};

module Up = {
  include Up;

  let atoms = List.concat_map(GTerrace.L.atoms);

  let starts_with = (gs, s) => starts_with(gs, atoms(s));
};

module Ord = {
  type nonrec t = t;
  let compare = compare;
};
module Set = {
  include Set.Make(Ord);

  // todo: choose according to grammar disjunction order
  let pick_dn = (~without=[], ~slot=GSlot.Empty, set: t) =>
    set
    |> filter(accommodates(slot))
    |> (without == [] ? Fun.id : filter(s => !Dn.starts_with(without, s)))
    |> choose_opt;
  let pick_up = (~without=[], ~slot=GSlot.Empty, set: t) =>
    set
    |> filter(accommodates(slot))
    |> (without == [] ? Fun.id : filter(s => !Up.starts_with(without, s)))
    |> choose_opt;
};
