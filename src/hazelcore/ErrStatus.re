module HoleReason = {
  /* Variable: `reason` */
  [@deriving sexp]
  type t =
    | TypeInconsistent
    | WrongLength;

  let eq = (x, y) => x == y;
} /* Variable: `err` */;

[@deriving sexp]
type t =
  | NotInHole
  | InHole(HoleReason.t, MetaVar.t);

let make_recycled_InHole: (t, HoleReason.t, IDGen.t) => (t, IDGen.t) =
  (err, reason, id_gen) =>
    switch (err) {
    | NotInHole =>
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (InHole(reason, u), id_gen);
    | InHole(_, u) => (InHole(reason, u), id_gen)
    };
