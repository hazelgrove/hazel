module HoleReason = {
  /* Variable: `reason` */
  [@deriving (sexp, show)]
  type t =
    | TypeInconsistent
    | WrongLength;

  let eq = (x, y) => x == y;
};

/* Variable: `err` */
[@deriving (sexp, show)]
type t =
  | NotInHole
  | InHole(HoleReason.t, MetaVar.t);

let make_recycled_InHole: (t, HoleReason.t, MetaVarGen.t) => (t, MetaVarGen.t) =
  (err, reason, u_gen) =>
    switch (err) {
    | NotInHole =>
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (InHole(reason, u), u_gen);
    | InHole(_, u) => (InHole(reason, u), u_gen)
    };
