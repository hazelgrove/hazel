open Sexplib.Std;

module SubReason = {
  [@deriving sexp]
  type t =
    | InsufficientParams
    | DecodingError
    | IllTypedExpansion;
};

module HoleReason = {
  /* Variable: `reason` */
  [@deriving sexp]
  type t =
    | TypeInconsistent(option(SubReason.t))
    | WrongLength;

  let eq = (x, y) => x == y;
};

/* Variable: `err` */
[@deriving sexp]
type t =
  | NotInHole
  | InHole(HoleReason.t, MetaVar.t);

let make_recycled_InHole: (t, HoleReason.t, MetaVarGen.t) => (t, MetaVarGen.t) =
  (err, reason, u_gen) =>
    switch (err) {
    | NotInHole =>
      let (u, u_gen) = MetaVarGen.next_hole(u_gen);
      (InHole(reason, u), u_gen);
    | InHole(_, u) => (InHole(reason, u), u_gen)
    };
