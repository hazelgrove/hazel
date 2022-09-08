module HoleReason = {
  /* Variable: `reason` */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | TypeInconsistent
    | WrongLength;

  let eq = (x, y) => x == y;
};

/* Variable: `err` */
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | NotInHole
  | InHole(HoleReason.t, MetaVar.t);
