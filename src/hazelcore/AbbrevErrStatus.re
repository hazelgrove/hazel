module HoleReason = {
  [@deriving sexp]
  type t =
    | Free
    | ExtraneousArgs
    | NotLivelitExp;
};

[@deriving sexp]
type t =
  | NotInAbbrevHole
  | InAbbrevHole(HoleReason.t, MetaVar.t);

let in_hole_with_reason = (reason, err) =>
  switch (err) {
  | InAbbrevHole(reason', _) when reason == reason' => true
  | _ => false
  };
