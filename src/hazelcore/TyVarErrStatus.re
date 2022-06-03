open Sexplib.Std;

module HoleReason = {
  [@deriving sexp]
  type t =
    | Unbound
    | Reserved
    | InvalidName;
};

[@deriving sexp]
type t =
  | NotInTyVarHole(Index.Abs.t, int)
  | InHole(HoleReason.t, MetaVar.t);
