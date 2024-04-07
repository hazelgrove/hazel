open Sexplib.Std;

module Move = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Step(Dir2.t)
    | Skip(Dir2.t)
    | Jump(Layout.Pos.t)
    | Hole(Dir.t);
};

module Select = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Un // unselect
    | All
    | Wald
    | Meld
    | Move(Move.t);
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Move(Move.t)
  | Select(Select.t)
  | Delete(Dir.t)
  | Insert(string);
