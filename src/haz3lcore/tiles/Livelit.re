open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  name: string,
  width: int,
  default: DHExp.t,
  expansion_type: Typ.t,
};

type state = Id.Map.t(DHExp.t);
let empty_state: state = Id.Map.empty;
let slider: t = {
  name: "^slider\t",
  width: 10,
  default: IntLit(50),
  expansion_type: Int,
};

let checkbox: t = {
  name: "^checkbox\t",
  width: 1,
  default: BoolLit(false),
  expansion_type: Bool,
};

let fslider: t = {
  name: "^fslider\t",
  width: 10,
  default: FloatLit(0.5),
  expansion_type: Float,
};

let livelits: list(t) = [checkbox, fslider, slider];

let find_livelit = (livelit_name: string): option(t) =>
  List.find_opt(l => l.name == livelit_name, livelits);

let elaborate_livelit =
    (livelit_name: string, uexp_id: int, livelits: state): option(DHExp.t) => {
  switch (Id.Map.find_opt(uexp_id, livelits)) {
  | Some(t) => Some(t)
  | None =>
    switch (find_livelit(livelit_name)) {
    | Some(l) => Some(l.default)
    | None => None
    }
  };
};
