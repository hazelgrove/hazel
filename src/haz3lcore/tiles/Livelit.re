open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  name: string,
  width: int,
  default: Pat.term,
  expansion_type: Typ.t,
};

type state = Id.Map.t(t);
let slider: t = {
  name: "slider\t",
  width: 10,
  default: Int(30),
  expansion_type: Typ.temp(Typ.Int),
};

let checkbox: t = {
  name: "checkbox\t",
  width: 1,
  default: Bool(false),
  expansion_type: Typ.temp(Typ.Bool),
};

let fslider: t = {
  name: "fslider\t",
  width: 10,
  default: Float(0.5),
  expansion_type: Typ.temp(Typ.Float),
};

let livelits: list(t) = [checkbox, fslider, slider];

let find_livelit = (livelit_name: string): option(t) =>
  List.find_opt(l => l.name == livelit_name, livelits);

let elaborate_livelit = (livelit_name: string): option(Typ.t) => {
  switch (find_livelit(livelit_name)) {
  | Some(l) => Some(l.expansion_type)
  | None => None
  };
};
