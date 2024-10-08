open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  name: string,
  width: int,
  default: Pat.term,
  expansion_type: Typ.cls,
};

type state = Id.Map.t(DHExp.t);
let empty_state: state = Id.Map.empty;
let slider: t = {
  name: "llslider\t",
  width: 10,
  default: Int(30),
  expansion_type: Int,
};

let checkbox: t = {
  name: "llcheckbox\t",
  width: 1,
  default: Bool(false),
  expansion_type: Bool,
};

let fslider: t = {
  name: "llfslider\t",
  width: 10,
  default: Float(0.5),
  expansion_type: Float,
};

let livelits: list(t) = [checkbox, fslider, slider];

let find_livelit = (livelit_name: string): option(t) =>
  List.find_opt(l => l.name == livelit_name, livelits);

let elaborate_livelit =
    (livelit_name: string, uexp_id: Uuidm.t, livelits: state)
    : option(Pat.term) => {
  switch (Id.Map.find_opt(uexp_id, livelits)) {
  //   | Some(t) => Some(t)
  | _ =>
    switch (find_livelit(livelit_name)) {
    | Some(l) => Some(l.default)
    | None => None
    }
  };
};
