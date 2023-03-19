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
  name: "^slider",
  width: 10,
  default: IntLit(50),
  expansion_type: Int,
};

let checkbox: t = {
  name: "^checkbox",
  width: 1,
  default: BoolLit(false),
  expansion_type: Bool,
};

let find_livelit = (livelit_name: string): option(t) =>
  switch (livelit_name) {
  | "^slider" => Some(slider)
  | "^checkbox" => Some(checkbox)
  | _ => None
  };

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
