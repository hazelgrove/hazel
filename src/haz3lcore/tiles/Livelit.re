open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  name: string,
  width: int,
  default: DHExp.t,
  expansion_type: Typ.t,
};

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
