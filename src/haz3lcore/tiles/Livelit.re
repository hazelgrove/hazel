open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  name: string,
  width: int,
  default: DHExp.t,
};

let slider: t = {name: "^slider", width: 10, default: IntLit(50)};

let checkbox: t = {name: "^checkbox", width: 1, default: BoolLit(false)};

let find_livelit = (livelit_name: string): option(t) =>
  switch (livelit_name) {
  | "^slider" => Some(slider)
  | "^checkbox" => Some(checkbox)
  | _ => None
  };
