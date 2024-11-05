open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  name: string,
  model_t: Typ.t,
  // init: UExp.term,
  expansion_t: Typ.t,
  expansion_f: UExp.t => UExp.t,
};
let slider: t = {
  name: "slider",
  expansion_t: Typ.temp(Typ.Int),
  expansion_f: (_model: UExp.t) => DHExp.fresh(Int(3)),
  model_t: Typ.temp(Typ.Int),
};

let timestamp: t = {
  name: "timestamp",
  expansion_t: Typ.temp(Typ.Int),
  expansion_f: (_model: UExp.t) =>
    DHExp.fresh(Int(Float.to_int(JsUtil.timestamp()))),
  model_t: Typ.temp(Typ.Int),
};

let func_test: t = {
  name: "func_test",
  expansion_t: Typ.temp(Typ.Arrow(Typ.temp(Typ.Int), Typ.temp(Typ.Int))),
  expansion_f: (_model: UExp.t) => DHExp.fresh(Int(3)),
  model_t: Typ.temp(Typ.Int),
};

// let checkbox: t = {
//   name: "checkbox",
//   width: 1,
//   default: Bool(false),
//   expansion_type: Typ.temp(Typ.Bool),
// };

// let fslider: t = {
//   name: "fslider",
//   width: 10,
//   default: Float(0.5),
//   expansion_type: Typ.temp(Typ.Float),
// };

let livelits: list(t) = [slider, timestamp, func_test];

let find_livelit = (livelit_name: string): option(t) =>
  List.find_opt(l => l.name == livelit_name, livelits);

let elaborate_livelit =
    (livelit_name: string): option((Typ.t, UExp.t => UExp.t)) => {
  let r =
    switch (find_livelit(livelit_name)) {
    | Some(l) => Some((l.expansion_t, l.expansion_f))
    | None => None
    };
  r;
};

let get_livelit_type = (livelit_name: string): Typ.t =>
  switch (find_livelit(livelit_name)) {
  | Some(l) => l.expansion_t
  | None => Typ.temp(Typ.Unknown(Internal))
  };

let project_livelit = (livelit_name: string): string => {
  switch (find_livelit(livelit_name)) {
  | Some(l) => l.name
  | None => "Not found"
  };
};

// [@deriving (show({with_path: false}), sexp, yojson)]
// type t =
//   name: string,
//   model_t: Typ.t,
//   init: UExp.t,
//   default: UExp.term,
//   expansion_t: Typ.t,
//   // view
//   // action_t
//   // update
