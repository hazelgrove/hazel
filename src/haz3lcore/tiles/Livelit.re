open Util;
open Virtual_dom.Vdom;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  name: string,
  model_t: Typ.t,
  expansion_t: Typ.t,
  expansion_f: UExp.t => UExp.t,
  projector: UExp.t => Node.t,
};
let slider: t = {
  name: "slider",
  expansion_t: Typ.temp(Typ.Int),
  expansion_f: (model: UExp.t) =>
    switch (model.term) {
    | Int(n) => DHExp.fresh(Int(n))
    | _ => DHExp.fresh(Int(-1))
    },
  model_t: Typ.temp(Typ.Int),
  projector: (model: UExp.t) =>
    Node.div(
      ~attrs=[Attr.class_("livelit")],
      [Node.text("I am a slider and my model is " ++ UExp.show(model))],
    ),
};
let timestamp: t = {
  name: "timestamp",
  expansion_t: Typ.temp(Typ.Int),
  expansion_f: (_model: UExp.t) =>
    DHExp.fresh(Int(Float.to_int(JsUtil.timestamp()))),
  model_t: Typ.temp(Typ.Prod([])),
  projector: (_model: UExp.t) =>
    Node.div(
      ~attrs=[Attr.class_("livelit")],
      [Node.text("Guess what guys -- I am a timestamp livelit!")],
    ),
};

let not_found: t = {
  name: "not_found",
  expansion_t: Typ.temp(Typ.Unknown(Internal)),
  expansion_f: (_model: UExp.t) => DHExp.fresh(String("No livelit found")),
  model_t: Typ.temp(Typ.Unknown(Internal)),
  projector: (_model: UExp.t) =>
    Node.div(
      ~attrs=[Attr.class_("livelit")],
      [Node.text("No livelit found")],
    ),
};

let livelits: list(t) = [slider, timestamp];

let find_livelit = (livelit_name: string): t =>
  // List.find_opt(l => l.name == livelit_name, livelits)
  // |> Option.value(~default=not_found);
  switch (List.find_opt(l => l.name == livelit_name, livelits)) {
  | Some(l) => l
  | None =>
    print_endline("Livelit " ++ livelit_name ++ " not found");
    not_found;
  };
