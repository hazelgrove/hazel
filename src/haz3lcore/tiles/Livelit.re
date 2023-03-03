open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  name: string,
  width: int,
  default: DHExp.t,
  expansion_type: Typ.t,
  elaborate: (~state: DHExp.t, ~params: DHExp.t) => option(DHExp.t),
};

let slider: t = {
  name: "^slider",
  width: 10,
  default: IntLit(50),
  expansion_type: Int,
  elaborate: (~state, ~params) => {
    switch (params) {
    | Tuple([min, max]) =>
      // We want (max - min) * state / 100 + min to get the scaled amount
      Some(
        BinIntOp(
          Plus,
          BinIntOp(
            Divide,
            BinIntOp(Times, BinIntOp(Minus, max, min), state),
            IntLit(100),
          ),
          min,
        ),
      )
    | _ => None
    };
  },
};

let checkbox: t = {
  name: "^checkbox",
  width: 1,
  default: BoolLit(false),
  expansion_type: Bool,
  elaborate: (~state, ~params as _) => Some(state),
};

let find_livelit = (livelit_name: string): option(t) =>
  switch (livelit_name) {
  | "^slider" => Some(slider)
  | "^checkbox" => Some(checkbox)
  | _ => None
  };
