open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type kind =
  | ExoSlider
  | ExoBuilder
  | ExoNool;

[@deriving (show({with_path: false}), sexp, yojson)]
type size = {
  width: int,
  height: int,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type shape =
  | Block
  | Tab;

[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | Resize(int, int);

type info = {
  kind,
  shape,
  dev: string,
  prod: string,
  guard: Language.Any.t => bool,
  size,
};

let name = (ek: kind): string => ek |> show_kind;

let of_name = (name: string): kind =>
  name |> Sexplib.Sexp.of_string |> kind_of_sexp;

let module_of_kind = (kind: kind): info =>
  switch (kind) {
  | ExoSlider => {
      kind,
      prod: WebEnv.base_url() ++ "/external/exoslider",
      dev: "http://localhost:5173",
      shape: Tab,
      guard: (
        fun
        | Exp({term: Atom(Int(_)), _}) => true
        | _ => false
      ),
      size: {
        width: 400,
        height: 160,
      },
    }
  | ExoBuilder => {
      kind,
      prod: WebEnv.base_url() ++ "/external/exovaluebuilder",
      dev: "http://localhost:5175",
      shape: Tab,
      // TODO: More specific syntax restriction
      guard: _ => true,
      size: {
        width: 795,
        height: 200,
      },
    }
  | ExoNool => {
      kind,
      prod: "https://andrewblinn.com/nool/exolivelit",
      dev: "http://localhost:3000",
      shape: Block,
      // TODO: More specific syntax restriction
      guard: _ => true,
      size: {
        width: 680,
        height: 490,
      },
    }
  };
