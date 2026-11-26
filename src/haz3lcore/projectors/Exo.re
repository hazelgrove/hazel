open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type kind = Language.ProjectorKind.exo;

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
      dev: "https://hazel.org/build/exolivelits/external/exoslider", //"http://localhost:5173",
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
      dev: "https://hazel.org/build/exolivelits//external/exovaluebuilder", //"http://localhost:5175",
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
      dev: "https://andrewblinn.com/nool/exolivelit", //"http://localhost:3000",
      shape: Block,
      // TODO: More specific syntax restriction
      guard: _ => true,
      size: {
        width: 680,
        height: 490,
      },
    }
  | Petrinaut => {
      kind,
      prod: "https://hazel.petrinaut.org",
      dev: "https://hazel.petrinaut.org", //"http://localhost:5174",
      shape: Block,
      // TODO: More specific syntax restriction
      guard: _ => true,
      size: {
        width: 1050,
        height: 590,
      },
    }
  | CatColLab => {
      kind,
      prod: "http://localhost:5175", //"https://catcolab.org/model/0194fbf4-fddf-7a12-b88b-33015d17d8e7",
      dev: "http://localhost:5175",
      shape: Block,
      guard: _ => true,
      size: {
        width: 680,
        height: 490,
      },
    }
  };
