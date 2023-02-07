[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Singleton(TypBase.t)
  | Abstract;

module Observation = {
  type t =
    | Base(TypBase.t)
    | Depth(int)
    | Free;
  let eq = (x, y) => {
    switch (x, y) {
    | (Base(x), Base(y)) => TypBase.eq(x, y)
    | (Depth(x), Depth(y)) => x == y
    | _ => false
    };
  };
};
