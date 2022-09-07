[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | L
  | R;

let to_string =
  fun
  | L => "L"
  | R => "R";

let pick = (side, l, r) =>
  switch (side) {
  | L => l
  | R => r
  };
