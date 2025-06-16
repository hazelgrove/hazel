[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | Left
  | Right;

let toggle =
  fun
  | Left => Right
  | Right => Left;

let sign =
  fun
  | Left => (-1)
  | Right => 1;

let choose = (d, (l, r)) =>
  switch (d) {
  | Left => l
  | Right => r
  };
