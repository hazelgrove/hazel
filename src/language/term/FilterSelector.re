[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type t =
  | Exp
  | Val;

let string_of_t = s => {
  switch (s) {
  | Exp => "exp"
  | Val => "val"
  };
};
