[@deriving (show({with_path: false}), sexp, yojson, enumerate)]
type cls =
  | Whitespace
  | Comment;
