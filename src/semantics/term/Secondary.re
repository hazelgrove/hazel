[@deriving (show({with_path: false}), sexp, yojson, enumerate)]
type cls =
  | Whitespace
  | Comment;
/*TODO(andrew): clarify whether this needs to exist*/
