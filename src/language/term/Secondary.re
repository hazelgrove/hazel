open Util;

[@deriving (show({with_path: false}), sexp, yojson, enumerate)]
type cls =
  | Whitespace
  | Comment;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type secondary_content =
  | Whitespace(string)
  | Comment(string);

/* Basic secondary type for use in term annotations.
   The full Secondary module in haz3lcore extends this with utility functions. */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = {
  id: Id.t,
  content: secondary_content,
};

let cls_of = (s: t): cls =>
  switch (s.content) {
  | Whitespace(_) => Whitespace
  | Comment(_) => Comment
  };
