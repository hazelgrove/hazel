open Sexplib.Std;
open Haz3lcore;

// TODO Make unified way of using consistent metavariables for syntactic forms
// TODO Use /tau instead of ty when can do that and still have highlighting work
[@deriving (show({with_path: false}), sexp, yojson)]
type feedback_option =
  | ThumbsUp
  | ThumbsDown
  | Unselected;

[@deriving (show({with_path: false}), sexp, yojson)]
type example = {
  sub_id: string,
  term: Segment.t,
  message: string,
  feedback: feedback_option,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type explanation = {
  message: string,
  feedback: feedback_option,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type form = {
  id: string,
  syntactic_form: Segment.t,
  expandable_id: option(Id.t),
  explanation,
  examples: list(example),
};
