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
type example_id =
  | IntList
  | TupleList
  | Cons1
  | Cons2
  | BasicFun
  | WildFun
  | IntLitFun
  | FloatLitFun
  | BoolLitFun
  | StrLitFun
  | TrivFun
  | ListNilFun
  | ListListFun
  | ConsHdFun
  | ConsSndFun
  | VarIncrFun
  | VarAndFun
  | Tuple2Fun
  | Tuple3Fun
  | TagFun
  | ApFun;

[@deriving (show({with_path: false}), sexp, yojson)]
type example = {
  sub_id: example_id,
  term: Segment.t,
  message: string,
  feedback: feedback_option,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type form_id =
  | EmptyHoleExp
  | MultiHoleExp
  | TrivExp
  | BoolExp
  | IntExp
  | FloatExp
  | StringExp
  | VarExp
  | TagExp
  //| ListExp
  //| ConsExp
  | FunctionExp
  | FunctionEmptyHole
  | FunctionMultiHole
  | FunctionWild
  | FunctionInt
  | FunctionFloat
  | FunctionBool;

[@deriving (show({with_path: false}), sexp, yojson)]
type explanation = {
  message: string,
  feedback: feedback_option,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type form = {
  id: form_id,
  syntactic_form: Segment.t,
  expandable_id: option(Id.t),
  explanation,
  examples: list(example),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type form_option = {
  form,
  expansion_label: Segment.t,
  selected: bool,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type group_id =
  | EmptyHoleExp
  | MultiHoleExp
  | TrivExp
  | BoolExp
  | IntExp
  | FloatExp
  | StringExp
  | VarExp
  | TagExp
  //| ListExp
  //| ConsExp
  | FunctionExp
  | FunctionEmptyHole
  | FunctionMultiHole
  | FunctionWild
  | FunctionInt
  | FunctionFloat
  | FunctionBool;
