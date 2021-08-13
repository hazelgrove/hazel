open Sexplib.Std;

[@deriving sexp]
type category =
  | InsertLit
  | InsertVar
  | InsertApp
  | InsertConstructor
  | InsertElim
  | Wrap
  | ReplaceOperator
  | Delete;

[@deriving sexp]
type score = {
  idiomaticity: int,
  type_specificity: int,
  delta_errors: int,
};

let blank_score: score = {
  idiomaticity: 0,
  type_specificity: 0,
  delta_errors: 0,
};

[@deriving sexp]
type t('a) = {
  category,
  score,
  action: Action.t,
  result: 'a,
  res_ty: HTyp.t,
  result_text: string,
};

let string_of_category: category => string =
  fun
  | InsertVar => "var"
  | InsertApp => "app"
  | InsertLit => "lit"
  | InsertConstructor => "con"
  | InsertElim => "eli"
  | ReplaceOperator => "opr"
  | Wrap => "wra"
  | Delete => "del";

let description_of_category: category => string =
  fun
  | InsertVar => "Inserts a variable"
  | InsertApp => "Inserts an application"
  | InsertLit => "Inserts a literal"
  | InsertConstructor => "Inserts a contructor"
  | InsertElim => "Inserts an eliminator"
  | ReplaceOperator => "Replaces an operator"
  | Wrap => "Wraps an existing form"
  | Delete => "Deletes the current form";
