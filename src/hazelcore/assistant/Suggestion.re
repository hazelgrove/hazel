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
  | InsertVar => "Insert a variable"
  | InsertApp => "Insert an application"
  | InsertLit => "Insert a literal"
  | InsertConstructor => "Insert a contructor"
  | InsertElim => "Insert an eliminator"
  | ReplaceOperator => "Replace an operator"
  | Wrap => "Wrap the current form"
  | Delete => "Delete the current form";
