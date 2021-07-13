open Sexplib.Std;
//open OptUtil.Syntax;

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
type t('a) = {
  category,
  action: Action.t,
  result: 'a,
  res_ty: HTyp.t,
  result_text: string,
  delta_errors: int,
  score: int,
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
