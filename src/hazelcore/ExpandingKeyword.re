[@deriving sexp]
type t =
  | Fun
  | Let
  | Case
  | TyAlias
  | TypFun
  | Forall;

let of_string: string => option(t) =
  fun
  | "fun" => Some(Fun)
  | "let" => Some(Let)
  | "case" => Some(Case)
  | "type" => Some(TyAlias)
  | "typfun" => Some(TypFun)
  | "forall" => Some(Forall)
  | _ => None;

let is_Fun = String.equal("fun");
let is_Let = String.equal("let");
let is_Case = String.equal("case");
let is_TyAlias = String.equal("type");
let is_TypFun = String.equal("typfun");
let is_Forall = String.equal("forall");

let mk = (text: string): option(t) =>
  if (text |> is_Let) {
    Some(Let);
  } else if (text |> is_Case) {
    Some(Case);
  } else if (text |> is_Fun) {
    Some(Fun);
  } else if (text |> is_TyAlias) {
    Some(TyAlias);
  } else if (text |> is_TypFun) {
    Some(TypFun);
  } else if (text |> is_Forall) {
    Some(Forall);
  } else {
    None;
  };

let to_string =
  fun
  | Fun => "fun"
  | Let => "let"
  | Case => "case"
  | TyAlias => "type"
  | TypFun => "typfun"
  | Forall => "forall";
