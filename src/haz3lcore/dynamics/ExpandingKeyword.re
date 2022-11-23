[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Let
  | Case
  | Fun
  | TypFun
  | Test;

let is_Let = String.equal("let");
let is_Case = String.equal("case");
let is_Fun = String.equal("fun");
let is_TypFun = String.equal("typfun");
let is_Test = String.equal("test");

let mk = (text: string): option(t) =>
  if (text |> is_Let) {
    Some(Let);
  } else if (text |> is_Case) {
    Some(Case);
  } else if (text |> is_Fun) {
    Some(Fun);
  } else if (text |> is_TypFun) {
    Some(TypFun);
  } else if (text |> is_Test) {
    Some(Test);
  } else {
    None;
  };

let to_string =
  fun
  | Let => "let"
  | Case => "case"
  | Fun => "fun"
  | TypFun => "typfun"
  | Test => "test";
