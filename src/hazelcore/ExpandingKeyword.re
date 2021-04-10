[@deriving sexp]
type t =
  | Let
  | Case;

let is_Let = String.equal("let");
let is_Case = String.equal("case");
let is_ReservedOperator = x =>
  if (Var.is_complete_operator(x)) {
    switch (x |> Var.remove_underscores |> Operators_Exp.string_to_operator) {
    | Some(UserOp(_))
    | None => false
    | _ => true
    };
  } else {
    false;
  };

let mk = (text: string): option(t) =>
  if (text |> is_Let) {
    Some(Let);
  } else if (text |> is_Case) {
    Some(Case);
  } else {
    None;
  };

let to_string =
  fun
  | Let => "let"
  | Case => "case";
