[@deriving sexp]
type t =
  | Assert
  | Expanding(ExpandingKeyword.t);

let is_Assert = String.equal("assert");

let mk = (text: string): option(t) =>
  if (text |> is_Assert) {
    Some(Assert);
  } else {
    switch (ExpandingKeyword.mk(text)) {
    | Some(Let) => Some(Expanding(Let))
    | Some(Case) => Some(Expanding(Case))
    | _ => None
    };
  };

let to_string =
  fun
  | Assert => "assert"
  | Expanding(t) => ExpandingKeyword.to_string(t);
