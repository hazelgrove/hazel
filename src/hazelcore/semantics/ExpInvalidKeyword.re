[@deriving sexp]
type t =
  | Expanding(ExpandingKeyword.t);

let mk = (text: string): option(t) =>
  switch (ExpandingKeyword.mk(text)) {
  | Some(Let) => Some(Expanding(Let))
  | Some(Case) => Some(Expanding(Case))
  | _ => None
  };

let to_string =
  fun
  | Expanding(t) => ExpandingKeyword.to_string(t);
