type t = string;

let rec extract = (~t: HTyp.t): t =>
  switch (t) {
  | Hole => failwith("Typ: Hole type")
  | Int => "int"
  | Float => "float"
  | Bool => "bool"
  // add parenthesis is necessary
  // (int -> int) -> int != int -> int -> int
  | Arrow(t1, t2) =>
    "(" ++ extract(~t=t1) ++ " -> " ++ extract(~t=t2) ++ ")"
  | Sum(t1, t2) => "(" ++ extract(~t=t1) ++ " | " ++ extract(~t=t2) ++ ")"
  | Prod(tl) => "(" ++ list_prod(~tl) ++ ")"
  | List(t) => extract(~t) ++ " list"
  }
and list_prod = (~tl: list(HTyp.t)): t =>
  switch (tl) {
  | [] => ""
  | [h, ...t] => extract(~t=h) ++ " * " ++ list_prod(~tl=t)
  };
