type t = string;

let rec extract = (~t: HTyp.t): t =>
  switch (t) {
  | Hole => failwith("Typ: Hole type") //FIXME: remove Hole check in expression
  | Int => "int"
  | Float => "float"
  | Bool => "bool"
  // add parenthesis is necessary
  // (int -> int) -> int != int -> int -> int
  | Arrow(t1, t2) =>
    "(" ++ extract(~t=t1) ++ " -> " ++ extract(~t=t2) ++ ")"
  // use Polymorphic Variants to encode sum, i.e. 'Left of t1, 'Right of t2, and easier for injection
  // the type is "[< `Left of t1 | `Right of t2]", < means no more than
  // so the expression should be `Left e1, `Right e2 to match this design
  | Sum(t1, t2) =>
    "[< "
    ++ "`Left of "
    ++ extract(~t=t1)
    ++ " | "
    ++ "`Right of "
    ++ extract(~t=t2)
    ++ " ]"
  | Prod(tl) => "(" ++ list_prod(~tl) ++ ")"
  | List(t) => extract(~t) ++ " list"
  }
and list_prod = (~tl: list(HTyp.t)): t =>
  switch (tl) {
  | [] => "unit"
  | [h] => extract(~t=h)
  | [h, ...t] => extract(~t=h) ++ " * " ++ list_prod(~tl=t)
  };
