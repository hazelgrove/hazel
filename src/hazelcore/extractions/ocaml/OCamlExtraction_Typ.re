// mark with a constructor to tell apart with pat, exp
type t =
  | OCamlTyp(string);

// exception whenever we have a Hole in type
exception Typ_Hole;
// exception reserved for pat and exp extraction, (expression category, required type)
exception Typ_NotMatch(string, string);

let rec extract = (t: HTyp.t): t =>
  switch (t) {
  | Hole => raise(Typ_Hole)
  | Int => OCamlTyp("int")
  | Float => OCamlTyp("float")
  | Bool => OCamlTyp("bool")
  // add parenthesis is necessary
  | Arrow(t1, t2) =>
    let OCamlTyp(typ1) = extract(t1);
    let OCamlTyp(typ2) = extract(t2);
    OCamlTyp("(" ++ typ1 ++ " -> " ++ typ2 ++ ")");
  // use Polymorphic Variants to encode sum, i.e. 'Left of t1, 'Right of t2, and easier for injection
  // the type is "[ `Left of t1 | `Right of t2]"
  // so the expression should be `Left e1, `Right e2 to match this design
  | Sum(t1, t2) =>
    let OCamlTyp(typ1) = extract(t1);
    let OCamlTyp(typ2) = extract(t2);
    OCamlTyp(
      "[ " ++ "`Left of " ++ typ1 ++ " | " ++ "`Right of " ++ typ2 ++ " ]",
    );
  // explicit parenthesize, i.e. a*b*c = (a*b)*c
  | Prod(tl) => list_prod(~tl)
  | List(t) =>
    let OCamlTyp(typ) = extract(t);
    OCamlTyp(typ ++ " list");
  }
and list_prod = (~tl: list(HTyp.t)): t =>
  switch (tl) {
  | [] => OCamlTyp("unit")
  | [h] => extract(h)
  | [h, ...t] =>
    let OCamlTyp(typ_hd) = extract(h);
    let OCamlTyp(typ_tl) = list_prod(~tl=t);
    OCamlTyp("(" ++ typ_hd ++ " * " ++ typ_tl ++ ")");
  };
