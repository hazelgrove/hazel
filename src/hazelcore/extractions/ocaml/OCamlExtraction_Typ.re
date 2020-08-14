// mark with a constructor to tell apart with pat, exp
type t =
  | OCamlTyp(string);

// exception whenever we have a Hole in type
exception Typ_Hole;
// reserve for pat and exp use, (expression category, required type)
exception Typ_NotMatch(string, string);

let rec extract = (t: HTyp.t): t =>
  switch (t) {
  | Hole => raise(Typ_Hole)
  | Int => OCamlTyp("int")
  | Float => OCamlTyp("float")
  | Bool => OCamlTyp("bool")
  // add parenthesis is necessary
  | Arrow(t1, t2) =>
    switch (extract(t1), extract(t2)) {
    | (OCamlTyp(typ1), OCamlTyp(typ2)) =>
      OCamlTyp("(" ++ typ1 ++ " -> " ++ typ2 ++ ")")
    }
  // use Polymorphic Variants to encode sum, i.e. 'Left of t1, 'Right of t2, and easier for injection
  // the type is "[ `Left of t1 | `Right of t2]"
  // so the expression should be `Left e1, `Right e2 to match this design
  | Sum(t1, t2) =>
    switch (extract(t1), extract(t2)) {
    | (OCamlTyp(typ1), OCamlTyp(typ2)) =>
      OCamlTyp(
        "[ " ++ "`Left of " ++ typ1 ++ " | " ++ "`Right of " ++ typ2 ++ " ]",
      )
    }
  | Prod(tl) => list_prod(~tl)
  // left binding, i.e. a*b*c = (a*b)*c
  | List(t) =>
    switch (extract(t)) {
    | OCamlTyp(typ) => OCamlTyp(typ ++ " list")
    }
  }
and list_prod = (~tl: list(HTyp.t)): t =>
  switch (tl) {
  | [] => OCamlTyp("unit")
  | [h] => extract(h)
  | [h, ...t] =>
    switch (extract(h), list_prod(~tl=t)) {
    | (OCamlTyp(typ_hd), OCamlTyp(typ_tl)) =>
      OCamlTyp("(" ++ typ_hd ++ " * " ++ typ_tl ++ ")")
    }
  };
