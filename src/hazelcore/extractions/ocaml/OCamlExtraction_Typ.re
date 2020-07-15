type t =
  | OcamlTyp(string)
  | ExtractionFailed(string);

let rec extract = (t: HTyp.t): t =>
  switch (t) {
  | Hole => ExtractionFailed("Typ: Hole type") //FIXME: remove Hole check in expression
  | Int => OcamlTyp("int")
  | Float => OcamlTyp("float")
  | Bool => OcamlTyp("bool")
  // add parenthesis is necessary
  // (int -> int) -> int != int -> int -> int
  | Arrow(t1, t2) =>
    switch (extract(t1), extract(t2)) {
    | (OcamlTyp(x), OcamlTyp(y)) => OcamlTyp("(" ++ x ++ " -> " ++ y ++ ")")
    | _ => ExtractionFailed("Typ: Hole type")
    }
  // use Polymorphic Variants to encode sum, i.e. 'Left of t1, 'Right of t2, and easier for injection
  // the type is "[ `Left of t1 | `Right of t2]"
  // so the expression should be `Left e1, `Right e2 to match this design
  | Sum(t1, t2) =>
    switch (extract(t1), extract(t2)) {
    | (OcamlTyp(s1), OcamlTyp(s2)) =>
      OcamlTyp(
        "[ " ++ "`Left of " ++ s1 ++ " | " ++ "`Right of " ++ s2 ++ " ]",
      )
    | _ => ExtractionFailed("Typ: Hole type")
    }
  | Prod(tl) => list_prod(~tl)
  // left binding, i.e. a*b*c = (a*b)*c
  | List(t) =>
    switch (extract(t)) {
    | OcamlTyp(s) => OcamlTyp(s ++ " list")
    | ExtractionFailed(s) => ExtractionFailed(s)
    }
  }
and list_prod = (~tl: list(HTyp.t)): t =>
  switch (tl) {
  | [] => OcamlTyp("unit")
  | [h] => extract(h)
  | [h, ...t] =>
    switch (extract(h), list_prod(~tl=t)) {
    | (OcamlTyp(sh), OcamlTyp(st)) =>
      OcamlTyp("(" ++ sh ++ " * " ++ st ++ ")")
    | _ => ExtractionFailed("Typ: Hole type")
    }
  };
