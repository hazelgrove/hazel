include String;

module Shape = {
  type t =
    | Const(string)
    | Int_lit
    | Float_lit
    | Alphanum_lower
    | Alphanum_upper;

  // order doesn't matter
  let to_int =
    fun
    | Const(_) => 0
    | Int_lit => 1
    | Float_lit => 2
    | Alphanum_lower => 3
    | Alphanum_upper => 4;

  let compare = (s1, s2) =>
    switch (s1, s2) {
    | (Const(s1), Const(s2)) => String.compare(s1, s2)
    | _ => Int.compare(to_int(s1), to_int(s2))
    };
};

// NOTE: keys are shapes, not the token strings
module Map = Map.Make(Shape);
