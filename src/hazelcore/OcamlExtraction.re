module Typ = {
  type ocaml_typ = string;

  let rec extract = (~t: HTyp.t): ocaml_typ =>
    switch (t) {
    | Hole => failwith("Type: Hole") // TODO: check if I understand correct
    | Int => "int"
    | Float => "float"
    | Bool => "bool"
    | Arrow(t1, t2) => extract(~t=t1) ++ "->" ++ extract(~t=t2)
    | Sum(t1, t2) => extract(~t=t1) ++ " | " ++ extract(~t=t2)
    | Prod(tl) => list_prod(~tl)
    | List(t) => extract(~t) ++ " list"
    }
  and list_prod = (~tl: list(HTyp.t)): ocaml_typ =>
    switch (tl) {
    | [] => ""
    | [h, ...t] => extract(~t=h) ++ "*" ++ list_prod(~tl=t)
    };
};

module Pat = {
  type ocaml_pat = string;
  // TODO: should write like "expand"
  // assert failwith if these things found, but I guess Keyword should have something to be deal with, TODO: look for codes related to Keyword
  let rec extract = (~dp: DHPat.t): ocaml_pat => {
    switch (dp) {
    | EmptyHole(_, _) => failwith("Pat: Empty Hole")
    | NonEmptyHole(_, _, _, _) => failwith("Pat: NonEmptyHole")
    | Wild => "_"
    | Keyword(_, _, _) => failwith("Pat: Keyword")  //FIXME: ?
    | Var(s) => s
    | IntLit(i) => string_of_int(i)
    | FloatLit(f) => string_of_float(f)
    | BoolLit(b) => string_of_bool(b)
    | Inj(_side, t) => extract(~dp=t)
    | ListNil => "[]"
    | Cons(a, b) => extract(~dp=a) ++ "::" ++ extract(~dp=b)
    | Pair(a, b) => "(" ++ extract(~dp=a) ++ ", " ++ extract(~dp=b) ++ ")"
    | Triv => "()"
    | Ap(_, _) => failwith("Pat: Ap")
    };
  };
};

module Exp = {
  type ocaml_exp = string;

  // (extracted expression, dynamic/inner type)
  type extract_result = option((ocaml_exp, HTyp.t));

  let extract = (~ctx: Contexts.t, ~de: DHExp.t): extract_result => {
    
  };




  /* Theorem: If extract(ctx, d) = Some(ce, ty) and ctx |- d : ty then ce :_ocaml Typ.extract(ty) */
};
