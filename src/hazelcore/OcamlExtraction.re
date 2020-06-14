module Typ = {
  type ocaml_typ = string;

  let rec extract = (~t: HTyp.t): ocaml_typ =>
    switch (t) {
    | Hole => "'a" // FIXME: maybe it means an error?
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
  // should write like "expand"
  let rec extract = (~dp: DHPat.t): ocaml_pat => {
    switch (dp) {
    | EmptyHole(_, _) => failwith("Pat: Empty Hole")
    | NonEmptyHole(_, _, _, _) => failwith("Pat: NonEmptyHole")
    | Wild => "_"
    | Keyword(_, _, _) => failwith("Pat: Incomplete Program(Keyword)") 
    | Var(s) => s
    | IntLit(i) => string_of_int(i)
    | FloatLit(f) => string_of_float(f)
    | BoolLit(b) => string_of_bool(b)
    | Inj(_side, t) => extract(~dp=t)
    | ListNil => "[]"
    | Cons(a, b) => extract(~dp=a) ++ "::" ++ extract(~dp=b)
    | Pair(a, b) => "(" ++ extract(~dp=a) ++ ", " ++ extract(~dp=b) ++ ")"
    | Triv => "()"
    | Ap(_, _) => failwith("Pat: Apply Palette")
    };
  };
};

module Exp = {
  type ocaml_exp = string;

  // (extracted expression, dynamic/inner type) like ExpandResult
  // Htyp.t is like the ground type

  //type extract_result = option((ocaml_exp, HTyp.t));
  type extract_result = (ocaml_exp, HTyp.t)

  let rec extract = (~ctx: Contexts.t, ~de: DHExp.t): extract_result => 
    switch(de) {
      | EmptyHole(_) => failwith("Exp: Empty Hole")
      | NonEmptyHole(_) => failwith("Exp: Non-Empty Hole")
      // used for variables that are keywords in Hazel, like "let" and "case"
      | Keyword(_) => failwith("Exp: Incomplete Program (Keyword)") 
      // | FreeVar
      // | BoundVar
      // | Let
      // | FixF
      // | Lam
      | Ap(_) => failwith("Exp: Apply Palette")
      | BoolLit(bool) => (string_of_bool(bool), Bool)
      | IntLit(int) => (string_of_int(int), Int)
      | FloatLit(float) => (string_of_float(float), Float)
      | BinBoolOp(op, d1, d2) => switch(d1, d2){
        | (BoolLit(b1), BoolLit(b2)) => {
          let str_of_op : DHExp.BinBoolOp.t => string = fun
            | And => "&&"
            | Or => "||";
          let str = string_of_bool(b1) ++ str_of_op(op) ++ string_of_bool(b2);
          (str, Bool)
        }
        | _ => failwith("Exp: BinBoolOp")
      }
      | BinIntOp(op, d1, d2) => {
          let str_of_op : DHExp.BinIntOp.t => (string, HTyp.t) = fun
            | Minus => ("-", Int)
            | Plus => ("+", Int)
            | Times => ("*", Int)
            | Divide => ("/", Int)
            | LessThan => ("<", Bool)
            | GreaterThan => (">", Bool)
            | Equals => ("==", Bool);
          let ocaml_op = str_of_op(op);
          let ocaml_d1 = extract(~ctx=ctx, ~de=d1);
          let ocaml_d2 = extract(~ctx=ctx, ~de=d2);
          switch(snd(ocaml_d1), snd(ocaml_d2)){
            | (Int, Int) => (fst(ocaml_d1) ++ fst(ocaml_op) ++ fst(ocaml_d2), snd(ocaml_op))
            | _ => failwith("Exp: BinIntOp")
          }
      }
      | BinFloatOp(op, d1, d2) => {
          let str_of_op : DHExp.BinFloatOp.t => (string, HTyp.t) = fun
            | FPlus => ("+.", Float)
            | FMinus => ("-.", Float)
            | FTimes => ("*.", Float)
            | FDivide => ("/.", Float)
            | FLessThan => ("<", Bool)
            | FGreaterThan => (">", Bool)
            | FEquals => ("==", Bool);
          let ocaml_op = str_of_op(op);
          let ocaml_d1 = extract(~ctx=ctx, ~de=d1);
          let ocaml_d2 = extract(~ctx=ctx, ~de=d2);
          switch(snd(ocaml_d1), snd(ocaml_d2)){
            | (Float, Float) => (fst(ocaml_d1) ++ fst(ocaml_op) ++ fst(ocaml_d2), snd(ocaml_op))
            | _ => failwith("Exp: BinFloatOp")
          }
      }
      | ListNil(t) => ("[]", t)
      | Cons(d1, d2) => switch(snd(extract(~ctx=ctx, ~de=d2))){
        // can't determine type
        | Hole => 
      }



  /* Theorem: If extract(ctx, d) = Some(ce, ty) and ctx |- d : ty then ce :_ocaml Typ.extract(ty) */
};
