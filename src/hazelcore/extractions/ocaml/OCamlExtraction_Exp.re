/* Theorem: If extract(ctx, d) = Some(ce, ty) and ctx |- d : ty then ce :_ocaml Typ.extract(ty) */

/*
 Naming Conventions:
   d for DHExp, dp for DHPat, ht for HTyp
   ocaml_(pat|typ|exp) for return result of extract
   (typ|pat|exp)(_(str|typ))? for destructed components
 */

type ocaml_exp = string;
type t =
  | OCamlExp(ocaml_exp, HTyp.t);

exception Exp_Hole;
exception Exp_Keyword(ExpandingKeyword.t);
exception Exp_Invalid(option(string));
exception Exp_Fixpoint(string);

//at top level, we use de instead of d because it's used for destruction only
let rec extract = (ctx: Contexts.t, de: DHExp.t): t =>
  switch (de) {
  | EmptyHole(_)
  | NonEmptyHole(_) => raise(Exp_Hole)
  // used for variables that are keywords in Hazel, like "let" and "case"
  | Keyword(_, _, _, keyword) => raise(Exp_Keyword(keyword))
  // an expression with a free variable cannot be extracted to OCaml because free variables cause type errors in OCaml
  | FreeVar(_, _, _, _) => raise(Exp_Invalid(Some("free variable")))
  | BoundVar(s) =>
    //directly lookup type in environment
    let typ = VarCtx.lookup(Contexts.gamma(ctx), s);
    switch (typ) {
    | None => raise(Exp_Invalid(Some("undeclared variable")))
    | Some(t) => OCamlExp(s, t)
    };
  // FixF is handled in Let,
  // Recursive functions can only declear by Let, this design is to keep the invariant
  | FixF(s, _ht, _d) => raise(Exp_Fixpoint(s))
  | Let(dp, d1, d2) =>
    let ocaml_pat = OCamlExtraction_Pat.extract(dp);
    // if d1 is a resursive function, should first update itself to context
    let ocaml_exp1 =
      switch (d1) {
      | FixF(s, ht, d) =>
        //fixpoint function name should be added into the context to evaluate itself
        let ctx_fix = Contexts.extend_gamma(ctx, (s, ht));
        switch (extract(ctx_fix, d)) {
        | OCamlExp(exp_str, _exp_typ) => OCamlExp(exp_str, ht)
        };
      | _ => extract(ctx, d1)
      };
    switch (ocaml_pat, ocaml_exp1) {
    | (OCamlPat(pat_str), OCamlExp(exp1_str, exp1_typ)) =>
      //let can support various pattern declaration besides variable
      let ctx' = OCamlExtraction_Pat.update_pattern(dp, exp1_typ, ctx);
      let ocaml_exp2 = extract(ctx', d2);
      let ocaml_typ = OCamlExtraction_Typ.extract(exp1_typ);
      switch (ocaml_typ, ocaml_exp2) {
      | (OCamlTyp(typ_str), OCamlExp(exp2_str, exp2_typ)) =>
        //using "let x:t = d1 in d2" ocaml format, if d1 is a recursive function, use "let rec" instead
        let body =
          pat_str
          ++ " : "
          ++ typ_str
          ++ " = "
          ++ exp1_str
          ++ " in\n"
          ++ exp2_str;
        switch (d1) {
        | FixF(_, _, _) => OCamlExp("let rec " ++ body, exp2_typ)
        | _ => OCamlExp("let " ++ body, exp2_typ)
        };
      };
    };
  | Lam(dp, ht, d) =>
    //Htyp.t is the expected type of pattern
    let ocaml_pat = OCamlExtraction_Pat.extract(dp);
    let ocaml_ht = OCamlExtraction_Typ.extract(ht);
    // lambda can also declear various patterns besides variable
    let ctx' = OCamlExtraction_Pat.update_pattern(dp, ht, ctx);
    let ocaml_exp = extract(ctx', d);
    switch (ocaml_pat, ocaml_ht, ocaml_exp) {
    | (OCamlPat(pat_str), OCamlTyp(typ_str), OCamlExp(exp_str, exp_typ)) =>
      OCamlExp(
        "(fun (" ++ pat_str ++ " : " ++ typ_str ++ ") -> (" ++ exp_str ++ "))",
        Arrow(ht, exp_typ),
      )
    };
  //apply
  | Ap(d1, d2) =>
    let ocaml_exp1 = extract(ctx, d1);
    let ocaml_exp2 = extract(ctx, d2);
    switch (ocaml_exp1, ocaml_exp2) {
    | (OCamlExp(exp1_str, exp1_typ), OCamlExp(exp2_str, _exp2_typ)) =>
      //removed the type check t1 == exp2_typ
      switch (exp1_typ) {
      | Arrow(_t1, t2) =>
        OCamlExp("(" ++ exp1_str ++ " " ++ exp2_str ++ ")", t2)
      | _ => raise(OCamlExtraction_Typ.Typ_NotMatch("apply", "arrow"))
      }
    };
  | BoolLit(bool) => OCamlExp(string_of_bool(bool), Bool)
  | IntLit(int) => OCamlExp(string_of_int(int), Int)
  // contains special floats, already implemented in ocaml(https://caml.inria.fr/pub/docs/manual-ocaml/libref/Float.html)
  | FloatLit(float) =>
    let str =
      switch (string_of_float(float)) {
      | "inf" => "infinity"
      | "-inf" => "neg_infinity"
      | "nan" => "nan"
      | s => s
      };
    OCamlExp(str, Float);
  // Bin(Bool|Int|Float)Op can accept Lit or Op
  | BinBoolOp(op, d1, d2) =>
    switch (extract(ctx, d1), extract(ctx, d2)) {
    | (OCamlExp(exp1_str, Bool), OCamlExp(exp2_str, Bool)) =>
      let str_of_op: DHExp.BinBoolOp.t => string = (
        fun
        | And => " && "
        | Or => " || "
      );
      // add parenthesis to avoid evaluation priority
      OCamlExp("(" ++ exp1_str ++ str_of_op(op) ++ exp2_str ++ ")", Bool);
    | _ => raise(OCamlExtraction_Typ.Typ_NotMatch("bool operation", "bool"))
    }
  | BinIntOp(op, d1, d2) =>
    switch (extract(ctx, d1), extract(ctx, d2)) {
    | (OCamlExp(exp1_str, Int), OCamlExp(exp2_str, Int)) =>
      let str_of_op: DHExp.BinIntOp.t => (string, HTyp.t) = (
        fun
        | Minus => (" - ", Int)
        | Plus => (" + ", Int)
        | Times => (" * ", Int)
        | Divide => (" / ", Int)
        | LessThan => (" < ", Bool)
        | GreaterThan => (" > ", Bool)
        | Equals => (" == ", Bool)
      );
      // add parenthesis to avoid evaluation priority
      switch (str_of_op(op)) {
      | (op_str, res_typ) =>
        OCamlExp("(" ++ exp1_str ++ op_str ++ exp2_str ++ ")", res_typ)
      };
    | _ => raise(OCamlExtraction_Typ.Typ_NotMatch("int operation", "int"))
    }
  | BinFloatOp(op, d1, d2) =>
    switch (extract(ctx, d1), extract(ctx, d2)) {
    | (OCamlExp(exp1_str, Float), OCamlExp(exp2_str, Float)) =>
      let str_of_op: DHExp.BinFloatOp.t => (string, HTyp.t) = (
        fun
        | FPlus => (" +. ", Float)
        | FMinus => (" -. ", Float)
        | FTimes => (" *. ", Float)
        | FDivide => (" /. ", Float)
        | FLessThan => (" < ", Bool)
        | FGreaterThan => (" > ", Bool)
        | FEquals => (" == ", Bool)
      );
      // add parenthesis to avoid evaluation priority
      switch (str_of_op(op)) {
      | (op_str, res_typ) =>
        OCamlExp("(" ++ exp1_str ++ op_str ++ exp2_str ++ ")", res_typ)
      };
    | _ =>
      raise(OCamlExtraction_Typ.Typ_NotMatch("float operation", "float"))
    }
  | ListNil(t) => OCamlExp("[]", List(t))
  | Cons(d1, d2) =>
    //add proper parenthesis to avoid priority
    switch (extract(ctx, d1), extract(ctx, d2)) {
    | (OCamlExp(hd_str, _hd_typ), OCamlExp(tl_str, tl_typ)) =>
      switch (tl_typ) {
      | List(t) =>
        //removed typechek t == hd_typ
        OCamlExp("(" ++ hd_str ++ "::" ++ tl_str ++ ")", List(t))
      | _ => raise(OCamlExtraction_Typ.Typ_NotMatch("cons", "list"))
      }
    }
  | Inj(t, side, d) =>
    // t is the type of another side, for example int | bool, side = L, then t = bool
    // currently restrict t can't be Hole. It means we should explicitly declear the both side of sum type, i.e., let x : Int | Bool = ...
    // injection is the introductory of sum type in Hazel, as designed in Typ, we pack them as `Left and `Right constructor
    switch (extract(ctx, d)) {
    | OCamlExp(exp_str, exp_typ) =>
      switch (side) {
      | L => OCamlExp("(`Left " ++ exp_str ++ ")", Sum(exp_typ, t))
      | R => OCamlExp("(`Right " ++ exp_str ++ ")", Sum(t, exp_typ))
      }
    }
  | Pair(d1, d2) =>
    switch (extract(ctx, d1), extract(ctx, d2)) {
    | (OCamlExp(exp1_str, exp1_typ), OCamlExp(exp2_str, exp2_typ)) =>
      OCamlExp(
        "(" ++ exp1_str ++ ", " ++ exp2_str ++ ")",
        Prod([exp1_typ, exp2_typ]),
      )
    }
  | Triv => OCamlExp("()", Prod([])) //Prod([]) is the base type
  | ConsistentCase(cases) =>
    switch (cases) {
    | Case(d, rules, _) =>
      switch (extract(ctx, d)) {
      | OCamlExp(exp_str, exp_typ) =>
        let ocaml_rules = rules_extract(~ctx, ~rules, ~pat_t=exp_typ);
        switch (ocaml_rules) {
        | OCamlExp(rules_str, rules_typ) =>
          OCamlExp(
            "(match (" ++ exp_str ++ ") with \n" ++ rules_str ++ ")",
            rules_typ,
          )
        };
      }
    }
  // inconsistbranches is a Case having inconsistent types
  | InconsistentBranches(_, _, _, _) =>
    raise(Exp_Invalid(Some("inconsistent Case branches")))
  | Cast(_, _, _)
  | FailedCast(_, _, _) => raise(Exp_Invalid(Some("cast")))
  | InvalidOperation(_, _) => raise(Exp_Invalid(None))
  | InvalidText(_, _, _, err_text) =>
    raise(Exp_Invalid(Some("text " ++ err_text)))
  }
and rules_extract =
    (~ctx: Contexts.t, ~rules: list(DHExp.rule), ~pat_t: HTyp.t): t =>
  switch (rules) {
  | [] =>
    raise(Exp_Invalid(Some("zero or incomplete rules in pattern matching")))
  | [h] => rule_extract(~ctx, ~rule=h, ~pat_t)
  | [h, ...t] =>
    let head = rule_extract(~ctx, ~rule=h, ~pat_t);
    let tail = rules_extract(~ctx, ~rules=t, ~pat_t);
    switch (head, tail) {
    | (OCamlExp(hd_str, _hd_typ), OCamlExp(tl_str, tl_typ)) =>
      //removed consistent check HTyp.consistent(hd_typ, tl_typ)
      OCamlExp(hd_str ++ "\n" ++ tl_str, tl_typ)
    };
  }
and rule_extract = (~ctx: Contexts.t, ~rule: DHExp.rule, ~pat_t: HTyp.t): t =>
  switch (rule) {
  | Rule(pat, exp) =>
    let ctx' = OCamlExtraction_Pat.update_pattern(pat, pat_t, ctx);
    switch (OCamlExtraction_Pat.extract(pat), extract(ctx', exp)) {
    | (OCamlPat(pat_str), OCamlExp(exp_str, exp_typ)) =>
      OCamlExp("\t| " ++ pat_str ++ " -> " ++ exp_str, exp_typ)
    };
  };

module Extraction_wrapper = {
  type t =
    | OCamlExp(string)
    | ExtractionFailed(string);

  //TODO: test exception handling (seems good now)
  let extract = (ctx: Contexts.t, exp: DHExp.t): t =>
    switch (extract(ctx, exp)) {
    // normal return value
    | item =>
      switch (item) {
      | OCamlExp(exp_str, _exp_typ) => OCamlExp(exp_str)
      }
    // Exception handling
    //TODO: make the exceptions more human readable
    // type extraction exceptions
    | exception OCamlExtraction_Typ.Typ_Hole =>
      ExtractionFailed("Type exception: Hole exists in program")
    | exception (OCamlExtraction_Typ.Typ_NotMatch(expression, required_type)) =>
      ExtractionFailed(
        "Type exception: Expression "
        ++ expression
        ++ " requires "
        ++ required_type
        ++ " type",
      )
    // pattern extraction exceptions

    | exception OCamlExtraction_Pat.Pat_Hole =>
      ExtractionFailed("Pattern exception: Hole exists in program")
    | exception (OCamlExtraction_Pat.Pat_Keyword(keyword)) =>
      ExtractionFailed(
        "Pattern exception: Keyword "
        ++ ExpandingKeyword.to_string(keyword)
        ++ " is not allowed",
      )
    | exception (OCamlExtraction_Pat.Pat_Invalid(err)) =>
      ExtractionFailed("Pattern exception: " ++ err ++ " is invalid in OCaml")
    | exception (OCamlExtraction_Pat.Pat_InvalidUpdate(err)) =>
      ExtractionFailed(
        "Pattern exception: Pattern format \""
        ++ err
        ++ "\" is invalid in declaration",
      )
    // expression extraction exceptions
    | exception Exp_Hole =>
      ExtractionFailed("Expression exception: Hole exists in program")
    | exception (Exp_Keyword(keyword)) =>
      ExtractionFailed(
        "Expression exception: Keyword "
        ++ ExpandingKeyword.to_string(keyword)
        ++ " is not allowed",
      )
    | exception (Exp_Invalid(err)) =>
      switch (err) {
      | Some(err_s) =>
        ExtractionFailed(
          "Expression exception: "
          ++ err_s
          ++ " is invalid in OCaml, unable to extract",
        )
      | None => ExtractionFailed("Expression exception: Invalid Operation")
      }
    | exception (Exp_Fixpoint(s)) =>
      ExtractionFailed(
        "Expression exception: Fixpoint \""
        ++ s
        ++ "\" should be declared in Let expression",
      )
    };
};
