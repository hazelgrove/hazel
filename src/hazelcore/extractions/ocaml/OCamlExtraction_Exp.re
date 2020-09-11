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
exception Exp_Invalid(string);
exception Exp_Fixpoint(string);

//at top level, we use de instead of d because it's used for destruction only
let rec extract = (ctx: Contexts.t, de: DHExp.t): t =>
  switch (de) {
  | EmptyHole(_)
  | NonEmptyHole(_) => raise(Exp_Hole)
  // used for variables that are keywords in Hazel, like "let" and "case"
  | Keyword(_, _, _, keyword) => raise(Exp_Keyword(keyword))
  // an expression with a free variable cannot be extracted to OCaml because free variables cause type errors in OCaml
  | FreeVar(_, _, _, var_name) =>
    raise(Exp_Invalid("free variable \"" ++ var_name ++ "\""))
  | BoundVar(s) =>
    //directly lookup type in environment
    let typ = VarCtx.lookup(Contexts.gamma(ctx), s);
    switch (typ) {
    | None => raise(Exp_Invalid("undeclared variable \"" ++ s ++ "\""))
    | Some(t) => OCamlExp(s, t)
    };
  // FixF is handled in Let,
  // Recursive functions can only declear by Let, this design is to keep the invariant
  | FixF(s, _ht, _d) => raise(Exp_Fixpoint(s))
  | Let(dp, d1, d2) =>
    // if d1 is a resursive function, should first update itself to context
    let OCamlExp(exp1_str, exp1_typ) =
      switch (d1) {
      | FixF(s, ht, d) =>
        //fixpoint function name should be added into the context to evaluate itself
        let ctx_fix = Contexts.extend_gamma(ctx, (s, ht));
        let OCamlExp(exp_str, _exp_typ) = extract(ctx_fix, d);
        OCamlExp(exp_str, ht);
      | _ => extract(ctx, d1)
      };
    //let can support various pattern declaration besides variable
    // extract and update the pattern
    let OCamlPat(pat_str, ctx') =
      OCamlExtraction_Pat.extract(dp, exp1_typ, ctx);
    let OCamlExp(exp2_str, exp2_typ) = extract(ctx', d2);
    let OCamlTyp(typ_str) = OCamlExtraction_Typ.extract(exp1_typ);
    //using "let x:t = d1 in d2" ocaml format, if d1 is a recursive function, use "let rec" instead
    let body =
      pat_str ++ " : " ++ typ_str ++ " = " ++ exp1_str ++ " in\n" ++ exp2_str;
    switch (d1) {
    | FixF(_, _, _) => OCamlExp("let rec " ++ body, exp2_typ)
    | _ => OCamlExp("let " ++ body, exp2_typ)
    };
  | Lam(dp, ht, d) =>
    //Htyp.t is the expected type of pattern
    // lambda can declear various patterns besides variable
    let OCamlPat(pat_str, ctx') = OCamlExtraction_Pat.extract(dp, ht, ctx);
    let OCamlTyp(typ_str) = OCamlExtraction_Typ.extract(ht);
    let OCamlExp(exp_str, exp_typ) = extract(ctx', d);
    OCamlExp(
      "(fun (" ++ pat_str ++ " : " ++ typ_str ++ ") -> (" ++ exp_str ++ "))",
      Arrow(ht, exp_typ),
    );
  //apply
  | Ap(d1, d2) =>
    let OCamlExp(exp1_str, exp1_typ) = extract(ctx, d1);
    let OCamlExp(exp2_str, _exp2_typ) = extract(ctx, d2);
    //removed the type check t1 == exp2_typ
    switch (exp1_typ) {
    | Arrow(_t1, t2) =>
      OCamlExp("(" ++ exp1_str ++ " " ++ exp2_str ++ ")", t2)
    | _ => raise(OCamlExtraction_Typ.Typ_NotMatch("apply", "arrow"))
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
    let OCamlExp(exp1_str, exp1_typ) = extract(ctx, d1);
    let OCamlExp(exp2_str, exp2_typ) = extract(ctx, d2);
    switch (exp1_typ, exp2_typ) {
    | (Bool, Bool) =>
      let str_of_op: DHExp.BinBoolOp.t => string = (
        fun
        | And => " && "
        | Or => " || "
      );
      // add parenthesis to avoid evaluation priority
      OCamlExp("(" ++ exp1_str ++ str_of_op(op) ++ exp2_str ++ ")", Bool);
    | _ => raise(OCamlExtraction_Typ.Typ_NotMatch("bool operation", "bool"))
    };
  | BinIntOp(op, d1, d2) =>
    let OCamlExp(exp1_str, exp1_typ) = extract(ctx, d1);
    let OCamlExp(exp2_str, exp2_typ) = extract(ctx, d2);
    switch (exp1_typ, exp2_typ) {
    | (Int, Int) =>
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
      let (op_str, res_typ) = str_of_op(op);
      OCamlExp("(" ++ exp1_str ++ op_str ++ exp2_str ++ ")", res_typ);
    | _ => raise(OCamlExtraction_Typ.Typ_NotMatch("int operation", "int"))
    };
  | BinFloatOp(op, d1, d2) =>
    let OCamlExp(exp1_str, exp1_typ) = extract(ctx, d1);
    let OCamlExp(exp2_str, exp2_typ) = extract(ctx, d2);
    switch (exp1_typ, exp2_typ) {
    | (Float, Float) =>
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
      let (op_str, res_typ) = str_of_op(op);
      OCamlExp("(" ++ exp1_str ++ op_str ++ exp2_str ++ ")", res_typ);
    | _ =>
      raise(OCamlExtraction_Typ.Typ_NotMatch("float operation", "float"))
    };
  | ListNil(t) => OCamlExp("[]", List(t))
  | Cons(d1, d2) =>
    let OCamlExp(hd_str, _hd_typ) = extract(ctx, d1);
    let OCamlExp(tl_str, tl_typ) = extract(ctx, d2);
    //add proper parenthesis to avoid priority
    switch (tl_typ) {
    | List(t) =>
      //removed typechek t == hd_typ
      OCamlExp("(" ++ hd_str ++ "::" ++ tl_str ++ ")", List(t))
    | _ => raise(OCamlExtraction_Typ.Typ_NotMatch("cons", "list"))
    };
  | Inj(t, side, d) =>
    // t is the type of another side, for example int | bool, side = L, then t = bool
    // currently restrict t can't be Hole. It means we should explicitly declear the both side of sum type, i.e., let x : Int | Bool = ...
    // injection is the introductory of sum type in Hazel, as designed in Typ, we pack them as `Left and `Right constructor
    let OCamlExp(exp_str, exp_typ) = extract(ctx, d);
    switch (side) {
    | L => OCamlExp("(`Left " ++ exp_str ++ ")", Sum(exp_typ, t))
    | R => OCamlExp("(`Right " ++ exp_str ++ ")", Sum(t, exp_typ))
    };
  | Pair(d1, d2) =>
    let OCamlExp(exp1_str, exp1_typ) = extract(ctx, d1);
    let OCamlExp(exp2_str, exp2_typ) = extract(ctx, d2);
    OCamlExp(
      "(" ++ exp1_str ++ ", " ++ exp2_str ++ ")",
      Prod([exp1_typ, exp2_typ]),
    );
  | Triv => OCamlExp("()", Prod([])) //Prod([]) is the base type
  | ConsistentCase(cases) =>
    let Case(d, rules, _) = cases;
    let OCamlExp(exp_str, exp_typ) = extract(ctx, d);
    let OCamlExp(rules_str, rules_typ) =
      rules_extract(~ctx, ~rules, ~pat_t=exp_typ);
    OCamlExp(
      "(match (" ++ exp_str ++ ") with \n" ++ rules_str ++ ")",
      rules_typ,
    );
  // inconsistbranches is a Case having inconsistent types
  | InconsistentBranches(_, _, _, _) =>
    raise(Exp_Invalid("inconsistent Case branches"))
  | Cast(_, _, _)
  | FailedCast(_, _, _) => raise(Exp_Invalid("cast"))
  | InvalidOperation(_, err) =>
    let err_msg = InvalidOperationError.err_msg(err);
    raise(Exp_Invalid(err_msg));
  | InvalidText(_, _, _, err_text) =>
    raise(Exp_Invalid("text " ++ err_text))
  }
and rules_extract =
    (~ctx: Contexts.t, ~rules: list(DHExp.rule), ~pat_t: HTyp.t): t =>
  switch (rules) {
  | [] => raise(Exp_Invalid("zero or incomplete rules in pattern matching"))
  | [h] => rule_extract(~ctx, ~rule=h, ~pat_t)
  | [h, ...t] =>
    let OCamlExp(hd_str, _hd_typ) = rule_extract(~ctx, ~rule=h, ~pat_t);
    let OCamlExp(tl_str, tl_typ) = rules_extract(~ctx, ~rules=t, ~pat_t);
    //removed consistent check HTyp.consistent(hd_typ, tl_typ)
    OCamlExp(hd_str ++ "\n" ++ tl_str, tl_typ);
  }
and rule_extract = (~ctx: Contexts.t, ~rule: DHExp.rule, ~pat_t: HTyp.t): t => {
  let Rule(pat, exp) = rule;
  let OCamlPat(pat_str, ctx') = OCamlExtraction_Pat.extract(pat, pat_t, ctx);
  let OCamlExp(exp_str, exp_typ) = extract(ctx', exp);
  OCamlExp("\t| " ++ pat_str ++ " -> " ++ exp_str, exp_typ);
};
