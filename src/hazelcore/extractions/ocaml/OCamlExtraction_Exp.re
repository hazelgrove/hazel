// (extracted expression, dynamic/inner type) like ExpandResult
// Htyp.t is like the ground type

//type extract_result = option((ocaml_exp, HTyp.t));
type t = (ocaml_exp, HTyp.t)
and ocaml_exp = string;

/* Theorem: If extract(ctx, d) = Some(ce, ty) and ctx |- d : ty then ce :_ocaml Typ.extract(ty) */

//TODO: replace == with HTyp.consistent
let rec extract = (~ctx: Contexts.t, ~de: DHExp.t): t =>
  switch (de) {
  | EmptyHole(_) => failwith("Exp: Empty Hole")
  | NonEmptyHole(_) => failwith("Exp: Non-Empty Hole")
  // used for variables that are keywords in Hazel, like "let" and "case"
  | Keyword(_) => failwith("Exp: Incomplete Program (Keyword)")
  // an expression with a free variable cannot be extracted to OCaml because free variables cause type errors in OCaml
  | FreeVar(_, _, _, _) => failwith("Exp: FreeVar is not allowed")
  | BoundVar(x) =>
    //directly lookup type in environment
    let typ = VarCtx.lookup(Contexts.gamma(ctx), x);
    switch (typ) {
    | None => failwith("Exp: BoundVar " ++ x ++ " Not Found")
    // if the hole type, we don't print the type
    | Some(t) => (x, t)
    // if (t == Hole) {
    //   ("(" ++ x ++ ")", t);
    // } else {
    //   ("(" ++ x ++ " : " ++ Typ.extract(~t) ++ ")", t);
    // }
    };
  | Let(dp, de1, de2) =>
    switch (dp) {
    | Var(x) =>
      let ocaml_dp = OCamlExtraction_Pat.extract(~dp);
      let ocaml_de1 = extract(~ctx, ~de=de1);
      let ctx' = Contexts.extend_gamma(ctx, (x, snd(ocaml_de1)));
      let ocaml_de2 = extract(~ctx=ctx', ~de=de2);
      // use the "let .. = ..;;" format
      // if it's a recursive function, it's format of let ... = fix ..., we change it to "let rec"
      //, because the recursive function should be declared by "rec" only in ocaml
      switch (de1) {
      | FixF(_, _, _) => (
          "let rec "
          ++ ocaml_dp
          ++ " : "
          ++ OCamlExtraction_Typ.extract(~t=snd(ocaml_de1))
          ++ " = "
          ++ fst(ocaml_de1)
          ++ ";;\n"
          ++ fst(ocaml_de2),
          snd(ocaml_de2),
          // fst(ocaml_de1) ++ fst(ocaml_de2),
          // snd(ocaml_de2),
        )
      | _ => (
          "let "
          ++ ocaml_dp
          ++ " : "
          ++ OCamlExtraction_Typ.extract(~t=snd(ocaml_de1))
          ++ " = "
          ++ fst(ocaml_de1)
          ++ ";;\n"
          ++ fst(ocaml_de2),
          snd(ocaml_de2),
        )
      };
    | _ => failwith("Exp: Let, not a variable pattern")
    }
  | FixF(x, ht, de) =>
    //it's the case of fixpoint/ recursive functions
    //Hazel programs will only ever result in internal expressions containing let ... = fix ...
    //let rec f: some type = fun x -> f ... (is reasonable)
    // x should be already inserted into context

    //let ocaml_ht = OCamlExtraction_Typ.extract(~t=ht);
    let ctx' = Contexts.extend_gamma(ctx, (x, ht));
    let ocaml_de = extract(~ctx=ctx', ~de);
    (
      // "let rec " ++ x ++ " : " ++ ocaml_ht ++ " = " ++ fst(ocaml_de) ++ ";;\n",
      fst(ocaml_de),
      ht,
    );

  | Lam(dp, ht, de) =>
    //Htyp.t is the ground type of pattern
    switch (dp) {
    | Var(x) =>
      let ocaml_dp = OCamlExtraction_Pat.extract(~dp);
      let ocaml_ht = OCamlExtraction_Typ.extract(~t=ht);
      let ctx' = Contexts.extend_gamma(ctx, (x, ht));
      let ocaml_de = extract(~ctx=ctx', ~de);
      (
        "(fun ("
        ++ ocaml_dp
        ++ " : "
        ++ ocaml_ht
        ++ ") -> ("
        ++ fst(ocaml_de)
        ++ "))",
        Arrow(ht, snd(ocaml_de)),
      );
    | _ => failwith("Exp: Lambda not variable")
    }
  | Ap(de1, de2) =>
    //apply
    let ocaml_de1 = extract(~ctx, ~de=de1);
    let ocaml_de2 = extract(~ctx, ~de=de2);
    switch (snd(ocaml_de1)) {
    | Arrow(t1, t2) =>
      // Hole -> some type should be ok
      if (t1 == snd(ocaml_de2) || t1 == Hole) {
        ("(" ++ fst(ocaml_de1) ++ " " ++ fst(ocaml_de2) ++ ")", t2);
      } else {
        failwith("Exp: Apply, type inconsistent");
      }
    | _ => failwith("Exp: Apply, not an Arrow type")
    };
  | BoolLit(bool) => (string_of_bool(bool), Bool)
  | IntLit(int) => (string_of_int(int), Int)
  | FloatLit(float) => (string_of_float(float), Float)
  | BinBoolOp(op, d1, d2) =>
    switch (d1, d2) {
    | (BoolLit(b1), BoolLit(b2)) =>
      let str_of_op: DHExp.BinBoolOp.t => string = (
        fun
        | And => " && "
        | Or => " || "
      );
      // add parenthesis to avoid evaluation priority
      let str =
        "("
        ++ string_of_bool(b1)
        ++ str_of_op(op)
        ++ string_of_bool(b2)
        ++ ")";
      (str, Bool);
    | _ => failwith("Exp: BinBoolOp")
    }
  | BinIntOp(op, d1, d2) =>
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
    let ocaml_op = str_of_op(op);
    let ocaml_d1 = extract(~ctx, ~de=d1);
    let ocaml_d2 = extract(~ctx, ~de=d2);
    switch (snd(ocaml_d1), snd(ocaml_d2)) {
    | (Int, Int) => (
        "(" ++ fst(ocaml_d1) ++ fst(ocaml_op) ++ fst(ocaml_d2) ++ ")",
        snd(ocaml_op),
      )
    | _ => failwith("Exp: BinIntOp")
    };
  | BinFloatOp(op, d1, d2) =>
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
    let ocaml_op = str_of_op(op);
    let ocaml_d1 = extract(~ctx, ~de=d1);
    let ocaml_d2 = extract(~ctx, ~de=d2);
    switch (snd(ocaml_d1), snd(ocaml_d2)) {
    | (Float, Float) => (
        "(" ++ fst(ocaml_d1) ++ fst(ocaml_op) ++ fst(ocaml_d2) ++ ")",
        snd(ocaml_op),
      )
    | _ => failwith("Exp: BinFloatOp")
    };
  | ListNil(t) => ("[]", List(t))
  | Cons(d1, d2) =>
    //TODO: add proper parenthesis to avoid priority in case "map"
    let ocaml_d1 = extract(~ctx, ~de=d1);
    let ocaml_d2 = extract(~ctx, ~de=d2);
    switch (snd(ocaml_d2)) {
    //FIXME: will ever happen? test!
    | Hole => (
        "(" ++ fst(ocaml_d1) ++ "::" ++ fst(ocaml_d2) ++ ")",
        List(snd(ocaml_d1)),
      )
    //d2 is already a list
    | List(t) =>
      if (t == snd(ocaml_d1) || t == Hole) {
        (
          "(" ++ fst(ocaml_d1) ++ "::" ++ fst(ocaml_d2) ++ ")",
          snd(ocaml_d2),
        );
      } else {
        failwith("Exp: Cons, type inconsistent");
      }
    | _ => failwith("Exp: Cons")
    };
  | Inj(t, side, de) =>
    // t is the type of another side, for example int | bool, side = L, then t = bool
    // FIXME: do we really need to restrict t can't be Hole? It means we should explicitly declear the both side of sum type, i.e., let x : Int | Bool = ...
    // injection is the introductory of sum type in hazel, as designed in Typ, we pack them as `Left and `Right constructor
    let ocaml_exp = extract(~ctx, ~de);
    switch (side) {
    | L => ("(`Left " ++ fst(ocaml_exp) ++ ")", Sum(snd(ocaml_exp), t))
    | R => ("(`Right " ++ fst(ocaml_exp) ++ ")", Sum(t, snd(ocaml_exp)))
    };
  | Pair(d1, d2) =>
    let ocaml_d1 = extract(~ctx, ~de=d1);
    let ocaml_d2 = extract(~ctx, ~de=d2);
    (
      "(" ++ fst(ocaml_d1) ++ ", " ++ fst(ocaml_d2) ++ ")",
      Prod([snd(ocaml_d1), snd(ocaml_d2)]),
    );
  | Triv => ("()", Hole) //We have no unit Htype...
  | ConsistentCase(cases) =>
    switch (cases) {
    // the int seems useless
    | Case(de, rules, _) =>
      let ocaml_de = extract(~ctx, ~de);
      let ocaml_rules = rules_extract(~ctx, ~rules, ~pat_t=snd(ocaml_de));
      //FIXME: Do we really need to indicate the type?
      let str =
        "((match ("
        ++ fst(ocaml_de)
        ++ ") with \n"
        ++ fst(ocaml_rules)
        ++ ") : "
        ++ OCamlExtraction_Typ.extract(~t=snd(ocaml_rules))
        ++ ")";
      (str, snd(ocaml_rules));
    }
  // inconsistbranches is a case have inconsistent types
  | InconsistentBranches(_, _, _, _) =>
    failwith("Exp: Ocaml don't allow inconsistent branches")
  | Cast(_, _, _) => failwith("Exp: cast not allowed")
  | FailedCast(_, _, _) => failwith("Exp: cast not allowed")
  | InvalidOperation(_, _) => failwith("Exp: Invalid Operation")
  }
and rules_extract =
    (~ctx: Contexts.t, ~rules: list(DHExp.rule), ~pat_t: HTyp.t): t =>
  switch (rules) {
  | [] => ("", Hole) //no rules, won't happen actually
  | [h] => rule_extract(~ctx, ~rule=h, ~pat_t)
  | [h, ...t] =>
    let head = rule_extract(~ctx, ~rule=h, ~pat_t);
    let tail = rules_extract(~ctx, ~rules=t, ~pat_t);
    // if (snd(head) == snd(tail)) {
    if (HTyp.consistent(snd(head), snd(tail))) {
      (fst(head) ++ "\n" ++ fst(tail), snd(head));
    } else {
      failwith("Exp: Case rules with inconsistent results");
    };
  }
and rule_extract = (~ctx: Contexts.t, ~rule: DHExp.rule, ~pat_t: HTyp.t): t =>
  switch (rule) {
  | Rule(dp, de) =>
    let ocaml_dp = OCamlExtraction_Pat.extract(~dp);
    //we seems don't allow constructors as pattern
    //we should add the patterns into the context
    let ctx' = OCamlExtraction_Pat.update_pattern(dp, pat_t, ctx);
    let ocaml_de = extract(~ctx=ctx', ~de);
    ("\t| " ++ ocaml_dp ++ " -> " ++ fst(ocaml_de), snd(ocaml_de));
  };
