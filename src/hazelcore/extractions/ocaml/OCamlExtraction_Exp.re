/* Theorem: If extract(ctx, d) = Some(ce, ty) and ctx |- d : ty then ce :_ocaml Typ.extract(ty) */

type t =
  | OcamlExp(ocaml_exp, HTyp.t)
  | ExtractionFailed(string)
and ocaml_exp = string;

//TODO: replace == with HTyp.consistent
let rec extract = (ctx: Contexts.t, de: DHExp.t): t =>
  switch (de) {
  | EmptyHole(_) => ExtractionFailed("Exp: Empty Hole")
  | NonEmptyHole(_) => ExtractionFailed("Exp: Non-Empty Hole")
  // used for variables that are keywords in Hazel, like "let" and "case"
  | Keyword(_) => ExtractionFailed("Exp: Incomplete Program (Keyword)")
  // an expression with a free variable cannot be extracted to OCaml because free variables cause type errors in OCaml
  | FreeVar(_, _, _, _) => ExtractionFailed("Exp: FreeVar is not allowed")
  | BoundVar(x) =>
    //directly lookup type in environment
    let typ = VarCtx.lookup(Contexts.gamma(ctx), x);
    switch (typ) {
    | None => ExtractionFailed("Exp: BoundVar " ++ x ++ " Not Found")
    | Some(t) => OcamlExp(x, t)
    };
  | Let(dp, de1, de2) =>
    // Let can bind patterns, so use "update_pattern" as rules (Case)
    let ocaml_dp = OCamlExtraction_Pat.extract(dp);
    let ocaml_de1 = extract(ctx, de1);
    switch (ocaml_dp, ocaml_de1) {
    | (ExtractionFailed(err), _)
    | (_, ExtractionFailed(err)) => ExtractionFailed(err)
    | (OcamlPat(pat_str), OcamlExp(exp1_str, exp1_typ)) =>
      // update the context
      switch (
        OCamlExtraction_Typ.extract(exp1_typ),
        OCamlExtraction_Pat.update_pattern(dp, exp1_typ, ctx),
      ) {
      | (ExtractionFailed(err), _)
      | (_, UpdateFailed(err)) => ExtractionFailed(err)
      | (OcamlTyp(typ_str), UpdateResult(ctx')) =>
        //extract the expression
        switch (extract(ctx', de2)) {
        | ExtractionFailed(err) => ExtractionFailed(err)
        | OcamlExp(exp2_str, exp2_typ) =>
          switch (de1) {
          // use the "let .. = .. in" format
          // if it's a recursive function, it's format of let ... = fix ..., we change it to "let rec"
          //, because the recursive function should be declared by "rec" only in ocaml
          | FixF(_, _, _) =>
            OcamlExp(
              "let rec "
              ++ pat_str
              ++ " : "
              ++ typ_str
              ++ " = "
              ++ exp1_str
              ++ " in\n"
              ++ exp2_str,
              exp2_typ,
            )
          | _ =>
            OcamlExp(
              "let "
              ++ pat_str
              ++ " : "
              ++ typ_str
              ++ " = "
              ++ exp1_str
              ++ " in\n"
              ++ exp2_str,
              exp2_typ,
            )
          }
        }
      }
    };
  | FixF(x, ht, de) =>
    //it's the case of fixpoint/ recursive functions
    //Hazel programs will only ever result in internal expressions containing let ... = fix ...
    //let rec f: some type = fun x -> f ... (is reasonable)
    // x should be already inserted into context

    // Here the function name should be added by let, do it here to be careful
    let ctx' = Contexts.extend_gamma(ctx, (x, ht));
    switch (extract(ctx', de)) {
    | ExtractionFailed(err) => ExtractionFailed(err)
    | OcamlExp(exp_str, _exp_typ) => OcamlExp(exp_str, ht)
    };
  | Lam(dp, ht, de) =>
    //Htyp.t is the ground type of pattern
    switch (dp) {
    | Var(x) =>
      let ocaml_dp = OCamlExtraction_Pat.extract(dp);
      let ocaml_ht = OCamlExtraction_Typ.extract(ht);
      let ctx' = Contexts.extend_gamma(ctx, (x, ht));
      let ocaml_de = extract(ctx', de);
      switch (ocaml_dp, ocaml_ht, ocaml_de) {
      | (ExtractionFailed(err), _, _)
      | (_, ExtractionFailed(err), _)
      | (_, _, ExtractionFailed(err)) => ExtractionFailed(err)
      | (OcamlPat(pat_str), OcamlTyp(typ_str), OcamlExp(exp_str, exp_typ)) =>
        OcamlExp(
          "(fun ("
          ++ pat_str
          ++ " : "
          ++ typ_str
          ++ ") -> ("
          ++ exp_str
          ++ "))",
          Arrow(ht, exp_typ),
        )
      };
    | _ => ExtractionFailed("Exp: Lambda not variable")
    }
  | Ap(de1, de2) =>
    //apply
    let ocaml_de1 = extract(ctx, de1);
    let ocaml_de2 = extract(ctx, de2);
    switch (ocaml_de1, ocaml_de2) {
    | (ExtractionFailed(err), _)
    | (_, ExtractionFailed(err)) => ExtractionFailed(err)
    | (OcamlExp(exp1_str, exp1_typ), OcamlExp(exp2_str, exp2_typ)) =>
      //FIXME: May can remove the type check
      switch (exp1_typ) {
      | Arrow(t1, t2) =>
        if (t1 == exp2_typ) {
          OcamlExp("(" ++ exp1_str ++ " " ++ exp2_str ++ ")", t2);
        } else {
          ExtractionFailed("Exp: Apply, type inconsistent");
        }
      | _ => ExtractionFailed("Exp: Apply, not an Arrow type")
      }
    };
  | BoolLit(bool) => OcamlExp(string_of_bool(bool), Bool)
  | IntLit(int) => OcamlExp(string_of_int(int), Int)
  // contains special floats, the float is already ocaml type
  | FloatLit(float) =>
    let str =
      switch (string_of_float(float)) {
      | "inf" => "infinity"
      | "-inf" => "neg_infinity"
      | "nan" => "nan"
      | s => s
      };
    OcamlExp(str, Float);
  // BinxxxOp can accept Lit or Op
  | BinBoolOp(op, d1, d2) =>
    switch (extract(ctx, d1), extract(ctx, d2)) {
    | (ExtractionFailed(err), _)
    | (_, ExtractionFailed(err)) => ExtractionFailed(err)
    | (OcamlExp(exp1_str, Bool), OcamlExp(exp2_str, Bool)) =>
      let str_of_op: DHExp.BinBoolOp.t => string = (
        fun
        | And => " && "
        | Or => " || "
      );
      // add parenthesis to avoid evaluation priority
      OcamlExp("(" ++ exp1_str ++ str_of_op(op) ++ exp2_str ++ ")", Bool);
    | _ => ExtractionFailed("Exp: BinBoolOp takes not Bool")
    }
  | BinIntOp(op, d1, d2) =>
    switch (extract(ctx, d1), extract(ctx, d2)) {
    | (ExtractionFailed(err), _)
    | (_, ExtractionFailed(err)) => ExtractionFailed(err)
    | (OcamlExp(exp1_str, Int), OcamlExp(exp2_str, Int)) =>
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
        OcamlExp("(" ++ exp1_str ++ op_str ++ exp2_str ++ ")", res_typ)
      };
    | _ => ExtractionFailed("Exp: BinIntOp takes not Int")
    }
  | BinFloatOp(op, d1, d2) =>
    switch (extract(ctx, d1), extract(ctx, d2)) {
    | (ExtractionFailed(err), _)
    | (_, ExtractionFailed(err)) => ExtractionFailed(err)
    | (OcamlExp(exp1_str, Float), OcamlExp(exp2_str, Float)) =>
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
        OcamlExp("(" ++ exp1_str ++ op_str ++ exp2_str ++ ")", res_typ)
      };
    | _ => ExtractionFailed("Exp: BinFloatOp takes not Float")
    }
  | ListNil(t) => OcamlExp("[]", List(t))
  | Cons(d1, d2) =>
    //add proper parenthesis to avoid priority
    switch (extract(ctx, d1), extract(ctx, d2)) {
    | (ExtractionFailed(err), _)
    | (_, ExtractionFailed(err)) => ExtractionFailed(err)
    | (OcamlExp(hd_str, hd_typ), OcamlExp(tl_str, tl_typ)) =>
      switch (tl_typ) {
      | List(t) =>
        if (t == hd_typ) {
          OcamlExp("(" ++ hd_str ++ "::" ++ tl_str ++ ")", List(t));
        } else {
          ExtractionFailed("Exp: Cons, type inconsistent");
        }
      | _ => ExtractionFailed("Exp: Cons, need to be a list")
      }
    }
  | Inj(t, side, de) =>
    // t is the type of another side, for example int | bool, side = L, then t = bool
    // TODO: do we really need to restrict t can't be Hole? It means we should explicitly declear the both side of sum type, i.e., let x : Int | Bool = ...
    // injection is the introductory of sum type in hazel, as designed in Typ, we pack them as `Left and `Right constructor
    switch (extract(ctx, de)) {
    | ExtractionFailed(err) => ExtractionFailed(err)
    | OcamlExp(exp_str, exp_typ) =>
      switch (side) {
      | L => OcamlExp("(`Left " ++ exp_str ++ ")", Sum(exp_typ, t))
      | R => OcamlExp("(`Right " ++ exp_str ++ ")", Sum(t, exp_typ))
      }
    }
  | Pair(d1, d2) =>
    switch (extract(ctx, d1), extract(ctx, d2)) {
    | (ExtractionFailed(err), _)
    | (_, ExtractionFailed(err)) => ExtractionFailed(err)
    | (OcamlExp(exp1_str, exp1_typ), OcamlExp(exp2_str, exp2_typ)) =>
      OcamlExp(
        "(" ++ exp1_str ++ ", " ++ exp2_str ++ ")",
        Prod([exp1_typ, exp2_typ]),
      )
    }
  | Triv => OcamlExp("()", Prod([])) //Prod([]) is the unit type
  | ConsistentCase(cases) =>
    switch (cases) {
    // the int seems useless
    | Case(de, rules, _) =>
      switch (extract(ctx, de)) {
      | ExtractionFailed(err) => ExtractionFailed(err)
      | OcamlExp(exp_str, exp_typ) =>
        let ocaml_rules = rules_extract(~ctx, ~rules, ~pat_t=exp_typ);
        switch (ocaml_rules) {
        | ExtractionFailed(err) => ExtractionFailed(err)
        | OcamlExp(rules_str, rules_typ) =>
          OcamlExp(
            "(match (" ++ exp_str ++ ") with \n" ++ rules_str ++ ")",
            rules_typ,
          )
        };
      }
    }
  // inconsistbranches is a case have inconsistent types
  | InconsistentBranches(_, _, _, _) =>
    ExtractionFailed("Exp: Ocaml don't allow inconsistent branches")
  | Cast(_, _, _) => ExtractionFailed("Exp: cast not allowed")
  | FailedCast(_, _, _) => ExtractionFailed("Exp: cast not allowed")
  | InvalidOperation(_, _) => ExtractionFailed("Exp: Invalid Operation")
  | InvalidText(_, _, _, err) =>
    ExtractionFailed("Exp: Invalid Text " ++ err)
  }
and rules_extract =
    (~ctx: Contexts.t, ~rules: list(DHExp.rule), ~pat_t: HTyp.t): t =>
  switch (rules) {
  | [] => failwith("Exp: No rules in a Case")
  | [h] => rule_extract(~ctx, ~rule=h, ~pat_t)
  | [h, ...t] =>
    let head = rule_extract(~ctx, ~rule=h, ~pat_t);
    let tail = rules_extract(~ctx, ~rules=t, ~pat_t);
    switch (head, tail) {
    | (ExtractionFailed(err), _)
    | (_, ExtractionFailed(err)) => ExtractionFailed(err)
    | (OcamlExp(hd_str, hd_typ), OcamlExp(tl_str, tl_typ)) =>
      if (HTyp.consistent(hd_typ, tl_typ)) {
        OcamlExp(hd_str ++ "\n" ++ tl_str, tl_typ);
      } else {
        ExtractionFailed("Exp: Case rules with inconsistent results");
      }
    };
  }
and rule_extract = (~ctx: Contexts.t, ~rule: DHExp.rule, ~pat_t: HTyp.t): t =>
  switch (rule) {
  | Rule(dp, de) =>
    switch (
      OCamlExtraction_Pat.extract(dp),
      OCamlExtraction_Pat.update_pattern(dp, pat_t, ctx),
    ) {
    // Add pattern to context
    | (ExtractionFailed(err), _)
    | (_, UpdateFailed(err)) => ExtractionFailed(err)
    | (OcamlPat(dp_str), UpdateResult(ctx')) =>
      //extract the expression
      switch (extract(ctx', de)) {
      | ExtractionFailed(err) => ExtractionFailed(err)
      | OcamlExp(exp_str, exp_typ) =>
        OcamlExp("\t| " ++ dp_str ++ " -> " ++ exp_str, exp_typ)
      }
    }
  };
