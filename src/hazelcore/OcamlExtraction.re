module Typ = {
  type ocaml_typ = string;

  let rec extract = (~t: HTyp.t): ocaml_typ =>
    switch (t) {
    | Hole => "'a"
    | Int => "int"
    | Float => "float"
    | Bool => "bool"
    // add parenthesis is necessary
    // (int -> int) -> int != int -> int -> int
    | Arrow(t1, t2) =>
      "(" ++ extract(~t=t1) ++ " -> " ++ extract(~t=t2) ++ ")"
    | Sum(t1, t2) => "(" ++ extract(~t=t1) ++ " | " ++ extract(~t=t2) ++ ")"
    | Prod(tl) => "(" ++ list_prod(~tl) ++ ")"
    | List(t) => extract(~t) ++ " list"
    }
  and list_prod = (~tl: list(HTyp.t)): ocaml_typ =>
    switch (tl) {
    | [] => ""
    | [h, ...t] => extract(~t=h) ++ " * " ++ list_prod(~tl=t)
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
    | Cons(dp1, dp2) => extract(~dp=dp1) ++ "::" ++ extract(~dp=dp2)
    | Pair(dp1, dp2) =>
      "(" ++ extract(~dp=dp1) ++ ", " ++ extract(~dp=dp2) ++ ")"
    | Triv => "()"
    | Ap(dp1, dp2) => extract(~dp=dp1) ++ " " ++ extract(~dp=dp2)
    };
  };
};

module Exp = {
  type ocaml_exp = string;

  // (extracted expression, dynamic/inner type) like ExpandResult
  // Htyp.t is like the ground type

  //type extract_result = option((ocaml_exp, HTyp.t));
  type extract_result = (ocaml_exp, HTyp.t);

  /* Theorem: If extract(ctx, d) = Some(ce, ty) and ctx |- d : ty then ce :_ocaml Typ.extract(ty) */

  //TODO: replace == with HTyp.consistent
  let rec extract = (~ctx: Contexts.t, ~de: DHExp.t): extract_result =>
    switch (de) {
    | EmptyHole(_) => failwith("Exp: Empty Hole")
    | NonEmptyHole(_) => failwith("Exp: Non-Empty Hole")
    // used for variables that are keywords in Hazel, like "let" and "case"
    | Keyword(_) => failwith("Exp: Incomplete Program (Keyword)")
    | FreeVar(_, _, map, x) =>
      //lookup the expression represented by x
      let map_e = VarMap.lookup(map, x);
      switch (map_e) {
      | None => failwith("Exp: FreeVar Not Found")
      | Some(e) =>
        let ocaml_e = extract(~ctx, ~de=e);
        //add a parenthesize to avoid any possible order priority
        (
          "(" ++ x ++ " : " ++ Typ.extract(~t=snd(ocaml_e)) ++ ")",
          snd(ocaml_e),
        );
      };
    | BoundVar(x) =>
      //directly lookup type in environment
      let typ = VarCtx.lookup(Contexts.gamma(ctx), x);
      switch (typ) {
      | None => failwith("Exp: BoundVar " ++ x ++ " Not Found")
      // if the hole type, we don't print the type
      | Some(t) =>
        if (t == Hole) {
          ("(" ++ x ++ ")", t);
        } else {
          ("(" ++ x ++ " : " ++ Typ.extract(~t) ++ ")", t);
        }
      };
    | Let(dp, de1, de2) =>
      switch (dp) {
      | Var(x) =>
        let ocaml_dp = Pat.extract(~dp);
        let ocaml_de1 = extract(~ctx, ~de=de1);
        let ctx' = Contexts.extend_gamma(ctx, (x, snd(ocaml_de1)));
        let ocaml_de2 = extract(~ctx=ctx', ~de=de2);
        // use the "let .. = ..;;" format
        // if it's a recursive function, we skip to directly evaluating the FixF (de1)
        // because the recursive function should be declared by "rec" only in ocaml
        switch (de1) {
        | FixF(_, _, _) => (
            fst(ocaml_de1) ++ fst(ocaml_de2),
            snd(ocaml_de2),
          )
        | _ => (
            "let "
            ++ ocaml_dp
            ++ " : "
            ++ Typ.extract(~t=snd(ocaml_de2))
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
      //let rec f: ... = fun x -> f ... (is reasonable)
      let ocaml_ht = Typ.extract(~t=ht);
      let ctx' = Contexts.extend_gamma(ctx, (x, ht));
      let ocaml_de = extract(~ctx=ctx', ~de);
      // here ht should equal snd(ocaml_de)
      (
        "let rec "
        ++ x
        ++ " : "
        ++ ocaml_ht
        ++ " = "
        ++ fst(ocaml_de)
        ++ ";;\n",
        ht,
      );
    | Lam(dp, ht, de) =>
      //Htyp.t is the ground type of pattern
      switch (dp) {
      | Var(x) =>
        let ocaml_dp = Pat.extract(~dp);
        let ocaml_ht = Typ.extract(~t=ht);
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
          (fst(ocaml_de1) ++ " " ++ fst(ocaml_de2), t2);
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
      let ocaml_d1 = extract(~ctx, ~de=d1);
      let ocaml_d2 = extract(~ctx, ~de=d2);
      switch (snd(ocaml_d2)) {
      //can't determine type
      | Hole => (
          fst(ocaml_d1) ++ "::" ++ fst(ocaml_d2),
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
      //ocaml don't have injection
      let ocaml_exp = extract(~ctx, ~de);
      switch (t) {
      | Sum(l, r) =>
        switch (side) {
        | L => (fst(ocaml_exp), l)
        | R => (fst(ocaml_exp), r)
        }
      | _ => failwith("Exp: Injection")
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
        let str =
          "((match ("
          ++ fst(ocaml_de)
          ++ ") with \n"
          ++ fst(ocaml_rules)
          ++ ") : "
          ++ Typ.extract(~t=snd(ocaml_rules))
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
      (~ctx: Contexts.t, ~rules: list(DHExp.rule), ~pat_t: HTyp.t)
      : extract_result =>
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
        failwith(
          "Exp: Case rules with inconsistent results"
          ++ "\n"
          ++ fst(head)
          ++ " is "
          ++ Typ.extract(~t=snd(head))
          ++ "\n"
          ++ Typ.extract(~t=snd(head))
          ++ "\n"
          ++ Typ.extract(~t=snd(tail)),
        );
      };
    }
  and rule_extract =
      (~ctx: Contexts.t, ~rule: DHExp.rule, ~pat_t: HTyp.t): extract_result =>
    switch (rule) {
    | Rule(dp, de) =>
      let ocaml_dp = Pat.extract(~dp);
      //we seems don't allow constructors as pattern
      //we should add the patterns into the context
      let rec update_ctx = (dp: DHPat.t, ctx: Contexts.t): Contexts.t =>
        switch (dp) {
        | Var(x) => Contexts.extend_gamma(ctx, (x, pat_t))
        | Inj(_, p) => update_ctx(p, ctx)
        | Cons(p1, p2) =>
          switch (pat_t) {
          | List(t) =>
            // only add variable into context
            let ctx1 =
              switch (p1) {
              | Var(x) => Contexts.extend_gamma(ctx, (x, t))
              | _ => ctx
              };
            switch (p2) {
            | Var(y) => Contexts.extend_gamma(ctx1, (y, List(t)))
            | _ => ctx1
            };
          | _ => failwith("Exp: Case wrong rule pattern, list")
          }
        //TODO: rewrite it more beautiful
        | Pair(p1, p2) =>
          switch (pat_t) {
          | Prod([h, t]) =>
            let ctx1 =
              switch (p1) {
              | Var(x) => Contexts.extend_gamma(ctx, (x, h))
              | _ => ctx
              };
            switch (p2) {
            | Var(y) => Contexts.extend_gamma(ctx1, (y, t))
            | _ => ctx1
            };
          | Prod([h, m, ...t]) =>
            let ctx1 =
              switch (p1) {
              | Var(x) => Contexts.extend_gamma(ctx, (x, h))
              | _ => ctx
              };
            switch (p2) {
            | Var(y) => Contexts.extend_gamma(ctx1, (y, Prod([m, ...t])))
            | _ => ctx1
            };
          | _ => failwith("Exp: Case wrong rule pattern, pair")
          }
        | Ap(_, _) => failwith("Exp: Case rule error, apply")
        | _ => ctx
        };
      let ctx' = update_ctx(dp, ctx);
      let ocaml_de = extract(~ctx=ctx', ~de);
      ("\t| " ++ ocaml_dp ++ " -> " ++ fst(ocaml_de), snd(ocaml_de));
    };
};
