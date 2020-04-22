open Extraction_declear;
open Extraction_trans;
open Extraction_tool;
open Extraction_uhpat;
open Extraction_uhtyp;

// This file will extract UHExp

// directly use UHExp.string_of_operator to translate the operator
//Js.log(UHExp.string_of_operator(Space));

//TODO: add a new type STR, and modify pass_check.

// FIXME: If we encounter "?" holes, for example in Case, if we leave it empty, it isn't None, but Some(Hole) indeed
// It means when the user see a "? hole" but don't fill it, we receive Some(Hole) in AST. But if the user doesn't see a hole, it should be None
// Currently, we don't have None cases except for enter ":" in "let"
// TODO: It's safe and okey to transform Hole to None only for "? holes", i.e. call Extraction_tool.hole_to_none

// The extract_t is prepared for ExpLine
// In Letline, we can't actually evaluate the type for a let
//      but we can evaluate the block in let
//      Letline itself should have UNK, because we can't let forever
let rec uhexp_trans = (~t: UHExp.t, ~vs: variable_set_t): extract_t =>
  switch (t) {
  | [] => (Some(""), UNK)
  | [l] =>
    extract_t_combine(
      ~ex1=fst(line_trans(~l, ~vs)),
      ~ex2=(Some("\n"), UNK),
    )
  | [h, ...t] =>
    let res = line_trans(~l=h, ~vs);
    extract_t_combine(
      ~ex1=extract_t_combine(~ex1=fst(res), ~ex2=(Some("\n"), UNK)),
      ~ex2=uhexp_trans(~t, ~vs=snd(res)),
    );
  }
//return the modified variable set
and line_trans =
    (~l: UHExp.line, ~vs: variable_set_t): (extract_t, variable_set_t) =>
  switch (l) {
  | EmptyLine => ((Some(""), UNK), vs)
  //uht is option given, hence if given, don't need to inference
  | LetLine(uhp, uht, t) =>
    // snd should be EMPTY since not defined, here use p as a string
    let uht_h2N = Extraction_tool.hole_to_none(~uht);
    let p = (fst(uhpat_trans(~t=uhp, ~vs)), UNK);
    let exp = uhexp_trans(~t, ~vs);
    switch (uht_h2N) {
    | Some(a) =>
      let typ = uhtyp_trans(~t=a);
      let new_vs = add_variable(~v=(fst(p), snd(typ)), ~env=vs);
      let e =
        extract_t_concat(
          ~le=[
            (Some("let "), UNK),
            (Some("("), UNK),
            p,
            (Some(":"), UNK),
            (fst(typ), UNK),
            (Some(")"), UNK),
            (Some(" = "), UNK),
            (fst(exp), UNK),
            (Some(" in "), UNK),
          ],
        );
      (e, new_vs);
    // need to first evaluate t=block and get the type
    | None =>
      let new_vs = add_variable(~v=(fst(p), snd(exp)), ~env=vs);
      let e =
        extract_t_concat(
          ~le=[
            (Some("let "), UNK),
            (Some("("), UNK),
            p,
            (Some(":"), UNK),
            (pass_trans(~type1=snd(exp)), UNK),
            (Some(")"), UNK),
            (Some(" = "), UNK),
            (fst(exp), UNK),
            (Some(" in "), UNK),
          ],
        );
      (e, new_vs);
    };
  | ExpLine(opseq) =>
    switch (opseq) {
    | OpSeq(_oprand, a) => (uhexp_seq_trans(~t=a, ~vs), vs)
    }
  }
and uhexp_seq_trans =
    (~t: Seq.t('operand, 'operator), ~vs: variable_set_t): extract_t =>
  switch (t) {
  | S(operand, affix_e) =>
    switch (affix_e) {
    | E => uhexp_operand_trans(~ope=operand, ~vs)
    | A(operator, seqt) =>
      uhexp_const(
        ~ope1=operand,
        ~op=operator,
        ~ope2=uhexp_seq_trans(~t=seqt, ~vs),
        ~vs,
      )
    }
  }
and uhexp_operand_trans =
    (~ope: UHExp.operand, ~vs: variable_set_t): extract_t =>
  switch (ope) {
  | EmptyHole(_) => (Some(""), HOLE)
  | Var(err, v_err, s) =>
    switch (err, v_err) {
    | (NotInHole, NotInVarHole) => var_annotate(~var=s, ~vs)
    | _ => (Some(""), HOLE)
    }
  | NumLit(err, s) =>
    switch (err) {
    | NotInHole => (Some(string_of_int(s)), Number)
    | _ => (Some(""), HOLE)
    }
  | BoolLit(err, s) =>
    switch (err) {
    | NotInHole => (Some(string_of_bool(s)), Bool)
    | _ => (Some(""), HOLE)
    }
  | ListNil(err) =>
    switch (err) {
    | NotInHole => (Some("[]"), List(UNK))
    | _ => (Some(""), HOLE)
    }
  | Lam(err, uhp, uht, t) =>
    switch (err) {
    | NotInHole => lam_trans(~uhp, ~uht, ~t, ~vs)
    | _ => (Some(""), HOLE)
    }
  // just give the result, i.e. the picked type
  | Inj(err, side, t) =>
    switch (err) {
    | NotInHole => inj_trans(~side, ~t, ~vs)
    | _ => (Some(""), HOLE)
    }
  | Case(err, t, rules, uht) =>
    switch (err) {
    | NotInHole => case_trans(~t, ~rules, ~uht, ~vs)
    | _ => (Some(""), HOLE)
    }
  | Parenthesized(t) =>
    extract_t_concat(
      ~le=[(Some("("), UNK), uhexp_trans(~t, ~vs), (Some(")"), UNK)],
    )
  | ApPalette(_) => (Some("ApPalette not implemented"), CONFLICT)
  }
//note that lambda will be the type (A->B)
and lam_trans =
    (~uhp: UHPat.t, ~uht: option(UHTyp.t), ~t: UHExp.t, ~vs: variable_set_t)
    : extract_t => {
  let uht_h2N = Extraction_tool.hole_to_none(~uht);
  switch (uht_h2N) {
  | Some(typ) =>
    let v = (fst(uhpat_trans(~t=uhp, ~vs)), UNK);
    let x_t = uhtyp_trans(~t=typ);
    let new_vs = add_variable(~v=(fst(v), snd(x_t)), ~env=vs);
    let e_t = uhexp_trans(~t, ~vs=new_vs);
    let str =
      option_string_concat(
        ~strs=[
          Some("(fun "),
          Some("("),
          fst(v),
          Some(":"),
          fst(x_t),
          Some(")"),
          Some(" -> "),
          fst(e_t),
          Some(")"),
        ],
      );
    (str, ARROW(snd(x_t), snd(e_t)));
  | None =>
    // FIXME: how to infer the type of x from expression?
    // TODO: revise the implement to :
    // pass a UNK of x, and infer the expression, then the expression should be infered
    // TODO: Modify the pass check, whenever a UNK is determined, modify the variable set and return
    let v = (fst(uhpat_trans(~t=uhp, ~vs)), UNK);
    let new_vs = add_variable(~v=(fst(v), UNK), ~env=vs);
    let v_t = (Some("'a"), UNK);
    let e_t = uhexp_trans(~t, ~vs=new_vs);
    let str =
      option_string_concat(
        ~strs=[
          Some("(fun "),
          Some("("),
          fst(v),
          Some(":"),
          fst(v_t),
          Some(")"),
          Some(" -> "),
          fst(e_t),
          Some(")"),
        ],
      );
    (str, ARROW(snd(v_t), snd(e_t)));
  };
}
and inj_trans =
    (~side: InjSide.t, ~t: UHExp.t, ~vs: variable_set_t): extract_t => {
  // should accpet a sum type, and the expression is evaluated like no injection
  // the type should be injected due to side
  let exp = uhexp_trans(~t, ~vs);
  switch (snd(exp)) {
  | SUM(t1, t2) =>
    switch (side) {
    | L => (fst(exp), t1)
    | R => (fst(exp), t2)
    }
  | _ => (Some("Not a sum type"), CONFLICT)
  };
}
//ocaml doesn't support gradual type, so every case should have exactly the same type
and case_trans =
    (
      ~t: UHExp.t,
      ~rules: UHExp.rules,
      ~uht: option(UHTyp.t),
      ~vs: variable_set_t,
    )
    : extract_t => {
  let uht_h2N = Extraction_tool.hole_to_none(~uht);

  let x = uhexp_trans(~t, ~vs);
  let r = rules_trans(~x_t=snd(x), ~rules, ~uht=uht_h2N, ~vs);
  extract_t_concat(
    ~le=[
      (Some("((match "), UNK),
      (fst(x), UNK),
      (Some(" with\n"), UNK),
      r,
      (Some(") : "), UNK),
      (pass_trans(~type1=snd(r)), UNK),
      (Some(")"), UNK),
    ],
  );
}
//divide rules into rule_trans and check if all types are same
and rules_trans =
    (
      ~x_t: pass_t,
      ~rules: UHExp.rules,
      ~uht: option(UHTyp.t),
      ~vs: variable_set_t,
    )
    : extract_t => {
  let ext =
    switch (rules) {
    | [] => (Some(""), UNK)
    | [h] => rule_trans(~x_t, ~rule=h, ~vs)
    | [h, ...t] =>
      extract_t_combine(
        ~ex1=rule_trans(~x_t, ~rule=h, ~vs),
        ~ex2=rules_trans(~x_t, ~rules=t, ~uht, ~vs),
      )
    };
  switch (uht) {
  | Some(t) =>
    extract_t_combine(~ex1=ext, ~ex2=(Some(""), snd(uhtyp_trans(~t))))
  | None => ext
  };
}
//check whether matches is same type with x_t
//note add a \n to each rule
and rule_trans =
    (~x_t: pass_t, ~rule: UHExp.rule, ~vs: variable_set_t): extract_t =>
  switch (rule) {
  | Rule(uhp, t) =>
    let pat = uhpat_trans(~t=uhp, ~vs);
    let typ = pass_check(~type1=snd(pat), ~type2=x_t);
    switch (typ) {
    | HOLE => (None, HOLE)
    | CONFLICT => (Some("Pattern not match"), CONFLICT)
    | _ =>
      extract_t_concat(
        ~le=[
          (Some(" | "), UNK),
          (fst(pat), UNK),
          (Some(" -> "), UNK),
          uhexp_trans(~t, ~vs),
          (Some("\n"), UNK),
        ],
      )
    };
  }
and uhexp_const =
    (
      ~ope1: UHExp.operand,
      ~op: UHExp.operator,
      ~ope2: extract_t,
      ~vs: variable_set_t,
    )
    : extract_t => {
  let op1 = uhexp_operand_trans(~ope=ope1, ~vs);
  //TODO: maybe add annotations
  let str =
    option_string_concat(
      ~strs=[fst(op1), Some(UHExp.string_of_operator(op)), fst(ope2)],
    );
  switch (op) {
  //apply logic is same, test the function
  | Space => (
      str,
      Extraction_uhpat.pass_apply(~t1=snd(op1), ~t2=snd(ope2)),
    )
  | Plus =>
    switch (pass_check(~type1=snd(op1), ~type2=snd(ope2))) {
    | HOLE => (None, HOLE)
    | Number => (str, Number)
    | _ => (None, CONFLICT)
    }
  | Minus =>
    switch (pass_check(~type1=snd(op1), ~type2=snd(ope2))) {
    | HOLE => (None, HOLE)
    | Number => (str, Number)
    | _ => (None, CONFLICT)
    }
  | Times =>
    switch (pass_check(~type1=snd(op1), ~type2=snd(ope2))) {
    | HOLE => (None, HOLE)
    | Number => (str, Number)
    | _ => (None, CONFLICT)
    }
  | LessThan =>
    switch (pass_check(~type1=snd(op1), ~type2=snd(ope2))) {
    | HOLE => (None, HOLE)
    | Number => (str, Bool)
    | _ => (None, CONFLICT)
    }
  | GreaterThan =>
    switch (pass_check(~type1=snd(op1), ~type2=snd(ope2))) {
    | HOLE => (None, HOLE)
    | Number => (str, Bool)
    | _ => (None, CONFLICT)
    }
  | Equals =>
    switch (pass_check(~type1=snd(op1), ~type2=snd(ope2))) {
    | HOLE => (None, HOLE)
    | Number => (str, Bool)
    | _ => (None, CONFLICT)
    }
  | Comma =>
    switch (snd(op1), snd(ope2)) {
    | (HOLE, _) => (None, HOLE)
    | (_, HOLE) => (None, HOLE)
    | (_, CONFLICT) => (None, CONFLICT)
    | (CONFLICT, _) => (None, CONFLICT)
    | (_, EMPTY) => (Some("Variable not found"), CONFLICT)
    | (EMPTY, _) => (Some("Variable not found"), CONFLICT)
    | _ => (str, PROD(snd(op1), snd(ope2)))
    }
  | Cons =>
    switch (snd(op1), snd(ope2)) {
    | (HOLE, _) => (None, HOLE)
    | (_, HOLE) => (None, HOLE)
    | (_, CONFLICT) => (None, CONFLICT)
    | (CONFLICT, _) => (None, CONFLICT)
    | (_, EMPTY) => (Some("Variable not found"), CONFLICT)
    | (EMPTY, _) => (Some("Variable not found"), CONFLICT)
    | (a, List(UNK)) => (str, List(a))
    | (a, List(b)) => (str, pass_check(~type1=a, ~type2=b))
    | _ => (Some("Cons to not a List"), CONFLICT)
    }
  | And =>
    switch (pass_check(~type1=snd(op1), ~type2=snd(ope2))) {
    | HOLE => (None, HOLE)
    | Bool => (str, Bool)
    | _ => (None, CONFLICT)
    }
  | Or =>
    switch (pass_check(~type1=snd(op1), ~type2=snd(ope2))) {
    | HOLE => (None, HOLE)
    | Bool => (str, Bool)
    | _ => (None, CONFLICT)
    }
  };
};

let extraction_call = (~t: UHExp.t): string =>
  switch (uhexp_trans(~t, ~vs=[])) {
  | (_, HOLE) => "Uncomplete holes exist"
  | (None, CONFLICT) => "There's type unsupport in OCaml"
  | (Some(s), CONFLICT) => s ++ "Conflict Here"
  | (Some(s), _) => s
  | _ => "Something's wrong... "
  };
