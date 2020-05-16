open Extraction_declear;
open Extraction_tool;

//FIXME: Test the function
// handle apply case
// for example (a->b->c a->b) should result in c
// only t1 allows to have UNK,
// the logic is a b c = a (b c), arrow logic is same
// Hence the structure should be ARROW(t, ARROW(...))
// if apply can't match type, return conflict
let rec pass_apply = (~t1: pass_t, ~t2: pass_t): pass_t =>
  switch (t1) {
  | ARROW(pre, con) =>
    switch (t2) {
    //here pre should only be non-ARROW case
    | CONFLICT => CONFLICT
    | UNK => CONFLICT //can't apply a ('a) as argument
    | HOLE => HOLE
    | EMPTY => CONFLICT
    | ARROW(p2, rest) =>
      if (pass_apply_eq(~t1=pre, ~t2=p2)) {
        pass_apply(~t1=con, ~t2=rest);
      } else {
        CONFLICT;
      }
    | _ =>
      if (pass_apply_eq(~t1=pre, ~t2)) {
        con;
      } else {
        CONFLICT;
      }
    }
  | _ => CONFLICT
  }
// check if t1 = t2, or if t1 = UNK
// if t2 is a compound type, check the head
// only need to compare non-ARROW case, or violate the right-bound logic
and pass_apply_eq = (~t1: pass_t, ~t2: pass_t): bool =>
  switch (t1, t2) {
  | (UNK, _) => true
  | (Bool, Bool) => true
  | (Number, Number) => true
  | (Unit, Unit) => true
  | (List(a), List(b)) => pass_apply_eq(~t1=a, ~t2=b)
  | (SUM(a1, b1), SUM(a2, b2)) =>
    pass_apply_eq(~t1=a1, ~t2=a2) && pass_apply_eq(~t1=b1, ~t2=b2)
  | (PROD(a1, b1), PROD(a2, b2)) =>
    pass_apply_eq(~t1=a1, ~t2=a2) && pass_apply_eq(~t1=b1, ~t2=b2)
  | _ => false
  };

// handle the case of a::b
let pass_list = (~t1: pass_t, ~t2: pass_t): pass_t =>
  switch (t1, t2) {
  | (HOLE, _) => HOLE
  | (_, List(HOLE)) => HOLE
  | (UNK, _) => CONFLICT
  | (a, List(b)) =>
    switch (pass_check(~type1=b, ~type2=a)) {
    //reverse since the UNK case
    | EMPTY => CONFLICT
    | HOLE => HOLE
    | CONFLICT => CONFLICT
    | UNK => CONFLICT //a::b should have a type
    | _ => List(pass_check(~type1=b, ~type2=a))
    }
  | _ => CONFLICT
  };

let rec uhpat_trans = (~t: UHPat.t, ~vs: variable_set_t): extract_t =>
  switch (t) {
  | OpSeq(_oprand, a) => uhpat_seq_trans(~t=a, ~vs)
  }
and uhpat_seq_trans =
    (~t: Seq.t('operand, 'operator), ~vs: variable_set_t): extract_t =>
  switch (t) {
  //I don't think oprand is necessary
  | S(operand, affix_e) =>
    switch (affix_e) {
    | E => uhpat_operand_trans(~ope=operand, ~vs) //only an operand
    | A(operator, seqt) =>
      uhpat_const(
        ~ope1=operand,
        ~op=operator,
        ~ope2=uhpat_seq_trans(~t=seqt, ~vs),
        ~vs,
      )
    }
  }
and uhpat_operand_trans =
    (~ope: UHPat.operand, ~vs: variable_set_t): extract_t =>
  switch (ope) {
  | EmptyHole(_) => (None, HOLE) //it's a HOLE
  | Wild(er) =>
    switch (er) {
    | NotInHole => (Some("_"), UNK)
    | _ => (None, HOLE)
    }
  | Var(er, ve, var) =>
    switch (er, ve) {
    | (NotInHole, NotInVarHole) => (
        Some(var),
        find_variable_set(~var, ~set=vs),
      )
    | _ => (None, HOLE)
    }
  | NumLit(a, b) =>
    switch (a) {
    | NotInHole => (Some(string_of_int(b)), Number)
    | _ => (None, HOLE)
    }
  | BoolLit(a, b) =>
    switch (a) {
    | NotInHole => (Some(string_of_bool(b)), Bool)
    | _ => (None, HOLE)
    }
  | ListNil(a) =>
    switch (a) {
    | NotInHole => (Some("[]"), List(UNK))
    | _ => (None, HOLE)
    }
  | Parenthesized(t) =>
    extract_t_concat(
      ~le=[(Some("("), UNK), uhpat_trans(~t, ~vs), (Some(")"), UNK)],
    )
  | Inj(er, injs, t) =>
    switch (er, uhpat_trans(~t, ~vs)) {
    | (NotInHole, (s, SUM(l, r))) =>
      switch (injs) {
      | L => (s, l)
      | R => (s, r)
      }
    | (NotInHole, _) => (None, CONFLICT)
    | _ => (None, HOLE)
    }
  }
and uhpat_const =
    (
      ~ope1: UHPat.operand,
      ~op: UHPat.operator,
      ~ope2: extract_t,
      ~vs: variable_set_t,
    )
    : extract_t => {
  let (s1, p1) = uhpat_operand_trans(~ope=ope1, ~vs);
  let (s2, p2) = ope2;
  switch (op) {
  | Comma => (
      option_string_concat(~strs=[s1, Some(","), s2]),
      PROD(p1, p2),
    )
  | Space => (
      option_string_concat(~strs=[s1, Some(" "), s2]),
      pass_apply(~t1=p1, ~t2=p2),
    )
  | Cons => (
      option_string_concat(~strs=[s1, Some("::"), s2]),
      pass_list(~t1=p1, ~t2=p2),
    )
  };
};
