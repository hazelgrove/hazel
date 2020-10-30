open OptUtil.Syntax;
open Option;
open Smyth.Lang;

/*

   The following datatype declarations:

   type Bool
     = True ()
     | False ()

   type Nat
     = Z ()
     | S Nat

   type NatList
     = Nil ()
     | Cons (Nat, NatList)

   Get translated into the following datatype_ctx:

   ((NatList (() ((Nil (TTuple ())) (Cons (TTuple ((TData Nat ()) (TData NatList ())))))))
    (Nat (() ((Z (TTuple ())) (S (TData Nat ())))))
    (Bool (() ((True (TTuple ())) (False (TTuple ()))))))

   Cleaned up:

  [
     ("NatList"
       ([]
        [
          (Nil (TTuple [])
          (Cons (TTuple [(TData Nat ()) (TData NatList ())])))
        ]
       )
    ("Nat"
       ([]
        [
          (Z (TTuple [])
          (S (TData Nat ())))
        ]
       )
    ("Bool"
       ([]
        [
          (True (TTuple [])
          (False (TTuple ())))
        ]
       )
   ]
 */

let datatype_prelude: Smyth.Lang.datatype_ctx = [
  ("Bool", ([], [("True", TTuple([])), ("False", TTuple([]))])),
  ("Nat", ([], [("Z", TTuple([])), ("S", TData("Nat", []))])),
  (
    "NatList",
    (
      [],
      [
        ("Nil", TTuple([])),
        ("Cons", TTuple([TData("Nat", []), TData("NatList", [])])),
      ],
    ),
  ),
];

type sm_pat = (string, Smyth.Lang.pat);
type sm_rule = (sm_pat, Smyth.Lang.exp);
type sm_def = (string, (Smyth.Lang.typ, Smyth.Lang.exp));
type sm_assert = (Smyth.Lang.exp, Smyth.Lang.exp);

let sm_ctor = (name, arg) => ECtor(name, [], arg);
let sm_null_ctor = name => sm_ctor(name, ETuple([]));
let sm_cons = (e1, e2) => sm_ctor("Cons", ETuple([e1, e2]));
let sm_zero = sm_null_ctor("Z");
let sm_true = sm_null_ctor("True");
let sm_false = sm_null_ctor("False");
let sm_nil = sm_null_ctor("Nil");

let rec sm_succ =
  fun
  | 0 => sm_zero
  | n => sm_ctor("S", sm_succ(n - 1));

let rec unsucc: Smyth.Lang.exp => int =
  fun
  | ECtor("S", _, smexp_1) => 1 + unsucc(smexp_1)
  | _ => 0; // in a perfect world this is ECtor("Z", [], _)

let str_to_smint = str => str |> int_of_string |> sm_succ;
[@warning "-32"]
let smint_to_string = smexp => smexp |> unsucc |> string_of_int;

let rec htyp_to_styp: HTyp.t => option(Smyth.Lang.typ) =
  Smyth.Lang.(
    fun
    | Hole
    | Float
    | Sum(_) => None
    | Int => Some(TData("Nat", []))
    | Bool => Some(TData("Bool", []))
    | List(Int) => Some(TData("NatList", []))
    | List(_) => None
    | Prod(h_ts) => {
        let* s_ts = List.map(htyp_to_styp, h_ts) |> OptUtil.sequence;
        Some(TTuple(s_ts));
      }
    | Arrow(h_t1, h_t2) => {
        let* s_t1 = htyp_to_styp(h_t1);
        let* s_t2 = htyp_to_styp(h_t2);
        Some(TArr(s_t1, s_t2));
      }
  );

let rec top_hexp_to_smprog = (lines: UHExp.t): option(Smyth.Desugar.program) => {
  // take first expression as entry point
  let sm_main =
    switch (List.filter_map(h_expline_to_sm_exp, lines)) {
    | [e, ..._] => Some(e)
    | _ => None
    };
  Smyth.Desugar.(
    Some({
      datatypes: datatype_prelude,
      definitions: List.filter_map(h_letline_to_sm_def, lines),
      assertions: List.filter_map(h_assert_to_sm_assert, lines),
      main_opt: sm_main,
    })
  );
}

and non_top_hexp_to_smexp = (hexp: UHExp.t): option(Smyth.Lang.exp) => {
  /* Non-top-level case:
     For now, assume there are no non-top letlines
     or commmentlines, or emptylines, or asserts

     TODO: non-top-level lets defining lambdas into fixes
     TODO: could rewrite lets into lambdas
     */
  switch (hexp) {
  | [ExpLine(opseq), ..._] => hexp_opseq_to_smexp(opseq)
  | _ => None
  };
}

and h_expline_to_sm_exp: UHExp.line => option(Smyth.Lang.exp) =
  fun
  | ExpLine(OpSeq(_, S(AssertLit(_), _))) => None // skip asserts
  | ExpLine(opseq) => hexp_opseq_to_smexp(opseq)
  | _ => None

and h_letline_to_sm_def: UHExp.line => option(sm_def) =
  fun
  | LetLine(OpSeq(_, S(Var(_, _, name), E)), Some(h_ty), h_exp) => {
      let* sm_ty = h_ty |> UHTyp.expand |> htyp_to_styp;
      let* sm_exp = non_top_hexp_to_smexp(h_exp);
      Some((name, (sm_ty, sm_exp)));
    }
  | _ => None

and h_assert_to_sm_assert: UHExp.line => option(sm_assert) =
  fun
  | ExpLine(
      OpSeq(
        _,
        S(
          AssertLit(_, _number),
          A(
            Space,
            S(
              Parenthesized([
                ExpLine(OpSeq(_, S(h_e1, A(Equals, S(h_e2, E))))),
              ]),
              E,
            ),
          ),
        ),
      ),
    ) => {
      let* sm_e1 = hexp_operand_to_smexp(h_e1);
      let* sm_e2 = hexp_operand_to_smexp(h_e2);
      Some((sm_e1, sm_e2));
    }
  | _ => None

and hexp_opseq_to_smexp = (opseq: UHExp.opseq): option(Smyth.Lang.exp) => {
  let OpSeq(skel, seq) = opseq;
  switch (skel) {
  | Placeholder(n) => hexp_operand_to_smexp(Seq.nth_operand(n, seq))
  | BinOp(_, Comma, _, _) =>
    let* sm_exps =
      UHExp.get_tuple_elements(skel)
      |> List.map(sk => OpSeq.OpSeq(sk, seq))
      |> List.map(hexp_opseq_to_smexp)
      |> OptUtil.sequence;
    Some(ETuple(sm_exps));
  | BinOp(_, Cons, e1n, e2n) =>
    let* s_e1 = hexp_opseq_to_smexp(OpSeq.OpSeq(e1n, seq));
    let* s_e2 = hexp_opseq_to_smexp(OpSeq.OpSeq(e2n, seq));
    Some(sm_cons(s_e1, s_e2));
  | BinOp(_) => None
  };
}

and hexp_operand_to_smexp: UHExp.operand => option(Smyth.Lang.exp) =
  fun
  | AssertLit(_) // assertions are top-level only
  | FloatLit(_)
  | ApPalette(_)
  | Inj(_)
  | InvalidText(_) => None
  | Parenthesized(expr) => non_top_hexp_to_smexp(expr)
  | EmptyHole(num) => Some(EHole(num))
  | Var(_, _, name) => Some(EVar(name))
  | IntLit(_, str) => Some(str_to_smint(str))
  | ListNil(_) => Some(sm_nil)
  | BoolLit(_, true) => Some(sm_true)
  | BoolLit(_, false) => Some(sm_false)
  | Lam(_, h_pat, None, h_exp) => {
      let* (_, sm_pat) = hpat_opseq_to_smpat(h_pat);
      let* sm_exp = non_top_hexp_to_smexp(h_exp);
      Some(EFix(None, PatParam(sm_pat), sm_exp));
    }
  | Lam(_, h_pat, Some(h_ty), h_exp) => {
      let* sm_ty = h_ty |> UHTyp.expand |> htyp_to_styp;
      let* (_, sm_pat) = hpat_opseq_to_smpat(h_pat);
      let* sm_exp = non_top_hexp_to_smexp(h_exp);
      Some(ETypeAnnotation(EFix(None, PatParam(sm_pat), sm_exp), sm_ty));
    }
  | Case(_, scrut, rules) => {
      let* sm_scrut = non_top_hexp_to_smexp(scrut);
      let* handled_rules = List.map(handle_rule, rules) |> OptUtil.sequence;
      Some(
        ECase(
          sm_scrut,
          List.map((((s, p), e)) => (s, (p, e)), handled_rules),
        ),
      );
    }

and hpat_opseq_to_smpat: UHPat.opseq => option(sm_pat) =
  fun
  | OpSeq(skel, seq) =>
    switch (skel) {
    | Placeholder(n) => hpat_operand_to_smpat(Seq.nth_operand(n, seq))
    | BinOp(_, Comma, _, _) =>
      let* sm_ps =
        UHPat.get_tuple_elements(skel)
        |> List.map(sk => OpSeq.OpSeq(sk, seq))
        |> List.map(hpat_opseq_to_smpat)
        |> OptUtil.sequence
        // HACK: note how we're assuming contained ps aren't contructor patterns
        |> Option.map(List.map(((_, p)) => p));
      Some(("", PTuple(sm_ps))); // is this properly constructed?
    | BinOp(_, Cons, p1n, p2n) =>
      // HACK: see above
      let* (_, s_p1) = hpat_opseq_to_smpat(OpSeq.OpSeq(p1n, seq));
      let* (_, s_p2) = hpat_opseq_to_smpat(OpSeq.OpSeq(p2n, seq));
      Some(("Cons", PTuple([s_p1, s_p2])));
    | BinOp(_) => None
    }

and hpat_operand_to_smpat: UHPat.operand => option(sm_pat) =
  fun
  | EmptyHole(_)
  | FloatLit(_)
  | InvalidText(_)
  | Inj(_) => None
  | Parenthesized(h_p) => hpat_opseq_to_smpat(h_p)
  | Var(_, _, name) => Some(("", PVar(name)))
  | Wild(_) => Some(("", PWildcard)) // is this properly constructed?
  | BoolLit(_, true) => Some(("True", PTuple([]))) // is PTuple correct?
  | BoolLit(_, false) => Some(("False", PTuple([])))
  | ListNil(_) => Some(("Nil", PTuple([])))
  | IntLit(_, n) =>
    switch (n) {
    | "0" => Some(("Z", PTuple([])))
    | _ => None /* for now */
    }

and handle_rule: UHExp.rule => option(sm_rule) =
  fun
  /* Special case :
     Rewrite | N    => let M = N-1 in body ...
     Into    | S(M) =>  body ... */
  | Rule(
      OpSeq(_, S(Var(_, _, case_name), E)),
      [
        LetLine(
          OpSeq(_, S(Var(_, _, let_name), E)),
          _,
          [
            ExpLine(
              OpSeq(
                _,
                S(
                  Var(_, _, case_name_in_let),
                  A(Minus, S(IntLit(_, "1"), E)),
                ),
              ),
            ),
          ],
        ),
        let_body,
      ],
    )
      when case_name == case_name_in_let => {
      let* sm_body = non_top_hexp_to_smexp([let_body]);
      Some((("S", PTuple([PVar(let_name)])), sm_body));
    }
  | Rule(h_pat, h_exp) => {
      let* sm_pat = hpat_opseq_to_smpat(h_pat);
      let* sm_exp = non_top_hexp_to_smexp(h_exp);
      Some((sm_pat, sm_exp));
    };

/******************************************************************************/

let rec smpat_to_uhpat: Smyth.Lang.pat => option(UHPat.t) =
  fun
  | PVar(name) => Some(OpSeq.wrap(UHPat.var(name)))
  | PTuple(ps) => {
      let* ps = List.map(smpat_to_uhpat, ps) |> OptUtil.sequence;
      let seqs = List.map((OpSeq.OpSeq(_skel, seq)) => seq, ps);
      let* comma_seq =
        switch (seqs) {
        | [] => assert(false)
        | [seq0, ...seqs'] =>
          Some(
            List.fold_right(
              (seq, acc) => Seq.seq_op_seq(seq, Operators_Pat.Comma, acc),
              seqs',
              seq0,
            ),
          )
        };
      Some(UHPat.mk_OpSeq(comma_seq));
    }
  | PWildcard => Some(OpSeq.wrap(UHPat.wild()));

let rec smexp_to_uhexp = (e: Smyth.Lang.exp): option(UHExp.t) => {
  let+ line = smexp_to_uhexp_line(e);
  [line];
}

and smexp_to_uhexp_line = (e: Smyth.Lang.exp): option(UHExp.line) => {
  let+ h_e = smexp_to_uhexp_opseq(e);
  UHExp.ExpLine(h_e);
}

and smexp_to_uhexp_opseq = (e: Smyth.Lang.exp): option(UHExp.opseq) => {
  let+ h_e = smexp_to_uhexp_operand(e);
  OpSeq.wrap(h_e);
}

/* [@warning "-32"] */
/* [@warning "-27"] */
and smexp_to_uhexp_operand: Smyth.Lang.exp => option(UHExp.operand) =
  fun
  | EFix(Some(_name), _param, _body) =>
    /* turns into a let? */
    failwith(__LOC__)
  | EFix(None, PatParam(p), body) => {
      let* h_p = smpat_to_uhpat(p);
      let+ body_uhexp = smexp_to_uhexp(body);
      UHExp.lam(h_p, body_uhexp);
    }
  | EFix(None, TypeParam(_), _) => {
      failwith(__LOC__);
    }
  | EApp(_special, _head, _arg) => failwith(__LOC__)
  | EVar(name) => Some(UHExp.var(name))
  | ETuple(_args) => failwith(__LOC__)
  | EProj(_n, _i, _arg) => failwith(__LOC__)
  | ECtor(_name, _type_args, _arg) => failwith(__LOC__)
  | ECase(_scrutinee, _branches) => failwith(__LOC__)
  | EHole(_name) => failwith(__LOC__)
  | EAssert(_lhs, _rhs) => failwith(__LOC__)
  | ETypeAnnotation(_an_exp, _a_typ) => failwith(__LOC__);

/* [@warning "-32"] */
/* [@warning "-27"] */
/* let smres_to_operand: Smyth.Lang.res => option(UHExp.operand) = */
/*   fun */
/*   | RFix(env, name_opt, param, body) => failwith(__LOC__) */
/*   | RTuple(args) => failwith(__LOC__) */
/*   | RCtor(name, arg) => failwith(__LOC__) */
/*   | RHole(_env, name) => Some(EmptyHole(name)) */
/*   | RApp(head, arg) => failwith(__LOC__) */
/*   | RProj(n, i, arg) => failwith(__LOC__) */
/*   | RCase(env, scrutinee, branches) => failwith(__LOC__) */
/*   | RCtorInverse(name, arg) => failwith(__LOC__); */

/******************************************************************************/

type solve_result = list(list((MetaVar.t, UHExp.t)));

[@warning "-32"]
let solve = (e: UHExp.t): option(solve_result) => {
  let* sm_prog = top_hexp_to_smprog(e);
  switch (Smyth.Endpoint.solve_program(sm_prog)) {
  | Error(_) => None
  | Ok(_) => failwith("todo")
  };
};
