open OptUtil.Syntax;
open Option;

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

[@warning "-32"]
let datatype_prelude: Smyth.Lang.datatype_ctx =
  Smyth.Lang.[
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

[@warning "-32"]
let rec htyp_to_styp: HTyp.t => option(Smyth.Lang.typ) =
  Smyth.Lang.(
    fun
    | Hole
    | Float
    | Sum(_) => None
    | Int => Some(TData("Nat", []))
    | Bool => Some(TData("Bool", []))
    | List(Int) => Some(TData("NatList", []))
    | List(_t) => None
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

open Smyth.Lang;

type sm_pat = (string, Smyth.Lang.pat);
type sm_rule = (sm_pat, Smyth.Lang.exp);

let sm_ctor = (name, arg) => ECtor(name, [], arg);
let sm_null_ctor = name => sm_ctor(name, ETuple([]));
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

[@warning "-32"]
let rec hexp_to_smexp = (_op: UHExp.t): option(Smyth.Lang.exp) => {
  // need this level for two? reasons:
  // capture top-level lets to turn into definitions
  // ??capture internal let-defined functions to turn into fixs??
  failwith(
    __LOC__,
  );
}

[@warning "-32"]
and hexp_to_smprog = (opseq: UHExp.opseq): option(Smyth.Desugar.program) => {
  // plan:
  // switch this from opseq to uhexp
  // this will be a block (list) of lines
  // filter these into three lists; letlines (definitions)
  // explines which begin with assertlits (assertions), and explines which dont
  // looks like definitions need to have a type, so we will require they are annotated
  // throw away all but the last expline, which will become main
  let* main = hexp_opseq_to_smexp(opseq);
  Smyth.Desugar.(
    Some({
      datatypes: datatype_prelude,
      definitions: [], // TODO: top-level defs: string * (typ * exp)) list
      assertions: [], // TODO: assertions: (exp * exp) list,
      main_opt: Some(main),
    })
  );
}

[@warning "-32"]
and hexp_opseq_to_smexp = (opseq: UHExp.opseq): option(Smyth.Lang.exp) => {
  let OpSeq(skel, seq) = opseq;
  switch (skel) {
  | Placeholder(n) => hexp_operand_to_smexp(Seq.nth_operand(n, seq))
  | BinOp(_, Comma, _, _) =>
    let* sm_es =
      UHExp.get_tuple_elements(skel)
      |> List.map(sk => OpSeq.OpSeq(sk, seq))
      |> List.map(hexp_opseq_to_smexp)
      |> OptUtil.sequence;
    Some(ETuple(sm_es));
  | BinOp(_, Cons, e1n, e2n) =>
    let* s_e1 = hexp_opseq_to_smexp(OpSeq.OpSeq(e1n, seq));
    let* s_e2 = hexp_opseq_to_smexp(OpSeq.OpSeq(e2n, seq));
    Some(ECtor("Cons", [], ETuple([s_e1, s_e2])));
  | BinOp(_) => None
  };
}

[@warning "-32"]
and hexp_operand_to_smexp: UHExp.operand => option(Smyth.Lang.exp) =
  fun
  | AssertLit(_) // assertions are top-level only for now
  | FloatLit(_)
  | ApPalette(_)
  | Inj(_)
  | InvalidText(_) => None
  | EmptyHole(num) => Some(EHole(num))
  | Var(_, _, name) => Some(EVar(name))
  | IntLit(_, str) => Some(str_to_smint(str))
  | ListNil(_) => Some(sm_nil)
  | BoolLit(_, true) => Some(sm_true)
  | BoolLit(_, false) => Some(sm_false)
  | Lam(_, pat, _ /* None */, body) => {
      // precondition: this lambda is not immediately named in a let
      let* (_, smp) = hpat_opseq_to_smpat(pat);
      let* smbody = hexp_to_smexp(body);
      Some(EFix(None, PatParam(smp), smbody));
    }
  // TODO: named fixes: rewrite into a let returning a lambda
  //| Lam(_, _var /*Var(_, _, param_name)*/, _ty, _body) => failwith(__LOC__)
  | Case(_, scrut, rules) => {
      let* sm_scrut = hexp_to_smexp(scrut);
      let* handled_rules = List.map(handle_rule, rules) |> OptUtil.sequence;
      Some(
        ECase(
          sm_scrut,
          List.map((((s, p), e)) => (s, (p, e)), handled_rules),
        ),
      );
    }
  | Parenthesized(expr) => hexp_to_smexp(expr) // does this make sense?

[@warning "-32"]
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
      Some(("", PTuple(sm_ps))); // check this is properly constructed
    | BinOp(_, Cons, p1n, p2n) =>
      // HACK: See above
      let* (_, s_p1) = hpat_opseq_to_smpat(OpSeq.OpSeq(p1n, seq));
      let* (_, s_p2) = hpat_opseq_to_smpat(OpSeq.OpSeq(p2n, seq));
      Some(("Cons", PTuple([s_p1, s_p2])));
    | BinOp(_) => None
    }

[@warning "-32"]
and hpat_operand_to_smpat: UHPat.operand => option(sm_pat) =
  fun
  | EmptyHole(_)
  | FloatLit(_)
  | InvalidText(_)
  | Inj(_) => None
  | Parenthesized(p) => hpat_opseq_to_smpat(p) // correct?
  | Wild(_) => Some(("", PWildcard)) // is "" correct?
  | Var(_, _, name) => Some(("", PVar(name)))
  | BoolLit(_, true) => Some(("True", PTuple([]))) // is PTuple correct?
  | BoolLit(_, false) => Some(("False", PTuple([])))
  | ListNil(_) => Some(("Nil", PTuple([])))
  | IntLit(_, n) =>
    switch (n) {
    | "0" => Some(("Z", PTuple([])))
    | _ => None /* for now */
    }

[@warning "-32"]
and handle_rule: UHExp.rule => option(sm_rule) =
  fun
  | Rule(pat, exp) => {
      let* smp = hpat_opseq_to_smpat(pat);
      // TODO: special case for when exp begins with let n_1 = n-1 in ...
      // i guess this will be the only supported let for now?
      // could rewrite lets into lambda generally
      // does the naive desugaring strategy work in the other direction?
      let* sme = hexp_to_smexp(exp);
      Some((smp, sme));
    };

/*
 [@warning "-8"]
 let rec styp_to_htyp: Smyth.Lang.typ => option(HTyp.t) =
   fun
   | TArr(t1, t2) => {
       let%bind.Option t1' = styp_to_htyp(t1);
       let%map.Option t2' = styp_to_htyp(t2);
       HTyp.Arrow(t1', t2');
     };
 */

[@warning "-32"]
[@warning "-27"]
let smexp_to_operand: Smyth.Lang.exp => option(UHExp.operand) =
  fun
  | EFix(name_opt, param, body) => failwith(__LOC__)
  | EApp(special, head, arg) => failwith(__LOC__)
  | EVar(name) => {
      let in_hole = /* NotInHole */ failwith(__LOC__);
      let in_var_hole = /* NotInVarHole */ failwith(__LOC__);
      Some(Var(in_hole, in_var_hole, name));
    }
  | ETuple(args) => failwith(__LOC__)
  | EProj(n, i, arg) => failwith(__LOC__)
  | ECtor(name, type_args, arg) => failwith(__LOC__)
  | ECase(scrutinee, branches) => failwith(__LOC__)
  | EHole(name) => Some(EmptyHole(name))
  | EAssert(lhs, rhs) => failwith(__LOC__)
  | ETypeAnnotation(an_exp, a_typ) => failwith(__LOC__);

[@warning "-32"]
[@warning "-27"]
let smres_to_operand: Smyth.Lang.res => option(UHExp.operand) =
  fun
  | RFix(env, name_opt, param, body) => failwith(__LOC__)
  | RTuple(args) => failwith(__LOC__)
  | RCtor(name, arg) => failwith(__LOC__)
  | RHole(_env, name) => Some(EmptyHole(name))
  | RApp(head, arg) => failwith(__LOC__)
  | RProj(n, i, arg) => failwith(__LOC__)
  | RCase(env, scrutinee, branches) => failwith(__LOC__)
  | RCtorInverse(name, arg) => failwith(__LOC__);

type solve_result = list(list((MetaVar.t, UHExp.t)));

[@warning "-32"]
let solve = (e: UHExp.t): option(solve_result) => {
  switch (
    Smyth.Endpoint.solve_program(
      Smyth.Desugar.{
        // TODO
        datatypes: [],
        definitions: [],
        assertions: [],
        main_opt: hexp_to_smexp(e),
      },
    )
  ) {
  | Error(_) => None
  | Ok(_) => failwith("todo")
  };
};
