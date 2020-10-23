module Option = {
  /* Note that references to `Option` in this module refer to ocaml's option */
  module Let_syntax = {
    module Let_syntax = {
      /* let return = (o: 'a): option('a) => Some(o); */

      let map = (~f: 'a => 'b, o: option('a)): option('b) =>
        Option.map(f, o);

      let bind = (o: option('a), ~f: 'a => option('b)): option('b) =>
        Option.bind(o, f);
    };
  };
};

open OptUtil.Syntax;

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

 (
   (NatList
     (()
      ((Nil (TTuple ())) (Cons (TTuple ((TData Nat ()) (TData NatList ())))))))
  (Nat (() ((Z (TTuple ())) (S (TData Nat ())))))
  (Bool (() ((True (TTuple ())) (False (TTuple ())))))
 )

 */
[@warning "-32"]
let datatype_prelude: Smyth.Lang.datatype_ctx = [
  (
    "Bool",
    (
      [],
      [("True", Smyth.Lang.TTuple([])), ("False", Smyth.Lang.TTuple([]))],
    ),
  ),
  (
    "Nat",
    (
      [],
      [("Z", Smyth.Lang.TTuple([])), ("S", Smyth.Lang.TData("Nat", []))],
    ),
  ),
  (
    "NatList",
    (
      [],
      [
        ("Nil", Smyth.Lang.TTuple([])),
        (
          "Cons",
          Smyth.Lang.TTuple([
            Smyth.Lang.TData("Nat", []),
            Smyth.Lang.TData("NatList", []),
          ]),
        ),
      ],
    ),
  ),
];

[@warning "-32"]
let rec htyp_to_styp = (h_t: HTyp.t): option(Smyth.Lang.typ) =>
  Smyth.Lang.(
    switch (h_t) {
    | Hole
    | Float
    | Sum(_) => None
    | Int => Some(TData("Nat", []))
    | Bool => Some(TData("Bool", []))
    | List(Int) => Some(TData("NatList", []))
    | List(_t) => None
    | Prod(h_ts) =>
      let* s_ts = List.map(htyp_to_styp, h_ts) |> OptUtil.sequence;
      Some(TTuple(s_ts));
    | Arrow(h_t1, h_t2) =>
      let* s_t1 = htyp_to_styp(h_t1);
      let* s_t2 = htyp_to_styp(h_t2);
      Some(TArr(s_t1, s_t2));
    }
  );

[@warning "-32"]
let hpat_to_smpat = (_h_p: UHPat.t): option(Smyth.Lang.pat) => {
  failwith(__LOC__);
}
[@warning "-32"]
and pat_opseq_to_smpat = (_opseq: UHPat.opseq): option(Smyth.Lang.pat) => {
  failwith(__LOC__);
}
[@warning "-32"]
and pat_operand_to_smpat = (operand: UHPat.operand): option(Smyth.Lang.pat) =>
  // TODO: special case constructors
  switch (operand) {
  | EmptyHole(_) // TODO: does smyth support hole patterns?
  | FloatLit(_)
  | InvalidText(_)
  | Inj(_) => None
  | Wild(_) => Some(PWildcard)
  | Var(_, _, name) => Some(PVar(name))
  | IntLit(_, _string) => None // TODO: where are the smyth literal patterns??
  | BoolLit(_, _bool) => None // TODO: above
  | ListNil(_) => None // TODO: above
  | Parenthesized(_t) => None // TODO: ???
  };

[@warning "-32"]
let str_to_smint = (_str: string): Smyth.Lang.exp => {
  failwith(__LOC__);
};

[@warning "-32"]
let rec hexp_to_smexp = (_op: UHExp.t): option(Smyth.Lang.exp) => {
  failwith(__LOC__);
}
[@warning "-32"]
and opseq_to_smexp =
    (opseq: UHExp.opseq)
    : option((Smyth.Lang.exp, list((Smyth.Lang.exp, Smyth.Lang.exp)))) => {
  open OptUtil.Syntax;
  let OpSeq(skel, seq) = opseq;
  switch (skel) {
  | Placeholder(n) =>
    let+ sme = operand_to_smexp(Seq.nth_operand(n, seq));
    (sme, []);
  | BinOp(_) => failwith(__LOC__)
  };
}
[@warning "-32"]
[@warning "-8"]
and operand_to_smexp = (operand: UHExp.operand): option(Smyth.Lang.exp) =>
  switch (operand) {
  | FloatLit(_)
  | BoolLit(_)
  | ApPalette(_)
  | Inj(_, _, _)
  | InvalidText(_) => None
  | EmptyHole(number) => Some(EHole(number))
  | Var(_, _, name) => Some(EVar(name))
  | IntLit(_, str) => Some(str_to_smint(str))
  | ListNil(_) => Some(ECtor("Nil", [], ETuple([]))) // TODO: check last arg makes sense
  | Lam(_, _var /*Var(_, _, param_name)*/, _ty, _body) => failwith(__LOC__)
  // separate non/annotated cases?
  // need seperate letline case to capture non-anonymous function
  | Case(_, _scrut, _rules) => failwith(__LOC__)
  | Parenthesized(expr) => hexp_to_smexp(expr) // hmmm
  };

// d: disable error on inexhaustive match for now
[@warning "-8"]
let rec styp_to_htyp: Smyth.Lang.typ => option(HTyp.t) =
  fun
  | TArr(t1, t2) => {
      let%bind.Option t1' = styp_to_htyp(t1);
      let%map.Option t2' = styp_to_htyp(t2);
      HTyp.Arrow(t1', t2');
    };

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

type solve_result = list(list((MetaVar.t, UHExp.opseq)));

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
