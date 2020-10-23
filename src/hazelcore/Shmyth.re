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

let datatype_prelude: Smyth.Lang.datatype_ctx = [
  ("Bool", (["true", "false"], [])),
  ("Nat", (["Z"], [("S", Smyth.Lang.TData("Nat", []))])),
  // TODO: type parameter?
  ("List", (["nil"], [("cons", Smyth.Lang.TData("List", []))])),
];

[@warning "-32"]
let rec htyp_to_styp = (h_ty: HTyp.t): option(Smyth.Lang.typ) => {
  switch (h_ty) {
  | Hole
  | Float
  | Sum(_, _) => None
  | Arrow(h_t1, h_t2) =>
    let s_t1 = htyp_to_styp(h_t1);
    let s_t2 = htyp_to_styp(h_t2);
    Some(TArr(s_t1, s_t2));
  //| Prod(ts) => OptUtil.sequence(htyp_to_styp) |> Option.Map(TTuple)
  //| Int => Some() // gotta be int type we declare in prelude
  //| Bool => Some() // likewise
  //| List(t) => Some() // likewise
  };
};

[@warning "-32"]
let hexp_to_smexp = (_uhexp: UHExp.t): option(Smyth.Lang.exp) => {
  failwith(__LOC__);
};

[@warning "-32"]
let opseq_to_smexp = (_opseq: UHExp.opseq): option(Smyth.Lang.exp) => {
  failwith(__LOC__);
};

[@warning "-32"]
let str_to_smint = (_str: string): Smyth.Lang.exp => {
  failwith(__LOC__);
};

[@warning "-32"]
[@warning "-8"]
let operand_to_smexp = (operand: UHExp.operand): option(Smyth.Lang.exp) =>
  switch (operand) {
  | FloatLit(_)
  | BoolLit(_)
  | ApPalette(_)
  | InvalidText(_) => None
  | EmptyHole(number) => Some(EHole(number))
  | Var(_, _, name) => Some(EVar(name))
  | IntLit(_, str) => Some(str_to_smint(str))
  | ListNil(_) => failwith(__LOC__)
  | Lam(_, _var /*Var(_, _, param_name)*/, _ty, _body) => failwith(__LOC__)
  // separate non/annotated cases?
  // need seperate letline case to capture non-anonymous function
  | Inj(_, _side, _v) => failwith(__LOC__)
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
