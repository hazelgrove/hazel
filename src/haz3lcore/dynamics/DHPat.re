open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | EmptyHole(MetaVar.t, MetaVarInst.t) // () or _HOLE
  | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, MetaVarInst.t, t) //(_HOLE x)
  | Wild //_
  | ExpandingKeyword(MetaVar.t, MetaVarInst.t, ExpandingKeyword.t)
  | InvalidText(MetaVar.t, MetaVarInst.t, string)
  | BadConstructor(MetaVar.t, MetaVarInst.t, string) //_BAD x
  | Var(Var.t) // x
  | IntLit(int) // 1
  | FloatLit(float) //1.0
  | BoolLit(bool) // false
  | StringLit(string) //"hello"
  | ListLit(Typ.t, list(t)) //[a, b, c]
  | Cons(t, t) //a :: b
  | Tuple(list(t)) //(a, b)
  | Constructor(string) //X
  | Ap(t, t); //a(1)

let mk_tuple: list(t) => t =
  fun
  | []
  | [_] => failwith("mk_tuple: expected at least 2 elements")
  | dps => Tuple(dps);

/**
 * Whether dp contains the variable x outside of a hole.
 */
let rec binds_var = (x: Var.t, dp: t): bool =>
  switch (dp) {
  | EmptyHole(_, _)
  | NonEmptyHole(_, _, _, _)
  | Wild
  | InvalidText(_)
  | BadConstructor(_)
  | IntLit(_)
  | FloatLit(_)
  | BoolLit(_)
  | StringLit(_)
  | Constructor(_)
  | ExpandingKeyword(_, _, _) => false
  | Var(y) => Var.eq(x, y)
  | Tuple(dps) => dps |> List.exists(binds_var(x))
  | Cons(dp1, dp2) => binds_var(x, dp1) || binds_var(x, dp2)
  | ListLit(_, d_list) =>
    let new_list = List.map(binds_var(x), d_list);
    List.fold_left((||), false, new_list);
  | Ap(_, _) => false
  };

let rec of_menhir_ast = (pat: Hazel_menhir.AST.pat, getId: bool => Uuidm.t): t => {
  let of_menhir_ast_noid = of_menhir_ast(_, getId);
  // let getId_all_args = getId;
  let getId_no_inc = () => getId(false);
  let getId = () => getId(true);
  switch (pat) {
  | IntPat(i) => IntLit(i)
  | FloatPat(f) => FloatLit(f)
  | VarPat(x) => Var(x)
  | BadConstructorPat(x) => BadConstructor(getId_no_inc(), 0, x)
  | ConstructorPat(x) => Constructor(x)
  | StringPat(s) => StringLit(s)
  | TypeAnn(pat, _typ) => of_menhir_ast_noid(pat)
  | TuplePat(pats) => Tuple(List.map(of_menhir_ast_noid, pats))
  | ApPat(pat1, pat2) =>
    Ap(of_menhir_ast_noid(pat1), of_menhir_ast_noid(pat2))
  | ConsPat(p1, p2) => Cons(of_menhir_ast_noid(p1), of_menhir_ast_noid(p2))
  | BoolPat(b) => BoolLit(b)
  | EmptyHolePat => EmptyHole(getId(), 0)
  | NonEmptyHolePat(p) =>
    let id = getId();
    let p = of_menhir_ast_noid(p);
    NonEmptyHole(ErrStatus.HoleReason.TypeInconsistent, id, 0, p);
  | WildPat => Wild
  | ListPat(l, t) =>
    ListLit(Typ.of_menhir_ast(t), List.map(of_menhir_ast_noid, l))
  };
};

let rec bound_vars = (dp: t): list(Var.t) =>
  switch (dp) {
  | EmptyHole(_, _)
  | NonEmptyHole(_, _, _, _)
  | Wild
  | InvalidText(_)
  | BadConstructor(_)
  | IntLit(_)
  | FloatLit(_)
  | BoolLit(_)
  | StringLit(_)
  | Constructor(_)
  | ExpandingKeyword(_, _, _) => []
  | Var(y) => [y]
  | Tuple(dps) => List.flatten(List.map(bound_vars, dps))
  | Cons(dp1, dp2) => bound_vars(dp1) @ bound_vars(dp2)
  | ListLit(_, dps) => List.flatten(List.map(bound_vars, dps))
  | Ap(_, dp1) => bound_vars(dp1)
  };
