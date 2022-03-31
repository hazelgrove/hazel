open Sexplib.Std;

type direction =
  | L
  | R;

type raef_provenance = // this can still be called hole_provenance
  | Matched_arrow(direction)
  | Matched_sum(direction)
  | Matched_prod(int)
  | Matched_list;

type andrew_provenance = // this could be called unknown_hole
  | TypeHole
  | SynPatternVar // let expr with no type annot
  | Internal      // all of our provenance prob all here

type hole_provenance = // this can still be called hole_provenance
  | Matched_arrow(direction)
  | Matched_sum(direction)
  | Matched_prod(int)
  | Matched_list;
  // | SynPatternVar;

type is_syn_pattern_var =
| IsSynPatternVar
| NotSynPatternVar;

// let (x, y) = expr ?? Since (x, y) is a tuple pattern, would Matched_prod be called on it?
// let x : ? = .. (TypeHole)
// let x = case expr    (IsSynPatternVar)

type hole_id = MetaVar.t;
type internal_hole_id = (MetaVar.t, list(hole_provenance));

type unknown_type =
  | TypeHole(hole_id)
  | Internal(internal_hole_id, is_syn_pattern_var);

/*
from UHPat.re
and operand =
  | EmptyHole(MetaVar.t)
  | Wild(ErrStatus.t)
  | TypeAnn(ErrStatus.t, operand, UHTyp.t)
  | InvalidText(MetaVar.t, string)
  | Var(ErrStatus.t, VarErrStatus.t, Var.t)
*/

/*
let mk_syn_text =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, caret_index: int, text: string)
    : ActionOutcome.t(syn_success) => {
  let text_cursor = CursorPosition.OnText(caret_index);
  switch (TextShape.of_text(text)) {
  | InvalidTextShape(t) =>
    if (text |> StringUtil.is_empty) {
      let (zhole, u_gen) = u_gen |> ZPat.new_EmptyHole;
      Succeeded((ZOpSeq.wrap(zhole), HTyp.Unknown(Internal), ctx, u_gen));
    } else {
      let (it, u_gen) = UHPat.new_InvalidText(u_gen, t);
      let zp = ZOpSeq.wrap(ZPat.CursorP(text_cursor, it));
      Succeeded((zp, HTyp.Unknown(Internal), ctx, u_gen));
    }
  | Underscore =>
    let zp = ZOpSeq.wrap(ZPat.CursorP(OnDelim(0, After), UHPat.wild()));
    Succeeded((zp, HTyp.Unknown(Internal), ctx, u_gen));
  | IntLit(n) =>
    let zp = ZOpSeq.wrap(ZPat.CursorP(text_cursor, UHPat.intlit(n)));
    Succeeded((zp, HTyp.Int, ctx, u_gen));
  | FloatLit(f) =>
    let zp = ZOpSeq.wrap(ZPat.CursorP(text_cursor, UHPat.floatlit(f)));
    Succeeded((zp, HTyp.Float, ctx, u_gen));
  | BoolLit(b) =>
    let zp = ZOpSeq.wrap(ZPat.CursorP(text_cursor, UHPat.boollit(b)));
    Succeeded((zp, HTyp.Bool, ctx, u_gen));
  | ExpandingKeyword(k) =>
    let (u, u_gen) = u_gen |> MetaVarGen.next;
    let var =
      UHPat.var(
        ~var_err=InVarHole(Keyword(k), u),
        k |> ExpandingKeyword.to_string,
      );
    let zp = ZOpSeq.wrap(ZPat.CursorP(text_cursor, var));
    Succeeded((zp, HTyp.Unknown(Internal), ctx, u_gen));
  | Var(x) =>
        let ctx = Contexts.extend_gamma(ctx, (x, Unknown(SynPatternVar)));   TODO: ASK PROF OMAR ABOUT THIS. Why extend the context at all for this var? (Even before we were using Hole Provenances)
    let zp = ZOpSeq.wrap(ZPat.CursorP(text_cursor, UHPat.var(x)));
    Succeeded((zp, HTyp.Unknown(SynPatternVar), ctx, u_gen));
  };
};
*/

type base_hole_provenance = 
  | UserGenerated   // akin to Andrew's TypeHole provenance
  | SynPatternVar;

// TODO: ASK ANDREW AGAIN ABOUT INTERNAL and its purpose/role
  
type derived_hole_provenance = // this can still be called hole_provenance // by nature, these are Internal
  | Matched_arrow(direction)
  | Matched_sum(direction)
  | Matched_prod(int)
  | Matched_list;

type base_hole_id = (MetaVar.t, base_hole_provenance);
type derived_hole_id = (MetaVar.t, list(derived_hole_provenance))

type unknown_type_2 = 
  | Base(base_hole_id)
  | Derived(internal_hole_id);

// holes that exist with no ancestor
// holes that have an ancestor

/* types with holes */
[@deriving sexp]
type t =
  | Unknown(unknown_type) // UHHole [1] -> HHole [1.1.1.1.2.1], HHole [1.1.1.1.2.2]
  // UHHole [1] used in (HHole [1.1] , HHole [1.2]) ; 
  // UHHole [1] used in (HHole [1.3] , HHole [1.4]) ;
  // HHole [1.3] used in (HHole [1.3.1], HHole [1.3.2])
  // ^ Hole(1, [3], *0->2*) used in (Hole(1, [3, 1], 0), Hole(1, [3, 2], 0))
  // HHole [1.3] used in (HHole [1.3.3], HHole [1.3.4])
  // ^ Hole(1, [3], *2->4*) used in (Hole(1, [3, 3], 0), Hole(1, [3, 4], 0))
  | Int
  | Float
  | Bool
  | Arrow(t, t)
  | Sum(t, t)
  | Prod(list(t))
  | List(t);

[@deriving sexp]
type join =
  | GLB
  | LUB;

type inf_constraint = (t, t);

let precedence_Prod = Operators_Typ.precedence(Prod);
let precedence_Arrow = Operators_Typ.precedence(Arrow);
let precedence_Sum = Operators_Typ.precedence(Sum);
let precedence_const = Operators_Typ.precedence_const;
let precedence = (ty: t): int =>
  switch (ty) {
  | Int
  | Float
  | Bool
  | Hole(_)
  | Prod([])
  | List(_) => precedence_const
  | Prod(_) => precedence_Prod
  | Sum(_, _) => precedence_Sum
  | Arrow(_, _) => precedence_Arrow
  };

/* equality
   At the moment, this coincides with default equality,
   but this will change when polymorphic types are implemented */
let eq = (==);

/* type consistency */
let rec consistent = (x, y) =>
  switch (x, y) {
  | (Hole(_), _)
  | (_, Hole(_)) => true
  | (Int, Int) => true
  | (Int, _) => false
  | (Float, Float) => true
  | (Float, _) => false
  | (Bool, Bool) => true
  | (Bool, _) => false
  | (Arrow(ty1, ty2), Arrow(ty1', ty2'))
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    consistent(ty1, ty1') && consistent(ty2, ty2')
  | (Arrow(_, _), _) => false
  | (Sum(_, _), _) => false
  | (Prod(tys1), Prod(tys2)) =>
    ListUtil.for_all2_opt(consistent, tys1, tys2)
    |> Option.value(~default=false)
  | (Prod(_), _) => false
  | (List(ty), List(ty')) => consistent(ty, ty')
  | (List(_), _) => false
  };

let inconsistent = (ty1, ty2) => !consistent(ty1, ty2);

let rec consistent_all = (types: list(t)): bool =>
  switch (types) {
  | [] => true
  | [hd, ...tl] =>
    if (List.exists(inconsistent(hd), tl)) {
      false;
    } else {
      consistent_all(tl);
    }
  };

let get_prod_elements: t => list(t) =
  fun
  | Prod(tys) => tys
  | _ as ty => [ty];

let get_prod_arity = ty => ty |> get_prod_elements |> List.length;

let matched_arrow: t => (option(t, t)) =
  (typ: t) =>
  let pair, _ = matched_arrow_inf(typ);
  pair;

/**
 * let (x: ?0) be EHole(1) in
 * let y be x 2 in
 * let z be x True in
 * z
 */

/**
 * ?0 = ?1
 * ?0 = Arrow(?0.matched_left, ?0.matched_right)
 * ?0.matched_left = Num
 * ?0 = Arrow(?0.matched_left, ?0.matched_right)
 * ?0.matched_left = Bool
 */

//?0   unsolved   Num->?0.matched_right Bool->?0.matched_right
//?0.matched_right   unsolved   Num//Bool

let matched_arrow_inf: t => (option(t, t), list(inf_constraint)) =
  (typ: t) =>
  switch (typ) {
    | Hole(base, provenances) =>
      let hole_left = Hole(base, provenances @ (Matched_arrow(L)));
      let hole_right = Hole(base, provenances @ (Matched_arrow(R)));
      let pair = (hole_left, hole_right);
      let constraints = [(typ, Arrow(hole_left, hole_right))];
      (Some(pair), constraints);
    | Arrow(ty1, ty2) => (Some((ty1, ty2)), [])
    | _ => (None, [])
  };

let matched_sum: t => (option(t, t)) =
  (typ: t) =>
  let pair, _ = matched_sum_inf(typ);
  pair;

let matched_sum_inf: t => (option(t, t), list(inf_constraint)) =
  (typ: t) =>
  switch (typ) {
    | Hole(base, provenances) =>
      let hole_left = Hole(base, provenances @ (Matched_sum(L)));
      let hole_right = Hole(base, provenances @ (Matched_sum(R)));
      let pair = (hole_left, hole_right);
      let constraints = [(typ, Sum(hole_left, hole_right))];
      (Some(pair), constraints);
    | Sum(ty1, ty2) => (Some((ty1, ty2)), [])
    | _ => (None, [])
  };

let matched_list: t => (option(t, t)) =
  (typ: t) =>
  let pair, _ = matched_list_inf(typ);
  pair;

let matched_list_inf: t => (option(t), list(inf_constraint)) =
  (typ: t) =>
  switch (typ) {
    | Hole(base, provenances) =>
      let hole_elt = Hole(base, provenances @ Matched_list);
      let constraint = [(typ, List(hole_elt))];
      (Some(hole_elt), constraint);
    | List(ty_ls) => (Some(ty_ls), [])
    | _ => (None, [])
  };

let rec load_type_variable = (typ: t) => {
  switch (typ) {
  | Hole(id) =>
    InfVar.type_variable := InfVar.recent(id + 1, InfVar.type_variable^)
  | Bool
  | Int
  | Float => ()
  | Arrow(ty1, ty2)
  | Sum(ty1, ty2) =>
    load_type_variable(ty1);
    load_type_variable(ty2);
  | Prod(tys) => List.iter(load_type_variable, tys)
  | List(ty) => load_type_variable(typ)
  };
};

/* complete (i.e. does not have any holes) */
let rec complete =
  fun
  | Hole(_) => false
  | Int => true
  | Float => true
  | Bool => true
  | Arrow(ty1, ty2)
  | Sum(ty1, ty2) => complete(ty1) && complete(ty2)
  | Prod(tys) => tys |> List.for_all(complete)
  | List(ty) => complete(ty);

let rec join = (j, ty1, ty2) =>
  switch (ty1, ty2) {
  | (_, Hole(_) as hole) =>
    switch (j) {
    | GLB => Some(hole)
    | LUB => Some(ty1)
    }
  | (Hole(_) as hole, _) =>
    switch (j) {
    | GLB => Some(hole)
    | LUB => Some(ty2)
    }
  | (Int, Int) => Some(ty1)
  | (Int, _) => None
  | (Float, Float) => Some(ty1)
  | (Float, _) => None
  | (Bool, Bool) => Some(ty1)
  | (Bool, _) => None
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    switch (join(j, ty1, ty1'), join(j, ty2, ty2')) {
    | (Some(ty1), Some(ty2)) => Some(Arrow(ty1, ty2))
    | _ => None
    }
  | (Arrow(_), _) => None
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    switch (join(j, ty1, ty1'), join(j, ty2, ty2')) {
    | (Some(ty1), Some(ty2)) => Some(Sum(ty1, ty2))
    | _ => None
    }
  | (Sum(_), _) => None
  | (Prod(tys1), Prod(tys2)) =>
    ListUtil.map2_opt(join(j), tys1, tys2)
    |> Option.map(OptUtil.sequence)
    |> Option.join
    |> Option.map(joined_types => Prod(joined_types))
  | (Prod(_), _) => None
  | (List(ty), List(ty')) =>
    switch (join(j, ty, ty')) {
    | Some(ty) => Some(List(ty))
    | None => None
    }
  | (List(_), _) => None
  };

let join_all = (j: join, types: list(t)): option(t) => {
  switch (types) {
  | [] => None
  | [hd] => Some(hd)
  | [hd, ...tl] =>
    if (!consistent_all(types)) {
      None;
    } else {
      List.fold_left(
        (common_opt, ty) =>
          switch (common_opt) {
          | None => None
          | Some(common_ty) => join(j, common_ty, ty)
          },
        Some(hd),
        tl,
      );
    }
  };
};
