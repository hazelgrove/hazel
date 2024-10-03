// module UTyp = {

//   /* Converts a syntactic type into a semantic type */
//   let rec to_typ: (Ctx.t, t) => Typ.t =
//     (ctx, utyp) =>
//       switch (utyp.term) {
//       | Invalid(_)
//       | MultiHole(_) => Unknown(Internal)
//       | EmptyHole => Unknown(TypeHole)
//       | Bool => Bool
//       | Int => Int
//       | Float => Float
//       | String => String
//       | Var(name) =>
//         switch (Ctx.lookup_tvar(ctx, name)) {
//         | Some(_) => Var(name)
//         | None => Unknown(Free(name))
//         }
//       | Arrow(u1, u2) => Arrow(to_typ(ctx, u1), to_typ(ctx, u2))
//       | Tuple(us) => Prod(List.map(to_typ(ctx), us))
//       | Sum(uts) => Sum(to_ctr_map(ctx, uts))
//       | List(u) => List(to_typ(ctx, u))
//       | Parens(u) => to_typ(ctx, u)
//       | Forall({term: Var(name), _} as utpat, tbody) =>
//         let ctx =
//           Ctx.extend_tvar(
//             ctx,
//             {name, id: UTPat.rep_id(utpat), kind: Abstract},
//           );
//         Forall(name, to_typ(ctx, tbody));
//       // Rec is same as Forall
//       | Rec({term: Var(name), _} as utpat, tbody) =>
//         let ctx =
//           Ctx.extend_tvar(
//             ctx,
//             {name, id: UTPat.rep_id(utpat), kind: Abstract},
//           );
//         Rec(name, to_typ(ctx, tbody));
//       | Forall({term: Invalid(_), _}, tbody)
//       | Forall({term: EmptyHole, _}, tbody)
//       | Forall({term: MultiHole(_), _}, tbody) =>
//         Forall("?", to_typ(ctx, tbody))
//       | Rec({term: Invalid(_), _}, tbody)
//       | Rec({term: EmptyHole, _}, tbody)
//       | Rec({term: MultiHole(_), _}, tbody) => Rec("?", to_typ(ctx, tbody))
//       /* The below cases should occur only inside sums */
//       | Constructor(_)
//       | Ap(_) => Unknown(Internal)
//       | Module(u) =>
//         let rep_id_p = ({ids, _}: TermBase.UPat.t) => {
//           assert(ids != []);
//           List.hd(ids);
//         };
//         let rep_id_t = ({ids, _}: TermBase.UTPat.t) => {
//           assert(ids != []);
//           List.hd(ids);
//         };
//         let rec upat_to_ctx =
//                 (
//                   (outer_ctx: Ctx.t, inner_ctx: Ctx.t, incomplete: bool),
//                   upat: TermBase.UPat.t,
//                 ) => {
//           switch (upat.term) {
//           | Invalid(_)
//           | EmptyHole
//           | MultiHole(_)
//           | Int(_)
//           | Float(_)
//           | Bool(_)
//           | String(_)
//           | Triv
//           | ListLit(_)
//           | Cons(_)
//           | Tuple(_)
//           | Wild
//           | Ap(_) => (outer_ctx, inner_ctx, incomplete)
//           // | Wild => (outer_ctx, inner_ctx, true) // disabled due to casting issues.
//           | TypeAnn(var, utyp1) =>
//             switch (var.term, utyp1.term) {
//             | (Var(name), _)
//             // All constructors appearing here should be Modules.
//             | (Constructor(name), _) => (
//                 outer_ctx,
//                 [
//                   VarEntry({
//                     name,
//                     id: rep_id_p(var),
//                     typ: to_typ(outer_ctx, utyp1),
//                   }),
//                   ...inner_ctx,
//                 ],
//                 incomplete,
//               )
//             | _ => (outer_ctx, inner_ctx, incomplete)
//             }
//           | Var(name) => (
//               outer_ctx,
//               [
//                 VarEntry({name, id: rep_id_p(upat), typ: Unknown(TypeHole)}),
//                 ...inner_ctx,
//               ],
//               incomplete,
//             )
//           | Constructor(name) => (
//               outer_ctx,
//               [
//                 ConstructorEntry({
//                   name,
//                   id: rep_id_p(upat),
//                   typ: Unknown(TypeHole),
//                 }),
//                 ...inner_ctx,
//               ],
//               incomplete,
//             )
//           | TyAlias(typat, utyp) =>
//             switch (typat.term) {
//             | Var(name)
//                 when
//                   !Form.is_base_typ(name)
//                   && Ctx.lookup_alias(inner_ctx, name) == None =>
//               /* NOTE(andrew):  See TyAlias in Statics.uexp_to_info_map  */
//               let (ty_def, ctx_body, new_inner) = {
//                 let ty_pre =
//                   to_typ(Ctx.extend_dummy_tvar(outer_ctx, name), utyp);
//                 switch (utyp.term) {
//                 | Sum(_) when List.mem(name, Typ.free_vars(ty_pre)) =>
//                   let ty_rec =
//                     Typ.Rec("α", Typ.subst(Var("α"), name, ty_pre));
//                   (
//                     ty_rec,
//                     Ctx.extend_alias(
//                       outer_ctx,
//                       name,
//                       rep_id_t(typat),
//                       ty_rec,
//                     ),
//                     Ctx.extend_alias(
//                       inner_ctx,
//                       name,
//                       rep_id_t(typat),
//                       ty_rec,
//                     ),
//                   );
//                 | _ =>
//                   let ty = to_typ(outer_ctx, utyp);
//                   (
//                     ty,
//                     Ctx.extend_alias(outer_ctx, name, rep_id_t(typat), ty),
//                     Ctx.extend_alias(inner_ctx, name, rep_id_t(typat), ty),
//                   );
//                 };
//               };
//               switch (Typ.get_sum_constructors(outer_ctx, ty_def)) {
//               | Some(sm) => (
//                   Ctx.add_ctrs(ctx_body, name, rep_id(utyp), sm),
//                   Ctx.add_ctrs(new_inner, name, rep_id(utyp), sm),
//                   incomplete,
//                 )
//               | None => (ctx_body, new_inner, incomplete)
//               };

//             | _ => (outer_ctx, inner_ctx, incomplete)
//             }
//           | Parens(p) => upat_to_ctx((outer_ctx, inner_ctx, incomplete), p)
//           };
//         };
//         let rec get_Tuple: TermBase.UPat.t => Typ.t = (
//           ut =>
//             switch (ut.term) {
//             | Tuple(us) =>
//               let (_, inner_ctx, incomplete) =
//                 List.fold_left(upat_to_ctx, (ctx, [], false), us);
//               Module({inner_ctx, incomplete});
//             | Parens(p) => get_Tuple(p)
//             | TypeAnn(_)
//             | Var(_)
//             | Constructor(_)
//             | TyAlias(_)
//             | Invalid(_)
//             | EmptyHole
//             | MultiHole(_)
//             | Wild
//             | Int(_)
//             | Float(_)
//             | Bool(_)
//             | String(_)
//             | Triv
//             | ListLit(_)
//             | Cons(_)
//             | Ap(_) =>
//               let (_, inner_ctx, incomplete) =
//                 upat_to_ctx((ctx, [], false), ut);
//               Module({inner_ctx, incomplete});
//             }
//         );
//         get_Tuple(u);
//       | Dot(typ1, typ2) =>
//         /** Currently, the only possible way to introduce modules are through
//       a variable in Constructor form.

//       Maybe better to put to_typ in Statics? */
//         open Util.OptUtil.Syntax;
//         let rec inner_normalize = (ctx: Ctx.t, ty: Typ.t): option(Typ.t) =>
//           switch (ty) {
//           | Var(x) =>
//             let* ty = Ctx.lookup_alias(ctx, x);
//             inner_normalize(ctx, ty);
//           | _ => Some(ty)
//           };
//         let res = {
//           let* name = Module.get_tyname(typ2);
//           let+ (tag_name, inner_ctx) = Module.get_module("", ctx, typ1);
//           let ty = {
//             let* inner_ctx = inner_ctx;
//             inner_normalize(inner_ctx, to_typ(inner_ctx, typ2));
//           };
//           (tag_name ++ name, ty);
//         };
//         switch (res) {
//         | Some((name, Some(ty))) => Member(name, ty)
//         | Some((name, None)) => Member(name, Unknown(Internal))
//         | None => Member("?", Unknown(Internal))
//         };
//       }
//   and to_variant:
//     (Ctx.t, variant) => option(ConstructorMap.binding(option(Typ.t))) =
//     ctx =>
//       fun
//       | Variant(ctr, _, u) => Some((ctr, Option.map(to_typ(ctx), u)))
//       | BadEntry(_) => None
//   and to_ctr_map = (ctx: Ctx.t, uts: list(variant)): Typ.sum_map => {
//     List.fold_left(
//       (acc, ut) =>
//         List.find_opt(((ctr, _)) => ctr == fst(ut), acc) == None
//           ? acc @ [ut] : acc,
//       [],
//       List.filter_map(to_variant(ctx), uts),
//     );
//   };
// };

module Pat = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | EmptyHole
    | MultiHole
    | Wild
    | Int
    | Float
    | Bool
    | String
    | ListLit
    | Constructor
    | Cons
    | Var
    | ModuleVar
    | Tuple
    | Parens
    | Ap
    | Cast;

  include TermBase.Pat;

  let rep_id = ({ids, _}: t) => {
    assert(ids != []);
    List.hd(ids);
  };

  let term_of: t => TermBase.Pat.term = IdTagged.term_of;

  let unwrap: t => (term, term => t) = IdTagged.unwrap;

  let fresh: term => t = IdTagged.fresh;

  let hole = (tms: list(TermBase.Any.t)) =>
    switch (tms) {
    | [] => EmptyHole
    | [_, ..._] => MultiHole(tms)
    };

  let cls_of_term: term => cls =
    fun
    | Invalid(_) => Invalid
    | EmptyHole => EmptyHole
    | MultiHole(_) => MultiHole
    | Wild => Wild
    | Int(_) => Int
    | Float(_) => Float
    | Bool(_) => Bool
    | String(_) => String
    | ListLit(_) => ListLit
    | Constructor(_) => Constructor
    | Cons(_) => Cons
    | Var(_) => Var
    | Tuple(_) => Tuple
    | Parens(_) => Parens
    | Ap(_) => Ap
    | TyAlias(_) => TyAlias
    | Cast(_) => Cast;

  let show_cls: cls => string =
    fun
    | Invalid => "Invalid pattern"
    | MultiHole => "Broken pattern"
    | EmptyHole => "Empty pattern hole"
    | Wild => "Wildcard"
    | Int => "Integer literal"
    | Float => "Float literal"
    | Bool => "Boolean literal"
    | String => "String literal"
    | ListLit => "List literal"
    | Constructor => "Constructor"
    | Cons => "Cons"
    | Var => "Variable binding"
    | ModuleVar => "Module variable binding"
    | Tuple => "Tuple"
    | Parens => "Parenthesized pattern"
    | Ap => "Constructor application"
    | TyAlias => "Type alias definition pattern"
    | Cast => "Annotation";

  let rec is_var = (pat: t) => {
    switch (pat.term) {
    | Parens(pat)
    | Cast(pat, _, _) => is_var(pat)
    | Var(_) => true
    | TyAlias(_)
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Wild
    | Int(_)
    | Float(_)
    | Bool(_)
    | String(_)
    | ListLit(_)
    | Cons(_, _)
    | Tuple(_)
    | Constructor(_)
    | Ap(_) => false
    };
  };

  let rec is_fun_var = (pat: t) => {
    switch (pat.term) {
    | Parens(pat) => is_fun_var(pat)
    | Cast(pat, typ, _) =>
      is_var(pat) && (UTyp.is_arrow(typ) || Typ.is_forall(typ))
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Wild
    | Int(_)
    | Float(_)
    | Bool(_)
    | String(_)
    | ListLit(_)
    | Cons(_, _)
    | Var(_)
    | Tuple(_)
    | Constructor(_)
    | TyAlias(_)
    | Ap(_) => false
    };
  };

  let rec is_tuple_of_arrows = (pat: t) =>
    is_fun_var(pat)
    || (
      switch (pat.term) {
      | Parens(pat) => is_tuple_of_arrows(pat)
      | Tuple(pats) => pats |> List.for_all(is_fun_var)
      | Invalid(_)
      | EmptyHole
      | MultiHole(_)
      | Wild
      | Int(_)
      | Float(_)
      | Bool(_)
      | String(_)
      | ListLit(_)
      | Cons(_, _)
      | Var(_)
      | Cast(_)
      | Constructor(_)
      | TyAlias(_)
      | Ap(_) => false
      }
    );

  let rec is_tuple_of_vars = (pat: t) =>
    is_var(pat)
    || (
      switch (pat.term) {
      | Parens(pat)
      | Cast(pat, _, _) => is_tuple_of_vars(pat)
      | Tuple(pats) => pats |> List.for_all(is_var)
      | Invalid(_)
      | EmptyHole
      | MultiHole(_)
      | Wild
      | Int(_)
      | Float(_)
      | Bool(_)
      | String(_)
      | ListLit(_)
      | Cons(_, _)
      | Var(_)
      | TyAlias(_)
      | Constructor(_)
      | Ap(_) => false
      }
    );

  let rec get_var = (pat: t) => {
    switch (pat.term) {
    | Parens(pat) => get_var(pat)
    | Var(x) => Some(x)
    | TyAlias(_)
    | Cast(x, _, _) => get_var(x)
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Wild
    | Int(_)
    | Float(_)
    | Bool(_)
    | String(_)
    | ListLit(_)
    | Cons(_, _)
    | Tuple(_)
    | Constructor(_)
    | Ap(_) => None
    };
  };

  let rec get_fun_var = (pat: t) => {
    switch (pat.term) {
    | Parens(pat) => get_fun_var(pat)
    | Cast(pat, t1, _) =>
      if (Typ.is_arrow(t1) || UTyp.is_forall(t1)) {
        get_var(pat) |> Option.map(var => var);
      } else {
        None;
      }
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Wild
    | Int(_)
    | Float(_)
    | Bool(_)
    | String(_)
    | ListLit(_)
    | Cons(_, _)
    | Var(_)
    | Tuple(_)
    | Constructor(_)
    | TyAlias(_)
    | Ap(_) => None
    };
  };

  let rec get_num_of_vars = (pat: t) =>
    if (is_var(pat)) {
      Some(1);
    } else {
      switch (pat.term) {
      | Parens(pat)
      | TypeAnn(pat, _) => get_num_of_vars(pat)
      | Tuple(pats) =>
        is_tuple_of_vars(pat) ? Some(List.length(pats)) : None
      | Invalid(_)
      | EmptyHole
      | MultiHole(_)
      | Wild
      | Int(_)
      | Float(_)
      | Bool(_)
      | String(_)
      | Triv
      | ListLit(_)
      | Cons(_, _)
      | Var(_)
      | Constructor(_)
      | TyAlias(_)
      | Ap(_) => None
      };
    };

  let rec get_bindings = (pat: t) =>
    switch (get_var(pat)) {
    | Some(x) => Some([x])
    | None =>
      switch (pat.term) {
      | Parens(pat)
      | Cast(pat, _, _) => get_bindings(pat)
      | Tuple(pats) =>
        let vars = pats |> List.map(get_var);
        if (List.exists(Option.is_none, vars)) {
          None;
        } else {
          Some(List.map(Option.get, vars));
        };
      | Invalid(_)
      | EmptyHole
      | MultiHole(_)
      | Wild
      | Int(_)
      | Float(_)
      | Bool(_)
      | String(_)
      | ListLit(_)
      | Cons(_, _)
      | Var(_)
      | TyAlias(_)
      | Constructor(_)
      | Ap(_) => None
      }
    };

  let rec get_num_of_vars = (pat: t) =>
    if (is_var(pat)) {
      Some(1);
    } else {
      switch (pat.term) {
      | Parens(pat)
      | Cast(pat, _, _) => get_num_of_vars(pat)
      | Tuple(pats) =>
        is_tuple_of_vars(pat) ? Some(List.length(pats)) : None
      | Invalid(_)
      | EmptyHole
      | MultiHole(_)
      | Wild
      | Int(_)
      | Float(_)
      | Bool(_)
      | String(_)
      | ListLit(_)
      | Cons(_, _)
      | Var(_)
      | Constructor(_)
      | Ap(_) => None
      };
    };

  let ctr_name = (p: t): option(Constructor.t) =>
    switch (p.term) {
    | Constructor(name, _) => Some(name)
    | _ => None
    };
};

module Exp = {
  include TermBase.Exp;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | EmptyHole
    | MultiHole
    | StaticErrorHole
    | DynamicErrorHole
    | FailedCast
    | Deferral
    | Undefined
    | Bool
    | Int
    | Float
    | String
    | ListLit
    | Constructor
    | Fun
    | TypFun
    | Tuple
    | Var
    | MetaVar
    | Let
    | Module
    | ModuleVar
    | Dot
    | FixF
    | TyAlias
    | Ap
    | TypAp
    | DeferredAp
    | Pipeline
    | If
    | Seq
    | Test
    | Filter
    | Closure
    | Parens
    | Cons
    | UnOp(Operators.op_un)
    | BinOp(Operators.op_bin)
    | BuiltinFun
    | Match
    | Cast
    | ListConcat;

  let hole = (tms: list(TermBase.Any.t)): term =>
    switch (tms) {
    | [] => EmptyHole
    | [_, ..._] => MultiHole(tms)
    };

  let rep_id: t => Id.t = IdTagged.rep_id;
  let fresh: term => t = IdTagged.fresh;
  let unwrap: t => (term, term => t) = IdTagged.unwrap;

  let cls_of_term: term => cls =
    fun
    | Invalid(_) => Invalid
    | EmptyHole => EmptyHole
    | MultiHole(_) => MultiHole
    | DynamicErrorHole(_) => DynamicErrorHole
    | FailedCast(_) => FailedCast
    | Deferral(_) => Deferral
    | Undefined => Undefined
    | Bool(_) => Bool
    | Int(_) => Int
    | Float(_) => Float
    | String(_) => String
    | ListLit(_) => ListLit
    | Constructor(_) => Constructor
    | Fun(_) => Fun
    | TypFun(_) => TypFun
    | Tuple(_) => Tuple
    | Var(_) => Var
    | Let(_) => Let
    | Module(_) => Module
    | Dot(_) => Dot
    | FixF(_) => FixF
    | TyAlias(_) => TyAlias
    | Ap(_) => Ap
    | TypAp(_) => TypAp
    | DeferredAp(_) => DeferredAp
    | If(_) => If
    | Seq(_) => Seq
    | Test(_) => Test
    | Filter(_) => Filter
    | Closure(_) => Closure
    | Parens(_) => Parens
    | Cons(_) => Cons
    | ListConcat(_) => ListConcat
    | UnOp(op, _) => UnOp(op)
    | BinOp(op, _, _) => BinOp(op)
    | BuiltinFun(_) => BuiltinFun
    | Match(_) => Match
    | Cast(_) => Cast;

  let show_cls: cls => string =
    fun
    | Invalid => "Invalid expression"
    | MultiHole => "Broken expression"
    | EmptyHole => "Empty expression hole"
    | StaticErrorHole => "Static error hole"
    | DynamicErrorHole => "Dynamic error hole"
    | FailedCast => "Failed cast"
    | Deferral => "Deferral"
    | Undefined => "Undefined expression"
    | Bool => "Boolean literal"
    | Int => "Integer literal"
    | Float => "Float literal"
    | String => "String literal"
    | ListLit => "List literal"
    | Constructor => "Constructor"
    | Fun => "Function literal"
    | TypFun => "Type Function Literal"
    | Tuple => "Tuple literal"
    | Var => "Variable reference"
    | MetaVar => "Meta variable reference"
    | Let => "Let expression"
    | Module => "Module expression"
    | ModuleVar => "Module path"
    | Dot => "Dot access"
    | FixF => "Fixpoint operator"
    | TyAlias => "Type Alias definition"
    | Ap => "Application"
    | TypAp => "Type application"
    | DeferredAp => "Partial Application"
    | Pipeline => "Pipeline expression"
    | If => "If expression"
    | Seq => "Sequence expression"
    | Test => "Test"
    | Filter => "Filter"
    | Closure => "Closure"
    | Parens => "Parenthesized expression"
    | Cons => "Cons"
    | ListConcat => "List Concatenation"
    | BinOp(op) => Operators.show_binop(op)
    | UnOp(op) => Operators.show_unop(op)
    | BuiltinFun => "Built-in Function"
    | Match => "Case expression"
    | Cast => "Cast expression";

  // Typfun should be treated as a function here as this is only used to
  // determine when to allow for recursive definitions in a let binding.
  let rec is_fun = (e: t) => {
    switch (e.term) {
    | Parens(e) => is_fun(e)
    | Cast(e, _, _) => is_fun(e)
    | TypFun(_)
    | Fun(_)
    | BuiltinFun(_) => true
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | DynamicErrorHole(_)
    | FailedCast(_)
    | Deferral(_)
    | Undefined
    | Bool(_)
    | Int(_)
    | Float(_)
    | String(_)
    | ListLit(_)
    | Tuple(_)
    | Var(_)
    | Let(_)
    | Module(_)
    | Dot(_)
    | FixF(_)
    | TyAlias(_)
    | Ap(_)
    | TypAp(_)
    | DeferredAp(_)
    | If(_)
    | Seq(_)
    | Test(_)
    | Filter(_)
    | Cons(_)
    | ListConcat(_)
    | Closure(_)
    | UnOp(_)
    | BinOp(_)
    | Match(_)
    | Constructor(_) => false
    };
  };

  let rec is_tuple_of_functions = (e: t) =>
    is_fun(e)
    || (
      switch (e.term) {
      | Cast(e, _, _)
      | Parens(e) => is_tuple_of_functions(e)
      | Tuple(es) => es |> List.for_all(is_fun)
      | Invalid(_)
      | EmptyHole
      | MultiHole(_)
      | DynamicErrorHole(_)
      | FailedCast(_)
      | Deferral(_)
      | Undefined
      | Bool(_)
      | Int(_)
      | Float(_)
      | String(_)
      | ListLit(_)
      | Fun(_)
      | TypFun(_)
      | Closure(_)
      | BuiltinFun(_)
      | Var(_)
      | Let(_)
      | Module(_)
      | Dot(_)
      | FixF(_)
      | TyAlias(_)
      | Ap(_)
      | TypAp(_)
      | DeferredAp(_)
      | If(_)
      | Seq(_)
      | Test(_)
      | Filter(_)
      | Cons(_)
      | ListConcat(_)
      | UnOp(_)
      | BinOp(_)
      | Match(_)
      | Constructor(_) => false
      }
    );

  let ctr_name = (e: t): option(Constructor.t) =>
    switch (e.term) {
    | Constructor(name, _) => Some(name)
    | _ => None
    };

  let is_deferral = (e: t) => {
    switch (e.term) {
    | Deferral(_) => true
    | _ => false
    };
  };

  let rec get_num_of_functions = (e: t) =>
    if (is_fun(e)) {
      Some(1);
    } else {
      switch (e.term) {
      | Parens(e) => get_num_of_functions(e)
      | Tuple(es) => is_tuple_of_functions(e) ? Some(List.length(es)) : None
      | Invalid(_)
      | EmptyHole
      | MultiHole(_)
      | DynamicErrorHole(_)
      | FailedCast(_)
      | FixF(_)
      | Closure(_)
      | BuiltinFun(_)
      | Cast(_)
      | Deferral(_)
      | Undefined
      | Bool(_)
      | Int(_)
      | Float(_)
      | String(_)
      | ListLit(_)
      | Fun(_)
      | TypFun(_)
      | Var(_)
      | Let(_)
      | Module(_)
      | Dot(_)
      | Filter(_)
      | TyAlias(_)
      | Ap(_)
      | TypAp(_)
      | DeferredAp(_)
      | If(_)
      | Seq(_)
      | Test(_)
      | Cons(_)
      | ListConcat(_)
      | UnOp(_)
      | BinOp(_)
      | Match(_)
      | Constructor(_) => None
      };
    };
};

module Rul = {
  include TermBase.Rul;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Rule;

  // example of awkwardness induced by having forms like rules
  // that may have a different-sorted child with no delimiters
  // (eg scrut with no rules)
  let ids = (~any_ids, {ids, term, _}: t) =>
    switch (ids) {
    | [_, ..._] => ids
    | [] =>
      switch (term) {
      | Hole([tm, ..._]) => any_ids(tm)
      | Rules(scrut, []) => scrut.ids
      | _ => []
      }
    };

  let rep_id = (~any_ids, tm) =>
    switch (ids(~any_ids, tm)) {
    | [] => raise(Invalid_argument("UExp.rep_id"))
    | [id, ..._] => id
    };
};

module Any = {
  include TermBase.Any;

  let is_exp: t => option(TermBase.Exp.t) =
    fun
    | Exp(e) => Some(e)
    | _ => None;
  let is_pat: t => option(TermBase.Pat.t) =
    fun
    | Pat(p) => Some(p)
    | _ => None;
  let is_typ: t => option(TermBase.Typ.t) =
    fun
    | Typ(t) => Some(t)
    | _ => None;

  let rec ids =
    fun
    | Exp(tm) => tm.ids
    | Pat(tm) => tm.ids
    | Typ(tm) => tm.ids
    | TPat(tm) => tm.ids
    | Rul(tm) => Rul.ids(~any_ids=ids, tm)
    | Nul ()
    | Any () => [];

  // Terms may consist of multiple tiles, eg the commas in an n-tuple,
  // the rules of a case expression + the surrounding case-end tile,
  // the list brackets tile coupled with the elem-separating commas.
  // The _representative id_ is the canonical tile id used to identify
  // and look up info about a term.
  //
  // In instances like case expressions and list literals, where a parent
  // tile surrounds the other tiles, the representative id is the parent tile's.
  // In other instances like n-tuples, where the commas are all siblings,
  // the representative id is one of the comma ids, unspecified which one.
  // (This would change for n-tuples if we decided parentheses are necessary.)
  let rep_id =
    fun
    | Exp(tm) => Exp.rep_id(tm)
    | Pat(tm) => Pat.rep_id(tm)
    | Typ(tm) => Typ.rep_id(tm)
    | TPat(tm) => TPat.rep_id(tm)
    | Rul(tm) => Rul.rep_id(~any_ids=ids, tm)
    | Nul ()
    | Any () => raise(Invalid_argument("Term.rep_id"));
};
