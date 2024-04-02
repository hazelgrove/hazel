open Util;

module TPat = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | EmptyHole
    | MultiHole
    | Var;

  include TermBase.TPat;

  let rep_id = ({ids, _}: t) => {
    assert(ids != []);
    List.hd(ids);
  };

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
    | Var(_) => Var;

  let show_cls: cls => string =
    fun
    | Invalid => "Invalid type alias"
    | MultiHole => "Broken type alias"
    | EmptyHole => "Empty type alias hole"
    | Var => "Type alias";
};

module TypTerm = {
  include TermBase.TypTerm;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | EmptyHole
    | MultiHole
    | Int
    | Float
    | Bool
    | String
    | Arrow
    | Prod
    | Sum
    | List
    | Var
    | Constructor
    | Parens
    | Ap
    | Rec;

  include TermBase.TypTerm;

  let rep_id = ({ids, _}: t) => {
    assert(ids != []);
    List.hd(ids);
  };

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
    | Int => Int
    | Float => Float
    | Bool => Bool
    | String => String
    | List(_) => List
    | Arrow(_) => Arrow
    | Var(_) => Var
    | Prod(_) => Prod
    | Parens(_) => Parens
    | Ap(_) => Ap
    | Sum(_) => Sum
    | Rec(_) => Rec;

  let show_cls: cls => string =
    fun
    | Invalid => "Invalid type"
    | MultiHole => "Broken type"
    | EmptyHole => "Empty type hole"
    | Int
    | Float
    | String
    | Bool => "Base type"
    | Var => "Type variable"
    | Constructor => "Sum constructor"
    | List => "List type"
    | Arrow => "Function type"
    | Prod => "Product type"
    | Sum => "Sum type"
    | Parens => "Parenthesized type"
    | Ap => "Constructor application"
    | Rec => "Recursive Type";

  let rec is_arrow = (typ: t) => {
    switch (typ.term) {
    | Parens(typ) => is_arrow(typ)
    | Arrow(_) => true
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Int
    | Float
    | Bool
    | String
    | List(_)
    | Prod(_)
    | Var(_)
    | Ap(_)
    | Sum(_)
    | Rec(_) => false
    };
  };

  /* Converts a syntactic type into a semantic type */
  let rec to_typ: (Ctx.t, t) => Typ.t =
    (ctx, utyp) =>
      switch (utyp.term) {
      | Invalid(_)
      | MultiHole(_) => Unknown(Internal) |> Typ.fresh
      | EmptyHole => Unknown(Hole(EmptyHole)) |> Typ.fresh
      | Bool => Bool |> Typ.fresh
      | Int => Int |> Typ.fresh
      | Float => Float |> Typ.fresh
      | String => String |> Typ.fresh
      | Var(name) =>
        switch (Ctx.lookup_tvar(ctx, name)) {
        | Some(_) => Var(name) |> Typ.fresh
        | None => Unknown(Hole(Invalid(name))) |> Typ.fresh
        }
      | Arrow(u1, u2) =>
        Arrow(to_typ(ctx, u1), to_typ(ctx, u2)) |> Typ.fresh
      | Prod(us) => Prod(List.map(to_typ(ctx), us)) |> Typ.fresh
      | Sum(uts) => Sum(to_ctr_map(ctx, uts)) |> Typ.fresh
      | List(u) => List(to_typ(ctx, u)) |> Typ.fresh
      | Parens(u) => to_typ(ctx, u)
      /* The below cases should occur only inside sums */
      | Ap(_) => Unknown(Internal) |> Typ.fresh
      | Rec({term: Invalid(_), _}, tbody)
      | Rec({term: EmptyHole, _}, tbody)
      | Rec({term: MultiHole(_), _}, tbody) =>
        Rec("?", to_typ(ctx, tbody)) |> Typ.fresh
      | Rec({term: Var(name), _} as utpat, tbody) =>
        let ctx =
          Ctx.extend_tvar(
            ctx,
            {name, id: TPat.rep_id(utpat), kind: Abstract},
          );
        Rec(name, to_typ(ctx, tbody)) |> Typ.fresh;
      }
  and to_variant:
    (Ctx.t, ConstructorMap.variant(t)) => ConstructorMap.variant(Typ.t) =
    ctx =>
      fun
      | Variant(ctr, ids, u) =>
        ConstructorMap.Variant(ctr, ids, Option.map(to_typ(ctx), u))
      | BadEntry(u) => ConstructorMap.BadEntry(to_typ(ctx, u))
  and to_ctr_map =
      (ctx: Ctx.t, uts: list(ConstructorMap.variant(t))): Typ.sum_map => {
    uts
    |> List.map(to_variant(ctx))
    |> ListUtil.dedup_f(
         (
           x: ConstructorMap.variant(Typ.t),
           y: ConstructorMap.variant(Typ.t),
         ) =>
         switch (x, y) {
         | (Variant(c1, _, _), Variant(c2, _, _)) => c1 == c2
         | (Variant(_), BadEntry(_))
         | (BadEntry(_), Variant(_))
         | (BadEntry(_), BadEntry(_)) => false
         }
       );
  };
};

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
    | Tuple
    | Parens
    | Ap
    | TypeAnn;

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
    | TypeAnn(_) => TypeAnn;

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
    | Tuple => "Tuple"
    | Parens => "Parenthesized pattern"
    | Ap => "Constructor application"
    | TypeAnn => "Annotation";

  let rec is_var = (pat: t) => {
    switch (pat.term) {
    | Parens(pat) => is_var(pat)
    | Var(_) => true
    | TypeAnn(_)
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
    | TypeAnn(pat, typ) => is_var(pat) && TypTerm.is_arrow(typ)
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
      | TypeAnn(_)
      | Constructor(_)
      | Ap(_) => false
      }
    );

  let rec get_var = (pat: t) => {
    switch (pat.term) {
    | Parens(pat) => get_var(pat)
    | Var(x) => Some(x)
    | TypeAnn(x, _) => get_var(x)
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
    | TypeAnn(pat, typ) =>
      if (TypTerm.is_arrow(typ)) {
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
    | Ap(_) => None
    };
  };

  let rec get_recursive_bindings = (pat: t) => {
    switch (get_fun_var(pat)) {
    | Some(x) => Some([x])
    | None =>
      switch (pat.term) {
      | Parens(pat) => get_recursive_bindings(pat)
      | Tuple(pats) =>
        let fun_vars = pats |> List.map(get_fun_var);
        if (List.exists(Option.is_none, fun_vars)) {
          None;
        } else {
          Some(List.map(Option.get, fun_vars));
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
      | TypeAnn(_)
      | Constructor(_)
      | Ap(_) => None
      }
    };
  };

  let ctr_name = (p: t): option(Constructor.t) =>
    switch (p.term) {
    | Constructor(name) => Some(name)
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
    | Bool
    | Int
    | Float
    | String
    | ListLit
    | Constructor
    | Fun
    | Tuple
    | Var
    | MetaVar
    | Let
    | FixF
    | TyAlias
    | Ap
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
    | StaticErrorHole(_) => StaticErrorHole
    | DynamicErrorHole(_) => DynamicErrorHole
    | FailedCast(_) => FailedCast
    | Bool(_) => Bool
    | Int(_) => Int
    | Float(_) => Float
    | String(_) => String
    | ListLit(_) => ListLit
    | Constructor(_) => Constructor
    | Fun(_) => Fun
    | Tuple(_) => Tuple
    | Var(_) => Var
    | Let(_) => Let
    | FixF(_) => FixF
    | TyAlias(_) => TyAlias
    | Ap(_) => Ap
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
    | Bool => "Boolean literal"
    | Int => "Integer literal"
    | Float => "Float literal"
    | String => "String literal"
    | ListLit => "List literal"
    | Constructor => "Constructor"
    | Fun => "Function literal"
    | Tuple => "Tuple literal"
    | Var => "Variable reference"
    | MetaVar => "Meta variable reference"
    | Let => "Let expression"
    | FixF => "Fixpoint operator"
    | TyAlias => "Type Alias definition"
    | Ap => "Application"
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

  let rec is_fun = (e: t) => {
    switch (e.term) {
    | Parens(e) => is_fun(e)
    | Cast(e, _, _) => is_fun(e)
    | Fun(_)
    | BuiltinFun(_) => true
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | StaticErrorHole(_)
    | DynamicErrorHole(_)
    | FailedCast(_)
    | Bool(_)
    | Int(_)
    | Float(_)
    | String(_)
    | ListLit(_)
    | Tuple(_)
    | Var(_)
    | Let(_)
    | FixF(_)
    | TyAlias(_)
    | Ap(_)
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
      | StaticErrorHole(_)
      | DynamicErrorHole(_)
      | FailedCast(_)
      | Bool(_)
      | Int(_)
      | Float(_)
      | String(_)
      | ListLit(_)
      | Fun(_)
      | Closure(_)
      | BuiltinFun(_)
      | Var(_)
      | Let(_)
      | FixF(_)
      | TyAlias(_)
      | Ap(_)
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
    | Constructor(name) => Some(name)
    | _ => None
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
  let is_typ: t => option(TermBase.TypTerm.t) =
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
    | Typ(tm) => TypTerm.rep_id(tm)
    | TPat(tm) => TPat.rep_id(tm)
    | Rul(tm) => Rul.rep_id(~any_ids=ids, tm)
    | Nul ()
    | Any () => raise(Invalid_argument("Term.rep_id"));
};
