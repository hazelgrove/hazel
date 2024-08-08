/* TERM

   These data structures define the term structures on which
   the static and dynamic semantics of the language are based.
   Each sort has a corresponding U<Sort> module.

   The contained cls type lists the terms of that sort, and
   should be in 1-1 correspondence with the term type which
   is used to build composite terms.

   This is wrapped in a record type to associate a unique id
   with each term. These unique ids are the same as from the
   tile structure from the syntax module, as there is a 1-1
   correspondence between terms and tiles.

   TODO: add tests to check if there are forms and/or terms
   without correponding syntax classes */

include TermBase.Any;
type any = t;

module UTPat = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | EmptyHole
    | MultiHole
    | Var;

  include TermBase.UTPat;

  let rep_id = ({ids, _}) => {
    assert(ids != []);
    List.hd(ids);
  };

  let hole = (tms: list(any)) =>
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
    | Invalid => "Invalid type binding name"
    | MultiHole => "Broken type binding"
    | EmptyHole => "Empty type binding hole"
    | Var => "Type binding";

  let tyvar_of_utpat = ({ids: _, term}) =>
    switch (term) {
    | Var(x) => Some(x)
    | _ => None
    };
};

module UTyp = {
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
    | TupLabel
    | Tuple
    | Sum
    | List
    | Var
    | Constructor
    | Parens
    | Ap
    | Forall
    | Rec;

  include TermBase.UTyp;

  let rep_id = ({ids, _}: t) => {
    assert(ids != []);
    List.hd(ids);
  };

  let hole = (tms: list(any)) =>
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
    | Constructor(_) => Constructor
    | TupLabel(_) => TupLabel
    | Tuple(_) => Tuple
    | Parens(_) => Parens
    | Ap(_) => Ap
    | Sum(_) => Sum
    | Forall(_) => Forall
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
    | TupLabel => "Labeled Tuple Item type"
    | Tuple => "Product type"
    | Sum => "Sum type"
    | Parens => "Parenthesized type"
    | Ap => "Constructor application"
    | Forall => "Forall Type"
    | Rec => "Recursive Type";

  let rec is_arrow = (typ: t) => {
    switch (typ.term) {
    | Parens(typ)
    | TupLabel(_, typ) => is_arrow(typ)
    | Arrow(_) => true
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Int
    | Float
    | Bool
    | String
    | List(_)
    | Tuple(_)
    | Var(_)
    | Constructor(_)
    | Ap(_)
    | Sum(_)
    | Forall(_)
    | Rec(_) => false
    };
  };

  let rec is_forall = (typ: t) => {
    switch (typ.term) {
    | Parens(typ)
    | TupLabel(_, typ) => is_forall(typ)
    | Forall(_) => true
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Int
    | Float
    | Bool
    | String
    | Arrow(_)
    | List(_)
    | Tuple(_)
    | Var(_)
    | Constructor(_)
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
      | MultiHole(_) => Unknown(Internal)
      | EmptyHole => Unknown(TypeHole)
      | Bool => Bool
      | Int => Int
      | Float => Float
      | String => String
      | Var(name) =>
        switch (Ctx.lookup_tvar(ctx, name)) {
        | Some(_) => Var(name)
        | None => Unknown(Free(name))
        }
      | Arrow(u1, u2) => Arrow(to_typ(ctx, u1), to_typ(ctx, u2))
      | TupLabel(s, ut) => Label(s, to_typ(ctx, ut))
      | Tuple(us) => Prod(List.map(to_typ(ctx), us))
      | Sum(uts) => Sum(to_ctr_map(ctx, uts))
      | List(u) => List(to_typ(ctx, u))
      | Parens(u) => to_typ(ctx, u)
      | Forall({term: Var(name), _} as utpat, tbody) =>
        let ctx =
          Ctx.extend_tvar(
            ctx,
            {name, id: UTPat.rep_id(utpat), kind: Abstract},
          );
        Forall(name, to_typ(ctx, tbody));
      // Rec is same as Forall
      | Rec({term: Var(name), _} as utpat, tbody) =>
        let ctx =
          Ctx.extend_tvar(
            ctx,
            {name, id: UTPat.rep_id(utpat), kind: Abstract},
          );
        Rec(name, to_typ(ctx, tbody));
      | Forall({term: Invalid(_), _}, tbody)
      | Forall({term: EmptyHole, _}, tbody)
      | Forall({term: MultiHole(_), _}, tbody) =>
        Forall("?", to_typ(ctx, tbody))
      | Rec({term: Invalid(_), _}, tbody)
      | Rec({term: EmptyHole, _}, tbody)
      | Rec({term: MultiHole(_), _}, tbody) => Rec("?", to_typ(ctx, tbody))
      /* The below cases should occur only inside sums */
      | Constructor(_)
      | Ap(_) => Unknown(Internal)
      }
  and to_variant:
    (Ctx.t, variant) => option(ConstructorMap.binding(option(Typ.t))) =
    ctx =>
      fun
      | Variant(ctr, _, u) => Some((ctr, Option.map(to_typ(ctx), u)))
      | BadEntry(_) => None
  and to_ctr_map = (ctx: Ctx.t, uts: list(variant)): Typ.sum_map => {
    List.fold_left(
      (acc, ut) =>
        List.find_opt(((ctr, _)) => ctr == fst(ut), acc) == None
          ? acc @ [ut] : acc,
      [],
      List.filter_map(to_variant(ctx), uts),
    );
  };
};

module UPat = {
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
    | Triv
    | ListLit
    | Constructor
    | Cons
    | Var
    | TupLabel
    | Tuple
    | Parens
    | Ap
    | TypeAnn;

  include TermBase.UPat;

  let rep_id = ({ids, _}: t) => {
    assert(ids != []);
    List.hd(ids);
  };

  let hole = (tms: list(any)) =>
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
    | Triv => Triv
    | ListLit(_) => ListLit
    | Constructor(_) => Constructor
    | Cons(_) => Cons
    | Var(_) => Var
    | TupLabel(_) => TupLabel
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
    | Triv => "Trivial literal"
    | ListLit => "List literal"
    | Constructor => "Constructor"
    | Cons => "Cons"
    | Var => "Variable binding"
    | TupLabel => "Labeled Tuple Item pattern"
    | Tuple => "Tuple"
    | Parens => "Parenthesized pattern"
    | Ap => "Constructor application"
    | TypeAnn => "Annotation";

  let rec is_var = (pat: t) => {
    switch (pat.term) {
    | Parens(pat)
    | TupLabel(_, pat)
    | TypeAnn(pat, _) => is_var(pat)
    | Var(_) => true
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
    | Tuple(_)
    | Constructor(_)
    | Ap(_) => false
    };
  };

  let rec is_fun_var = (pat: t) => {
    switch (pat.term) {
    | Parens(pat) => is_fun_var(pat)
    | TupLabel(_, pat) => is_fun_var(pat)
    | TypeAnn(pat, typ) =>
      is_var(pat) && (UTyp.is_arrow(typ) || UTyp.is_forall(typ))
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
      | TupLabel(_, pat) => is_tuple_of_arrows(pat)
      | Tuple(pats) => pats |> List.for_all(is_fun_var)
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
      | TypeAnn(_)
      | Constructor(_)
      | Ap(_) => false
      }
    );

  let rec is_tuple_of_vars = (pat: t) =>
    is_var(pat)
    || (
      switch (pat.term) {
      | Parens(pat)
      | TupLabel(_, pat)
      | TypeAnn(pat, _) => is_tuple_of_vars(pat)
      | Tuple(pats) => pats |> List.for_all(is_var)
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
      | Ap(_) => false
      }
    );

  let rec get_var = (pat: t) => {
    switch (pat.term) {
    | Parens(pat)
    | TupLabel(_, pat)
    | TypeAnn(pat, _) => get_var(pat)
    | Var(x) => Some(x)
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
    | Tuple(_)
    | Constructor(_)
    | Ap(_) => None
    };
  };

  let rec get_fun_var = (pat: t) => {
    switch (pat.term) {
    | Parens(pat) => get_fun_var(pat)
    | TupLabel(_, pat) => get_fun_var(pat)
    | TypeAnn(pat, typ) =>
      if (UTyp.is_arrow(typ) || UTyp.is_forall(typ)) {
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
    | Triv
    | ListLit(_)
    | Cons(_, _)
    | Var(_)
    | Tuple(_)
    | Constructor(_)
    | Ap(_) => None
    };
  };

  let rec get_num_of_vars = (pat: t) =>
    if (is_var(pat)) {
      Some(1);
    } else {
      switch (pat.term) {
      | Parens(pat)
      | TupLabel(_, pat)
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
      | Ap(_) => None
      };
    };

  let rec get_bindings = (pat: t) =>
    switch (get_var(pat)) {
    | Some(x) => Some([x])
    | None =>
      switch (pat.term) {
      | Parens(pat)
      | TupLabel(_, pat)
      | TypeAnn(pat, _) => get_bindings(pat)
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
      | Triv
      | ListLit(_)
      | Cons(_, _)
      | Var(_)
      | Constructor(_)
      | Ap(_) => None
      }
    };

  let ctr_name = (p: t): option(Constructor.t) =>
    switch (p.term) {
    | Constructor(name) => Some(name)
    | _ => None
    };

  let get_label: t => option((LabeledTuple.t, t)) =
    p =>
      switch (p.term) {
      | TupLabel(s, p') => Some((s, p'))
      | _ => None
      };
};

module UExp = {
  include TermBase.UExp;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Invalid
    | EmptyHole
    | MultiHole
    | Triv
    | Deferral
    | Undefined
    | Bool
    | Int
    | Float
    | String
    | ListLit
    | Constructor
    | Fun
    | TupLabel
    | TypFun
    | Tuple
    | Dot
    | Var
    | MetaVar
    | Let
    | TyAlias
    | Ap
    | TypAp
    | DeferredAp
    | Pipeline
    | If
    | Seq
    | Test
    | Filter
    | Parens
    | Cons
    | UnOp(op_un)
    | BinOp(op_bin)
    | Match
    | ListConcat;

  let hole = (tms: list(any)): term =>
    switch (tms) {
    | [] => EmptyHole
    | [_, ..._] => MultiHole(tms)
    };

  let rep_id = ({ids, _}) => {
    assert(ids != []);
    List.hd(ids);
  };

  let cls_of_term: term => cls =
    fun
    | Invalid(_) => Invalid
    | EmptyHole => EmptyHole
    | MultiHole(_) => MultiHole
    | Triv => Triv
    | Deferral(_) => Deferral
    | Undefined => Undefined
    | Bool(_) => Bool
    | Int(_) => Int
    | Float(_) => Float
    | String(_) => String
    | ListLit(_) => ListLit
    | Constructor(_) => Constructor
    | Fun(_) => Fun
    | TupLabel(_, _) => TupLabel
    | TypFun(_) => TypFun
    | Tuple(_) => Tuple
    | Dot(_) => Dot
    | Var(_) => Var
    | Let(_) => Let
    | TyAlias(_) => TyAlias
    | Ap(_) => Ap
    | TypAp(_) => TypAp
    | DeferredAp(_) => DeferredAp
    | Pipeline(_) => Pipeline
    | If(_) => If
    | Seq(_) => Seq
    | Test(_) => Test
    | Filter(_) => Filter
    | Parens(_) => Parens
    | Cons(_) => Cons
    | ListConcat(_) => ListConcat
    | UnOp(op, _) => UnOp(op)
    | BinOp(op, _, _) => BinOp(op)
    | Match(_) => Match;

  let show_op_un_meta: op_un_meta => string =
    fun
    | Unquote => "Un-quotation";

  let show_op_un_bool: op_un_bool => string =
    fun
    | Not => "Boolean Negation";

  let show_op_un_int: op_un_int => string =
    fun
    | Minus => "Integer Negation";

  let show_unop: op_un => string =
    fun
    | Meta(op) => show_op_un_meta(op)
    | Bool(op) => show_op_un_bool(op)
    | Int(op) => show_op_un_int(op);

  let show_op_bin_bool: op_bin_bool => string =
    fun
    | And => "Boolean Conjunction"
    | Or => "Boolean Disjunction";

  let show_op_bin_int: op_bin_int => string =
    fun
    | Plus => "Integer Addition"
    | Minus => "Integer Subtraction"
    | Times => "Integer Multiplication"
    | Power => "Integer Exponentiation"
    | Divide => "Integer Division"
    | LessThan => "Integer Less Than"
    | LessThanOrEqual => "Integer Less Than or Equal"
    | GreaterThan => "Integer Greater Than"
    | GreaterThanOrEqual => "Integer Greater Than or Equal"
    | Equals => "Integer Equality"
    | NotEquals => "Integer Inequality";

  let show_op_bin_float: op_bin_float => string =
    fun
    | Plus => "Float Addition"
    | Minus => "Float Subtraction"
    | Times => "Float Multiplication"
    | Power => "Float Exponentiation"
    | Divide => "Float Division"
    | LessThan => "Float Less Than"
    | LessThanOrEqual => "Float Less Than or Equal"
    | GreaterThan => "Float Greater Than"
    | GreaterThanOrEqual => "Float Greater Than or Equal"
    | Equals => "Float Equality"
    | NotEquals => "Float Inequality";

  let show_op_bin_string: op_bin_string => string =
    fun
    | Concat => "String Concatenation"
    | Equals => "String Equality";

  let show_binop: op_bin => string =
    fun
    | Int(op) => show_op_bin_int(op)
    | Float(op) => show_op_bin_float(op)
    | Bool(op) => show_op_bin_bool(op)
    | String(op) => show_op_bin_string(op);

  let show_cls: cls => string =
    fun
    | Invalid => "Invalid expression"
    | MultiHole => "Broken expression"
    | EmptyHole => "Empty expression hole"
    | Triv => "Trivial literal"
    | Deferral => "Deferral"
    | Undefined => "Undefined expression"
    | Bool => "Boolean literal"
    | Int => "Integer literal"
    | Float => "Float literal"
    | String => "String literal"
    | ListLit => "List literal"
    | Constructor => "Constructor"
    | Fun => "Function literal"
    | TupLabel => "Labeled Tuple Item literal"
    | TypFun => "Type Function Literal"
    | Tuple => "Tuple literal"
    | Dot => "Dot operator"
    | Var => "Variable reference"
    | MetaVar => "Meta variable reference"
    | Let => "Let expression"
    | TyAlias => "Type Alias definition"
    | Ap => "Application"
    | TypAp => "Type application"
    | DeferredAp => "Partial Application"
    | Pipeline => "Pipeline expression"
    | If => "If expression"
    | Seq => "Sequence expression"
    | Test => "Test"
    | Filter => "Filter"
    | Parens => "Parenthesized expression"
    | Cons => "Cons"
    | ListConcat => "List Concatenation"
    | BinOp(op) => show_binop(op)
    | UnOp(op) => show_unop(op)
    | Match => "Case expression";

  let get_label: t => option((LabeledTuple.t, t)) =
    e =>
      switch (e.term) {
      | TupLabel(s, e') => Some((s, e'))
      | _ => None
      };

  // Typfun should be treated as a function here as this is only used to
  // determine when to allow for recursive definitions in a let binding.
  let rec is_fun = (e: t) => {
    switch (e.term) {
    | Parens(e) => is_fun(e)
    | TupLabel(_, e) => is_fun(e)
    | TypFun(_)
    | Fun(_) => true
    | Dot(e, s) =>
      let element: option(t) =
        switch (e.term) {
        | Tuple(ts) => LabeledTuple.find_label(get_label, ts, s)
        | _ => None // TODO (Anthony): other exps
        };
      switch (element) {
      | Some(exp) => is_fun(exp)
      | None => false
      };
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Triv
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
    | TyAlias(_)
    | Ap(_)
    | TypAp(_)
    | DeferredAp(_)
    | Pipeline(_)
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
    };
  };

  let rec is_tuple_of_functions = (e: t) =>
    is_fun(e)
    || (
      switch (e.term) {
      | Parens(e) => is_tuple_of_functions(e)
      | TupLabel(_, e) => is_tuple_of_functions(e)
      | Tuple(es) => es |> List.for_all(is_fun)
      | Dot(e, s) =>
        let element: option(t) =
          switch (e.term) {
          | Tuple(ts) => LabeledTuple.find_label(get_label, ts, s)
          | _ => None // TODO (Anthony): other exps
          };
        switch (element) {
        | Some(exp) => is_tuple_of_functions(exp)
        | None => false
        };
      | Invalid(_)
      | EmptyHole
      | MultiHole(_)
      | Triv
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
      | TyAlias(_)
      | Ap(_)
      | TypAp(_)
      | DeferredAp(_)
      | Pipeline(_)
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
      | Parens(e)
      | TupLabel(_, e)
      | Dot(e, _) => get_num_of_functions(e)
      | Tuple(es) => is_tuple_of_functions(e) ? Some(List.length(es)) : None
      | Invalid(_)
      | EmptyHole
      | MultiHole(_)
      | Triv
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
      | Filter(_)
      | TyAlias(_)
      | Ap(_)
      | TypAp(_)
      | DeferredAp(_)
      | Pipeline(_)
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

// TODO(d): consider just folding this into UExp
module URul = {
  include TermBase.URul;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Rule;

  // example of awkwardness induced by having forms like rules
  // that may have a different-sorted child with no delimiters
  // (eg scrut with no rules)
  let ids = (~any_ids, {ids, term}: t) =>
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
    | [] => raise(Invalid_argument("Term.UExp.rep_id"))
    | [id, ..._] => id
    };
};

module Cls = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Exp(UExp.cls)
    | Pat(UPat.cls)
    | Typ(UTyp.cls)
    | TPat(UTPat.cls)
    | Rul(URul.cls)
    | Secondary(Secondary.cls);

  let show = (cls: t) =>
    switch (cls) {
    | Exp(cls) => UExp.show_cls(cls)
    | Pat(cls) => UPat.show_cls(cls)
    | Typ(cls) => UTyp.show_cls(cls)
    | TPat(cls) => UTPat.show_cls(cls)
    | Rul(cls) => URul.show_cls(cls)
    | Secondary(cls) => Secondary.show_cls(cls)
    };
};

let rec ids =
  fun
  | Exp(tm) => tm.ids
  | Pat(tm) => tm.ids
  | Typ(tm) => tm.ids
  | TPat(tm) => tm.ids
  | Rul(tm) => URul.ids(~any_ids=ids, tm)
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
  | Exp(tm) => UExp.rep_id(tm)
  | Pat(tm) => UPat.rep_id(tm)
  | Typ(tm) => UTyp.rep_id(tm)
  | TPat(tm) => UTPat.rep_id(tm)
  | Rul(tm) => URul.rep_id(~any_ids=ids, tm)
  | Nul ()
  | Any () => raise(Invalid_argument("Term.rep_id"));
