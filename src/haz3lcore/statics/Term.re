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

open Util;
include TermBase.Any;

type any = t;
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
    | Tuple
    | Sum
    | List
    | Var
    | Module
    | Tag
    | Parens
    | Dot
    | Ap;

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
    | Tag(_) => Tag
    | TupLabel(_) => Int //TODO
    | Tuple(_) => Tuple
    | Parens(_) => Parens
    | Module(_) => Module
    | Ap(_) => Ap
    | Dot(_) => Dot
    | Sum(_) => Sum;

  let show_cls: cls => string =
    fun
    | Invalid => "Invalid Type"
    | EmptyHole => "Empty Type Hole"
    | MultiHole => "Multi Type Hole"
    | Int
    | Float
    | String
    | Bool => "Base Type"
    | Var => "Type Variable"
    | Tag => "Sum Constructor"
    | List => "List Type"
    | Arrow => "Function Type"
    | Tuple => "Product Type"
    | Sum => "Sum Type"
    | Parens => "Parenthesized Type Term"
    | Module => "Module Type"
    | Dot => "Member Type"
    | Ap => "Sum Constructor Application";

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
    | TupLabel(_)
    | Tuple(_)
    | Var(_)
    | Module(_)
    | Tag(_)
    | Ap(_)
    | Dot(_)
    | Sum(_) => false
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
      | TupLabel(_, e) => to_typ(ctx, e)
      | Tuple(us) => Prod(List.map(to_typ(ctx), us))
      | Sum(uts) => Sum(to_tag_map(ctx, uts))
      | List(u) => List(to_typ(ctx, u))
      | Parens(u) => to_typ(ctx, u)
      /* The below cases should occur only inside sums */
      | Tag(_)
      | Ap(_) => Unknown(Internal)
      | Module(u) =>
        let rep_id_p = ({ids, _}: TermBase.UPat.t) => {
          assert(ids != []);
          List.hd(ids);
        };
        let rep_id_t = ({ids, _}: TermBase.UTPat.t) => {
          assert(ids != []);
          List.hd(ids);
        };
        let rec upat_to_ctx =
                ((outer_ctx: Ctx.t, inner_ctx: Ctx.t), upat: TermBase.UPat.t) => {
          switch (upat.term) {
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
          | Cons(_)
          | Tuple(_)
          | Ap(_) => (outer_ctx, inner_ctx)
          | TypeAnn(var, utyp1) =>
            switch (var.term, utyp1.term) {
            | (Var(name), _) => (
                outer_ctx,
                [
                  VarEntry({
                    name,
                    id: rep_id_p(var),
                    typ: to_typ(outer_ctx, utyp1),
                  }),
                  ...inner_ctx,
                ],
              )
            | (Tag(name), _) => (
                outer_ctx,
                [
                  TagEntry({
                    name,
                    id: rep_id_p(var),
                    typ: to_typ(outer_ctx, utyp1),
                  }),
                  ...inner_ctx,
                ],
              )
            | _ => (outer_ctx, inner_ctx)
            }
          | Var(name) => (
              outer_ctx,
              [
                VarEntry({name, id: rep_id_p(upat), typ: Unknown(TypeHole)}),
                ...inner_ctx,
              ],
            )
          | Tag(name) => (
              outer_ctx,
              [
                TagEntry({name, id: rep_id_p(upat), typ: Unknown(TypeHole)}),
                ...inner_ctx,
              ],
            )
          | TyAlias(typat, utyp) =>
            switch (typat.term) {
            | Var(name)
                when
                  !Form.is_base_typ(name)
                  && Ctx.lookup_alias(inner_ctx, name) == None =>
              /* NOTE(andrew):  See TyAlias in Statics.uexp_to_info_map  */
              let (ty_def, ctx_body, new_inner) = {
                let ty_pre =
                  to_typ(Ctx.extend_dummy_tvar(outer_ctx, name), utyp);
                switch (utyp.term) {
                | Sum(_) when List.mem(name, Typ.free_vars(ty_pre)) =>
                  let ty_rec =
                    Typ.Rec("α", Typ.subst(Var("α"), name, ty_pre));
                  (
                    ty_rec,
                    Ctx.extend_alias(
                      outer_ctx,
                      name,
                      rep_id_t(typat),
                      ty_rec,
                    ),
                    Ctx.extend_alias(
                      inner_ctx,
                      name,
                      rep_id_t(typat),
                      ty_rec,
                    ),
                  );
                | _ =>
                  let ty = to_typ(outer_ctx, utyp);
                  (
                    ty,
                    Ctx.extend_alias(outer_ctx, name, rep_id_t(typat), ty),
                    Ctx.extend_alias(inner_ctx, name, rep_id_t(typat), ty),
                  );
                };
              };
              switch (Typ.get_sum_tags(outer_ctx, ty_def)) {
              | Some(sm) => (
                  Ctx.add_tags(ctx_body, name, rep_id(utyp), sm),
                  Ctx.add_tags(new_inner, name, rep_id(utyp), sm),
                )
              | None => (ctx_body, new_inner)
              };

            | _ => (outer_ctx, inner_ctx)
            }
          | Parens(p) => upat_to_ctx((outer_ctx, inner_ctx), p)
          };
        };
        let rec get_Tuple: TermBase.UPat.t => Typ.t = (
          ut =>
            switch (ut.term) {
            | Tuple(us) =>
              let (_, module_ctx) =
                List.fold_left(upat_to_ctx, (ctx, []), us);
              Module(module_ctx);
            | Parens(p) => get_Tuple(p)
            | TypeAnn(_)
            | Var(_)
            | Tag(_)
            | TyAlias(_)
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
            | Cons(_)
            | Ap(_) =>
              let (_, module_ctx) = upat_to_ctx((ctx, []), ut);
              Module(module_ctx);
            }
        );
        get_Tuple(u);
      | Dot(exp, name) =>
        /** Currently, the only possible way to introduce modules are through
      a variable in Tag form.

      Maybe better to put to_typ in Statics? */
        open Util.OptUtil.Syntax;
        let rec inner_normalize = (ctx: Ctx.t, ty: Typ.t): option(Typ.t) =>
          switch (ty) {
          | Var(x) =>
            let* ty = Ctx.lookup_alias(ctx, x);
            inner_normalize(ctx, ty);
          | _ => Some(ty)
          };
        let rec get_module =
                (name: string, ctx: Ctx.t, e: TermBase.UExp.term)
                : option((string, option(Ctx.t))) => {
          switch (e) {
          | Tag(tag_name) =>
            switch (Ctx.lookup_tag(ctx, tag_name)) {
            | Some({typ: Module(inner_ctx), _}) =>
              Some((name ++ tag_name ++ ".", Some(inner_ctx)))
            | _ => Some((name ++ tag_name ++ ".", None))
            }
          | Dot({term, _}, tag_name) =>
            let+ (name, ctx) = get_module(name, ctx, term);
            let inner_ctx = {
              let* ctx = ctx;
              switch (Ctx.lookup_tag(ctx, tag_name)) {
              | Some({typ: Module(inner_ctx), _}) => Some(inner_ctx)
              | _ => None
              };
            };
            (name ++ tag_name ++ ".", inner_ctx);
          | _ => None
          };
        };
        let res = {
          let+ (tag_name, inner_ctx) = get_module("", ctx, exp.term);
          let ty = {
            let* inner_ctx = inner_ctx;
            inner_normalize(inner_ctx, Var(name));
          };
          (tag_name ++ name, ty);
        };
        switch (res) {
        | Some((name, Some(ty))) => Member(name, ty)
        | Some((name, None)) => Member(name, Unknown(Internal))
        | None => Member("..." ++ name, Unknown(Internal))
        };
      }
  and to_variant: (Ctx.t, variant) => option(TagMap.binding(option(Typ.t))) =
    ctx =>
      fun
      | Variant(tag, _, u) => Some((tag, Option.map(to_typ(ctx), u)))
      | BadEntry(_) => None
  and to_tag_map = (ctx: Ctx.t, uts: list(variant)): Typ.sum_map => {
    List.fold_left(
      (acc, ut) =>
        List.find_opt(((tag, _)) => tag == fst(ut), acc) == None
          ? acc @ [ut] : acc,
      [],
      List.filter_map(to_variant(ctx), uts),
    );
  };
};

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
    | Invalid => "Invalid Type Variable"
    | EmptyHole => "Empty Type Variable Hole"
    | MultiHole => "Multi Type Variable Hole"
    | Var => "Type Variable";
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
    | Tag
    | Cons
    | Var
    | Tuple
    | Parens
    | Ap
    | TypeAnn
    | TyAlias;

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
    | Tag(_) => Tag
    | Cons(_) => Cons
    | Var(_) => Var
    | Tuple(_) => Tuple
    | Parens(_) => Parens
    | Ap(_) => Ap
    | TypeAnn(_) => TypeAnn
    | TyAlias(_) => TyAlias;

  let show_cls: cls => string =
    fun
    | Invalid => "Invalid Pattern"
    | EmptyHole => "Empty Pattern Hole"
    | MultiHole => "Multi Pattern Hole"
    | Wild => "Wildcard Pattern"
    | Int => "Integer Pattern Literal"
    | Float => "Float Pattern Literal"
    | Bool => "Boolean Pattern Literal"
    | String => "String Pattern Literal"
    | Triv => "Trivial Pattern Literal"
    | ListLit => "List Literal Pattern"
    | Tag => "Constructor Pattern"
    | Cons => "Cons Pattern"
    | Var => "Pattern Variable"
    | Tuple => "Tuple Pattern"
    | Parens => "Parenthesized Pattern"
    | Ap => "Constructor Application Pattern"
    | TypeAnn => "Type Annotation"
    | TyAlias => "Type Alias Definition Pattern";

  let rec is_var = (pat: t) => {
    switch (pat.term) {
    | Parens(pat) => is_var(pat)
    | Var(_) => true
    | TypeAnn(_)
    | TyAlias(_)
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
    | Tag(_)
    | Ap(_) => false
    };
  };

  let rec is_fun_var = (pat: t) => {
    switch (pat.term) {
    | Parens(pat) => is_fun_var(pat)
    | TypeAnn(pat, typ) => is_var(pat) && UTyp.is_arrow(typ)
    | TyAlias(_)
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
    | Tag(_)
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
      | Triv
      | ListLit(_)
      | Cons(_, _)
      | Var(_)
      | TypeAnn(_)
      | TyAlias(_)
      | Tag(_)
      | Ap(_) => false
      }
    );

  let rec get_var = (pat: t) => {
    switch (pat.term) {
    | Parens(pat) => get_var(pat)
    | Var(x) => Some(x)
    | TypeAnn(_)
    | TyAlias(_)
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
    | Tag(_)
    | Ap(_) => None
    };
  };

  let rec get_tag = (pat: t) => {
    switch (pat.term) {
    | Parens(pat) => get_tag(pat)
    | Tag(x) => Some(x)
    | TypeAnn(_)
    | TyAlias(_)
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
    | Var(_)
    | Ap(_) => None
    };
  };

  let rec get_fun_var = (pat: t) => {
    switch (pat.term) {
    | Parens(pat) => get_fun_var(pat)
    | TypeAnn(pat, typ) =>
      if (UTyp.is_arrow(typ)) {
        get_var(pat) |> Option.map(var => var);
      } else {
        None;
      }
    | TyAlias(_)
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
    | Tag(_)
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
      | Triv
      | ListLit(_)
      | Cons(_, _)
      | Var(_)
      | TypeAnn(_)
      | TyAlias(_)
      | Tag(_)
      | Ap(_) => None
      }
    };
  };

  let tag_name = (p: t): option(Tag.t) =>
    switch (p.term) {
    | Tag(name) => Some(name)
    | _ => None
    };
};

module UExp = {
  include TermBase.UExp;

  let hole = (tms: list(any)) =>
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
    | Bool(_) => Bool
    | Int(_) => Int
    | Float(_) => Float
    | String(_) => String
    | ListLit(_) => ListLit
    | Tag(_) => Tag
    | Fun(_) => Fun
    | TupLabel(_) => Int //TODO
    | Tuple(_) => Tuple
    | Var(_) => Var
    | Let(_) => Let
    | Module(_) => Module
    | Dot(_) => Dot
    | TyAlias(_) => TyAlias
    | Ap(_) => Ap
    | If(_) => If
    | Seq(_) => Seq
    | Test(_) => Test
    | Parens(_) => Parens
    | Cons(_) => Cons
    | UnOp(op, _) => UnOp(op)
    | BinOp(op, _, _) => BinOp(op)
    | Match(_) => Match;

  let show_op_un_int: op_un_int => string =
    fun
    | Minus => "Integer Negation";

  let show_unop: op_un => string =
    fun
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
    | Equals => "Integer Equality";

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
    | Equals => "Float Equality";

  let show_op_bin_string: op_bin_string => string =
    fun
    | Equals => "String Equality";

  let show_binop: op_bin => string =
    fun
    | Int(op) => show_op_bin_int(op)
    | Float(op) => show_op_bin_float(op)
    | Bool(op) => show_op_bin_bool(op)
    | String(op) => show_op_bin_string(op);

  let show_cls: cls => string =
    fun
    | Invalid => "Invalid Expression"
    | EmptyHole => "Empty Expression Hole"
    | MultiHole => "Multi Expression Hole"
    | Triv => "Trivial Literal. Pathetic, really."
    | Bool => "Boolean Literal"
    | Int => "Integer Literal"
    | Float => "Float Literal"
    | String => "String Literal"
    | ListLit => "List Literal"
    | Tag => "Constructor"
    | Fun => "Function Literal"
    | Tuple => "Tuple Literal"
    | Var => "Variable Reference"
    | Let => "Let Expression"
    | Module => "Module Expression"
    | Dot => "Dot Access"
    | TyAlias => "Type Alias Definition"
    | Ap => "Function/Contructor Application"
    | If => "If Expression"
    | Seq => "Sequence Expression"
    | Test => "Test (Effectful)"
    | Parens => "Parenthesized Expression"
    | Cons => "Cons"
    | BinOp(op) => show_binop(op)
    | UnOp(op) => show_unop(op)
    | Match => "Match Expression";

  let rec is_fun = (e: t) => {
    switch (e.term) {
    | Parens(e) => is_fun(e)
    | Fun(_) => true
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Triv
    | Bool(_)
    | Int(_)
    | Float(_)
    | String(_)
    | ListLit(_)
    | TupLabel(_)
    | Tuple(_)
    | Var(_)
    | Let(_)
    | Module(_)
    | Dot(_)
    | TyAlias(_)
    | Ap(_)
    | If(_)
    | Seq(_)
    | Test(_)
    | Cons(_)
    | UnOp(_)
    | BinOp(_)
    | Match(_)
    | Tag(_) => false
    };
  };

  let rec is_tuple_of_functions = (e: t) =>
    is_fun(e)
    || (
      switch (e.term) {
      | Parens(e) => is_tuple_of_functions(e)
      | Tuple(es) => es |> List.for_all(is_fun)
      | Invalid(_)
      | EmptyHole
      | MultiHole(_)
      | Triv
      | Bool(_)
      | Int(_)
      | Float(_)
      | String(_)
      | ListLit(_)
      | Fun(_)
      | Var(_)
      | Let(_)
      | Module(_)
      | Dot(_)
      | TyAlias(_)
      | Ap(_)
      | If(_)
      | Seq(_)
      | Test(_)
      | Cons(_)
      | UnOp(_)
      | BinOp(_)
      | Match(_)
      | TupLabel(_)
      | Tag(_) => false
      }
    );

  let tag_name = (e: t): option(Tag.t) =>
    switch (e.term) {
    | Tag(name) => Some(name)
    | _ => None
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
