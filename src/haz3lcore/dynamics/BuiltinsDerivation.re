open Derivation.Prop;

// The internal representation of Prop.t does not seperate the types. However,
// we could implement rules for frontend usage. The following code is a
// typeclass for Prop.t. Note that the typeclass is not used anywhere. It is
// just for reference.

// Propositional logic
type prop =
  | Atom(string)
  | And(prop, prop)
  | Or(prop, prop)
  | Implies(prop, prop)
  | Truth
  | Falsity;

// AL logic
type expr =
  | NumLit(int)
  | UnOp(unop, expr)
  | BinOp(binop, expr, expr)
and unop =
  | OpNeg
and binop =
  | OpPlus
  | OpMinus
  | OpTimes;

// Judgements (the outermost layer of this syntax)
type judgement =
  | Entail(list(prop), prop)
  | Val(expr)
  | Eval(expr, expr);

// The following code is the implementation of the typeclass for Prop.t.

[@deriving (show({with_path: false}), sexp, yojson)]
type alias =
  | Judgement
  | Expr
  | UnOp
  | BinOp
  | Prop;

let alias_fresh: alias => Typ.t = alia => Var(show_alias(alia)) |> Typ.fresh;

let cls_to_arg_typ: cls => list(Typ.t) =
  fun
  | Hole => failwith("impossible")

  | NumLit => [Int |> Typ.fresh]
  | Val => [Expr |> alias_fresh]
  | UnOp => [UnOp |> alias_fresh, Expr |> alias_fresh]
  | BinOp => [BinOp |> alias_fresh, Expr |> alias_fresh, Expr |> alias_fresh]
  | OpNeg
  | OpPlus
  | OpMinus
  | OpTimes => []
  | Eval => [Expr |> alias_fresh, Expr |> alias_fresh]

  | Atom => [String |> Typ.fresh]
  | And
  | Or
  | Implies => [Prop |> alias_fresh, Prop |> alias_fresh]
  | Truth
  | Falsity => []
  | Entail => [List(Prop |> alias_fresh) |> Typ.fresh, Prop |> alias_fresh]
  | Ctx => failwith("impossible");

let cls_to_arg_typ: cls => option(Typ.t) =
  cls =>
    switch (cls_to_arg_typ(cls)) {
    | [] => None
    | [ty] => Some(ty)
    | tys => Some(Prod(tys) |> Typ.fresh)
    };

let cls_to_alias: cls => alias =
  fun
  | Hole => failwith("impossible")

  | NumLit
  | UnOp
  | BinOp => Expr
  | OpNeg => UnOp
  | OpPlus
  | OpMinus
  | OpTimes => BinOp

  | Atom
  | And
  | Or
  | Implies
  | Truth
  | Falsity => Prop
  | Ctx => failwith("impossible")

  | Val
  | Eval
  | Entail => Judgement;

let cls_to_typ: cls => Typ.t =
  cls => {
    let alias_typ = cls_to_alias(cls) |> alias_fresh;
    switch (cls_to_arg_typ(cls)) {
    | None => alias_typ
    | Some(arg) => Arrow(arg, alias_typ) |> Typ.fresh
    };
  };

let all: list(cls) = [
  NumLit,
  Val,
  UnOp,
  BinOp,
  OpNeg,
  OpPlus,
  OpMinus,
  OpTimes,
  Eval,
  Atom,
  And,
  Or,
  Implies,
  Truth,
  Falsity,
  Entail,
];

let mk_ctr_entry: cls => Ctx.var_entry =
  term => {name: show_cls(term), id: Id.mk(), typ: cls_to_typ(term)};

let ctr_entries = all |> List.map(mk_ctr_entry);

let mk_variant: cls => ConstructorMap.variant(Typ.t) =
  term => {
    Variant(show_cls(term), [Id.mk()], cls_to_arg_typ(term));
  };

let add_tvar_entries = ctx => {
  let rec add_to_list = (map, cls) => {
    let alias = cls_to_alias(cls);
    switch (map) {
    | [] => [(alias, [cls])]
    | [(alias', cls'), ...rest] when alias == alias' => [
        (alias, [cls, ...cls']),
        ...rest,
      ]
    | [pair, ...rest] => [pair, ...add_to_list(rest, cls)]
    };
  };
  let map =
    List.fold_left(
      add_to_list,
      [],
      all,
    );
  List.fold_left(
    (ctx, (alias, cls)) => {
      let name = show_alias(alias);
      let ty = Sum(cls |> List.map(mk_variant)) |> Typ.fresh;
      let ty =
        switch (ty.term) {
        | Sum(_) when List.mem(name, Typ.free_vars(ty)) =>
          Typ.Rec(TPat.Var(name) |> IdTagged.fresh, ty) |> Typ.temp
        | _ => ty
        };
      let ctx = Ctx.extend_alias(ctx, name, Id.invalid, ty);
      switch (Typ.get_sum_constructors(ctx, ty)) {
      | Some(sm) => Ctx.add_ctrs(ctx, name, UTyp.rep_id(ty), sm)
      | None => ctx
      };
    },
    ctx,
    map,
  );
};

let to_term: string => option(cls) =
  s =>
    List.fold_left(
      (acc, ctr) =>
        Option.is_none(acc) && show_cls(ctr) == s ? Some(ctr) : acc,
      None,
      all,
    );

let term_of_dhexp: DHExp.t => option(cls) =
  d =>
    switch (DHExp.term_of(d)) {
    | Constructor(ctr, _) => ctr |> to_term
    | _ => None
    };

let rec prop_of_dhexp: DHExp.t => t =
  d => {
    let (fn, arg) =
      switch (DHExp.term_of(d)) {
      | Ap(_, fn, arg) => (fn, Some(arg))
      | _ => (d, None)
      };
    switch (term_of_dhexp(fn)) {
    | None => Hole("Cls not found") |> fresh
    | Some(ctr) => match_dhexp(ctr, arg)
    };
  }

and match_dhexp: (cls, option(DHExp.t)) => t =
  (ctr, arg) => {
    let (let.) = (x, f) =>
      switch (x) {
      | Some(x) => f(x)
      | None => Hole("Argument Error") |> fresh
      };
    let arg =
      switch (arg) {
      | None => []
      | Some(arg) =>
        switch (DHExp.term_of(arg)) {
        | Tuple(args) => args
        | _ => [arg]
        }
      };
    switch (ctr, arg) {
    | (NumLit, [d]) =>
      let. n = switch (DHExp.term_of(d)) {
      | Int(n) => Some(n)
      | _ => None
      }
      NumLit(n) |> fresh;
    | (Val, [d]) => 
      Val(prop_of_dhexp(d)) |> fresh
    | (UnOp,[d1, d2]) =>
      let (op, e) = (prop_of_dhexp(d1), prop_of_dhexp(d2));
      UnOp(op, e) |> fresh;
    | (BinOp, [d1, d2, d3]) =>
      let (op, e1, e2) = (
        prop_of_dhexp(d1),
        prop_of_dhexp(d2),
        prop_of_dhexp(d3),
      );
      BinOp(op, e1, e2) |> fresh;
    | (OpNeg, []) => OpNeg |> fresh
    | (OpPlus, []) => OpPlus |> fresh
    | (OpMinus, []) => OpMinus |> fresh
    | (OpTimes, []) => OpTimes |> fresh
    | (Eval, [d1, d2]) =>
      let (e1, e2) = (prop_of_dhexp(d1), prop_of_dhexp(d2));
      Eval(e1, e2) |> fresh;

    | (Atom, [d]) => 
      let. atom = switch (DHExp.term_of(d)) {
      | String(atom) => Some(atom)
      | _ => None
      }
      Atom(atom) |> fresh
    | (And, [d1, d2]) =>
      let (p1, p2) = (prop_of_dhexp(d1), prop_of_dhexp(d2));
      And(p1, p2) |> fresh;
    | (Or, [d1, d2]) =>
      let (p1, p2) = (prop_of_dhexp(d1), prop_of_dhexp(d2));
      Or(p1, p2) |> fresh;
    | (Implies, [d1, d2]) =>
      let (p1, p2) = (prop_of_dhexp(d1), prop_of_dhexp(d2));
      Implies(p1, p2) |> fresh;
    | (Truth, []) => Truth |> fresh
    | (Falsity, []) => Falsity |> fresh
    | (Entail, [d1, d2]) =>
      let. ctx = switch (DHExp.term_of(d1)) {
        | ListLit(l) => Some(l)
        | _ => None
      }
      let ctx = List.map(prop_of_dhexp, ctx);
      let p = prop_of_dhexp(d2);
      Entail(Ctx(ctx) |> fresh, p) |> fresh;
    | _ =>
      // print_endline("Argument Error: " ++ DHExp.show(d));
      Hole("Argument Error") |> fresh
    };
  };
