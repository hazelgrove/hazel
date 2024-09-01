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

let to_term = s =>
  List.fold_left(
    (acc, ctr) =>
      Option.is_none(acc) && show_cls(ctr) == s ? Some(ctr) : acc,
    None,
    all,
  );

let mk_ctr_entry: cls => Ctx.var_entry =
  term => {name: show_cls(term), id: Id.mk(), typ: cls_to_typ(term)};

let ctr_entries = all |> List.map(mk_ctr_entry);

let mk_variant: cls => ConstructorMap.variant(Typ.t) =
  term => {
    Variant(show_cls(term), [Id.mk()], cls_to_arg_typ(term));
  };

let tvar_entries = {
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
      // [(Judgement, []), (Prop, []), (Expr, []), (BinOp, []), (UnOp, [])],
      [],
      all,
    );
  // ignore(map);
  // let map: list((alias, list(cls))) = [
  //   (Judgement, [Entail]),
  //   (Prop, [Atom, And, Or, Implies, Truth, Falsity]),
  // ];
  // ignore(map);
  // let map = [];
  print_endline(
    map
    |> List.map(((alias, cls)) =>
         show_alias(alias)
         ++ ": "
         ++ (cls |> List.map(show_cls) |> String.concat(", "))
       )
    |> String.concat(";\n"),
  );
  List.map(
    ((alias, cls)) =>
      Ctx.TVarEntry({
        name: show_alias(alias),
        id: Id.invalid,
        kind: Ctx.Singleton(Sum(cls |> List.map(mk_variant)) |> Typ.fresh),
      }),
    map,
  );
};

let term_of_dhexp: DHExp.t => option(cls) =
  d =>
    switch (DHExp.term_of(d)) {
    | Constructor(ctr, _) => ctr |> to_term
    | _ => None
    // Need check?
    // Cons: ty |> Typ.term_of == Typ.Prop
    // Ap:    Typ.matched_arrow([], ty)
    //       |> snd
    //       |> Typ.term_of == Typ.Prop
    };

let rec prop_of_dhexp: DHExp.t => t =
  d => {
    let (fn, arg) =
      switch (DHExp.term_of(d)) {
      | Ap(_, fn, arg) => (fn, Some(arg))
      | _ => (d, None)
      };
    switch (term_of_dhexp(fn)) {
    | None =>
      print_endline("Cls not found: " ++ DHExp.show(d));
      Hole("Cls not found") |> Derivation.Prop.fresh;
    | Some(ctr) =>
      switch (ctr, arg) {
      | (Atom, Some({term: String(atom), _})) =>
        Atom(atom) |> Derivation.Prop.fresh
      | (And | Or | Implies, Some({term: Tuple([d1, d2]), _})) =>
        let (p1, p2) = (prop_of_dhexp(d1), prop_of_dhexp(d2));
        (
          switch (ctr) {
          | And => And(p1, p2)
          | Or => Or(p1, p2)
          | Implies => Implies(p1, p2)
          | _ => failwith("Impossible")
          }
        )
        |> Derivation.Prop.fresh;
      | (Truth, None) => Truth |> Derivation.Prop.fresh
      | (Falsity, None) => Falsity |> Derivation.Prop.fresh
      | (Entail, Some({term: Tuple([{term: ListLit(l), _}, d]), _})) =>
        let ctx = List.map(prop_of_dhexp, l);
        let p = prop_of_dhexp(d);
        Entail(Ctx(ctx) |> Derivation.Prop.fresh, p) |> Derivation.Prop.fresh;
      | _ =>
        print_endline("Argument Error: " ++ DHExp.show(d));
        Hole("Argument Error") |> Derivation.Prop.fresh;
      }
    };
  };
