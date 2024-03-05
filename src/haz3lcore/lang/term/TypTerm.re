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
  | Constructor
  | Parens
  | Ap;

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
  | Constructor(_) => Constructor
  | Tuple(_) => Tuple
  | Parens(_) => Parens
  | Ap(_) => Ap
  | Sum(_) => Sum;

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
  | Tuple => "Product type"
  | Sum => "Sum type"
  | Parens => "Parenthesized type"
  | Ap => "Constructor application";

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
  | Tuple(_)
  | Var(_)
  | Constructor(_)
  | Ap(_)
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
    | Tuple(us) => Prod(List.map(to_typ(ctx), us))
    | Sum(uts) => Sum(to_ctr_map(ctx, uts))
    | List(u) => List(to_typ(ctx, u))
    | Parens(u) => to_typ(ctx, u)
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
