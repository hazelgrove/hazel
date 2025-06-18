open Util;

let repr_list_ana_exp = (anas: list(DrvSort.t)): string =>
  anas |> List.map(DrvSort.show) |> String.concat(", ");

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type ancestors = list(Id.t);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type error =
  | BadToken(string)
  | MultiHole
  | FreeVar
  | VarNoJoin(DrvSort.t, Typ.t) // expect, actual
  | NoJoin(DrvSort.t, list(DrvSort.t)); // expect, actuals

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type status =
  | NotInHole
  | InHole(error);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  term: Drv.Any.t,
  cls: Cls.t,
  ancestors,
  sort: DrvSort.t,
  status,
};

let sort_of: t => DrvSort.t = ({sort, _}) => sort;

let cls_of: t => Cls.t = ({cls, _}) => cls;

let id_of: t => Id.t = ({term, _}) => Drv.Any.rep_id(term);

let error_of: t => option(error) =
  ({status, _}) => {
    switch (status) {
    | NotInHole => None
    | InHole(err) => Some(err)
    };
  };

let is_error: t => bool =
  ({status, _}) =>
    switch (status) {
    | NotInHole => false
    | InHole(_) => true
    };

let ancestors_of: t => ancestors = ({ancestors, _}) => ancestors;

let sorts_of_exp: Drv.Exp.cls => list(DrvSort.t) =
  fun
  | Tuple => []
  | Hole(_)
  | Quote
  | Parens => [Jdmt, Ctx, Prop, Exp]
  | Var => [Ctx, Prop, Exp] // Ctx is only for documentation purposes
  | Val
  | Eval
  | Entail
  | Consistent
  | MatchedArrow
  | MatchedProd
  | MatchedSum => [Jdmt]
  | Ctx
  | Cons
  | Concat => [Ctx]
  | Type
  | HasType
  | Syn
  | Ana
  | And
  | Or
  | Impl
  | Truth
  | Falsity => [Prop]
  | NumLit
  | Neg
  | BinOp(_)
  | True
  | False
  | If
  | Let
  | Fix
  | Fun
  | Ap
  | Pair
  | Triv
  | PrjL
  | PrjR
  | Case
  | ExpHole
  | InjL
  | InjR
  | Roll
  | Unroll => [Exp];

let status = (drv: Drv.Any.t, ~sort: DrvSort.t): status =>
  switch (drv) {
  | Exp({term: Hole(Invalid(token)), _})
  | Pat({term: Hole(Invalid(token)), _})
  | Typ({term: Hole(Invalid(token)), _})
  | TPat({term: Hole(Invalid(token)), _}) => InHole(BadToken(token))
  | Exp({term: Hole(MultiHole(_)), _})
  | Pat({term: Hole(MultiHole(_)), _})
  | Typ({term: Hole(MultiHole(_)), _})
  | TPat({term: Hole(MultiHole(_)), _}) => InHole(MultiHole)
  | Exp({term, _}) =>
    let sorts = sorts_of_exp(Drv.Exp.cls_of_term(term));
    List.mem(sort, sorts) ? NotInHole : InHole(NoJoin(sort, sorts));
  | _ => NotInHole
  };

let derived = (drv: Drv.Any.t, ~ancestors, ~sort): t => {
  let cls = Cls.Drv(Drv.Any.cls_of(drv));
  let status = status(drv, ~sort);
  {
    term: drv,
    cls,
    status,
    ancestors,
    sort,
  };
};
