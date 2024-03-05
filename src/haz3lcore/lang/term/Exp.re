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

let rep_id = ({ids, _}) => {
  assert(ids != []);
  List.hd(ids);
};

let unwrap = ({ids, term, copied}) => (term, term => {ids, term, copied});

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
