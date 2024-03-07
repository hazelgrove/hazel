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

let term_of = ({term, _}) => term;
// All children of term must have expression-unique ids.

let unwrap = ({ids, term}) => (term, term => {ids, term});

let fresh = term => {
  {ids: [Id.mk()], term};
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
  | TypeAnn(pat, typ) => is_var(pat) && UTyp.is_arrow(typ)
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
    if (UTyp.is_arrow(typ)) {
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
