open Haz3lcore;
open Util;

/*
  These are the syntax test functions used for the syntax validation
  section of the exercises. The syntax tests are designed to
  ensure that the user implementation satisfies certain syntax properties
  e.g. tail recursive function or variable usage.
 */

[@deriving (show({with_path: false}), sexp, yojson)]
type syntax_result = {
  results: list(bool),
  percentage: float,
};

let rec find_var_upat = (name: string, upat: Term.UPat.t): bool => {
  switch (upat.term) {
  | Var(x) => x == name
  | EmptyHole
  | Wild
  | Triv
  | Invalid(_)
  | MultiHole(_)
  | Int(_)
  | Float(_)
  | Bool(_)
  | String(_)
  | Constructor(_) => false
  | Cons(up1, up2) => find_var_upat(name, up1) || find_var_upat(name, up2)
  | ListLit(l)
  | Tuple(l) =>
    List.fold_left((acc, up) => {acc || find_var_upat(name, up)}, false, l)
  | Parens(up) => find_var_upat(name, up)
  | Ap(up1, up2) => find_var_upat(name, up1) || find_var_upat(name, up2)
  | TypeAnn(up, _) => find_var_upat(name, up)
  };
};

/*
  Helper function used in the function find_fn which takes the
  pattern (upat) and the definition (def) of a let expression and
  collects functions in def that are bound to variable name in
  upat. Example: for the expression "let (a,b) = (fun x -> x+1, 41) in"
  if name="a", then l=[fun x -> x+1]
 */
let rec find_in_let =
        (
          name: string,
          upat: Term.UPat.t,
          def: Term.UExp.t,
          l: list(Term.UExp.t),
        )
        : list(Term.UExp.t) => {
  switch (upat.term, def.term) {
  | (Parens(up), Parens(ue)) => find_in_let(name, up, ue, l)
  | (Parens(up), _) => find_in_let(name, up, def, l)
  | (_, Parens(ue)) => find_in_let(name, upat, ue, l)
  | (TypeAnn(up, _), _) => find_in_let(name, up, def, l)
  | (Var(x), Fun(_)) => x == name ? [def, ...l] : l
  | (Tuple(pl), Tuple(ul)) =>
    if (List.length(pl) != List.length(ul)) {
      l;
    } else {
      List.fold_left2(
        (acc, up, ue) => {find_in_let(name, up, ue, acc)},
        l,
        pl,
        ul,
      );
    }
  | (Var(_), _)
  | (Tuple(_), _)
  | (
      EmptyHole | Wild | Triv | Invalid(_) | MultiHole(_) | Int(_) | Float(_) |
      Bool(_) |
      String(_) |
      ListLit(_) |
      Constructor(_) |
      Cons(_, _) |
      Ap(_, _),
      _,
    ) => l
  };
};

/*
 Find any function expressions in uexp that are bound to variable name
 */
let rec find_fn =
        (name: string, uexp: Term.UExp.t, l: list(Term.UExp.t))
        : list(Term.UExp.t) => {
  switch (uexp.term) {
  | Let(up, def, body) =>
    l |> find_in_let(name, up, def) |> find_fn(name, body)
  | ListLit(ul)
  | Tuple(ul) =>
    List.fold_left((acc, u1) => {find_fn(name, u1, acc)}, l, ul)
  | TypFun(_, body)
  | Fun(_, body) => l |> find_fn(name, body)
  | TypAp(u1, _)
  | Parens(u1)
  | UnOp(_, u1)
  | TyAlias(_, _, u1)
  | Test(u1)
  | Filter(_, _, u1) => l |> find_fn(name, u1)
  | Ap(u1, u2)
  | Pipeline(u1, u2)
  | Seq(u1, u2)
  | Cons(u1, u2)
  | ListConcat(u1, u2)
  | BinOp(_, u1, u2) => l |> find_fn(name, u1) |> find_fn(name, u2)
  | If(u1, u2, u3) =>
    l |> find_fn(name, u1) |> find_fn(name, u2) |> find_fn(name, u3)
  | DeferredAp(fn, args) =>
    l
    |> find_fn(name, fn)
    |> List.fold_left((l, u) => find_fn(name, u, l), _, args)
  | Match(u1, ul) =>
    List.fold_left(
      (acc, (_, ue)) => {find_fn(name, ue, acc)},
      l |> find_fn(name, u1),
      ul,
    )
  | EmptyHole
  | Triv
  | Deferral(_)
  | Invalid(_)
  | MultiHole(_)
  | Bool(_)
  | Int(_)
  | Float(_)
  | String(_)
  | Constructor(_)
  | Var(_) => l
  };
};

/*
 Finds whether variable name is ever mentioned in upat.
 */
let rec var_mention_upat = (name: string, upat: Term.UPat.t): bool => {
  switch (upat.term) {
  | Var(x) => x == name
  | EmptyHole
  | Wild
  | Triv
  | Invalid(_)
  | MultiHole(_)
  | Int(_)
  | Float(_)
  | Bool(_)
  | String(_)
  | Constructor(_) => false
  | Cons(up1, up2) =>
    var_mention_upat(name, up1) || var_mention_upat(name, up2)
  | ListLit(l)
  | Tuple(l) =>
    List.fold_left(
      (acc, up) => {acc || var_mention_upat(name, up)},
      false,
      l,
    )
  | Parens(up) => var_mention_upat(name, up)
  | Ap(up1, up2) =>
    var_mention_upat(name, up1) || var_mention_upat(name, up2)
  | TypeAnn(up, _) => var_mention_upat(name, up)
  };
};

/*
 Finds whether variable name is ever mentioned in uexp.
 */
let rec var_mention = (name: string, uexp: Term.UExp.t): bool => {
  switch (uexp.term) {
  | Var(x) => x == name
  | EmptyHole
  | Triv
  | Invalid(_)
  | MultiHole(_)
  | Bool(_)
  | Int(_)
  | Float(_)
  | String(_)
  | Constructor(_)
  | Deferral(_) => false
  | Fun(args, body) =>
    var_mention_upat(name, args) ? false : var_mention(name, body)
  | ListLit(l)
  | Tuple(l) =>
    List.fold_left((acc, ue) => {acc || var_mention(name, ue)}, false, l)
  | Let(p, def, body) =>
    var_mention_upat(name, p)
      ? false : var_mention(name, def) || var_mention(name, body)
  | TypFun(_, u)
  | TypAp(u, _)
  | Test(u)
  | Parens(u)
  | UnOp(_, u)
  | TyAlias(_, _, u)
  | Filter(_, _, u) => var_mention(name, u)
  | Ap(u1, u2)
  | Pipeline(u1, u2)
  | Seq(u1, u2)
  | Cons(u1, u2)
  | ListConcat(u1, u2)
  | BinOp(_, u1, u2) => var_mention(name, u1) || var_mention(name, u2)
  | DeferredAp(u1, us) =>
    var_mention(name, u1) || List.exists(var_mention(name), us)
  | If(u1, u2, u3) =>
    var_mention(name, u1) || var_mention(name, u2) || var_mention(name, u3)
  | Match(g, l) =>
    var_mention(name, g)
    || List.fold_left(
         (acc, pe) => {
           let (p, e) = pe;
           var_mention_upat(name, p) ? false : acc || var_mention(name, e);
         },
         false,
         l,
       )
  };
};

/*
 Finds whether variable name is applied on another expresssion.
 i.e. Ap(Var(name), u) occurs anywhere in the uexp.
 */
let rec var_applied = (name: string, uexp: Term.UExp.t): bool => {
  switch (uexp.term) {
  | Var(_)
  | EmptyHole
  | Triv
  | Invalid(_)
  | MultiHole(_)
  | Bool(_)
  | Int(_)
  | Float(_)
  | String(_)
  | Constructor(_)
  | Deferral(_) => false
  | Fun(args, body) =>
    var_mention_upat(name, args) ? false : var_applied(name, body)
  | ListLit(l)
  | Tuple(l) =>
    List.fold_left((acc, ue) => {acc || var_applied(name, ue)}, false, l)
  | Let(p, def, body) =>
    var_mention_upat(name, p)
      ? false : var_applied(name, def) || var_applied(name, body)
  | TypFun(_, u)
  | Test(u)
  | Parens(u)
  | UnOp(_, u)
  | TyAlias(_, _, u)
  | Filter(_, _, u) => var_applied(name, u)
  | TypAp(u, _) =>
    switch (u.term) {
    | Var(x) => x == name ? true : false
    | _ => var_applied(name, u)
    }
  | Ap(u1, u2) =>
    switch (u1.term) {
    | Var(x) => x == name ? true : var_applied(name, u2)
    | _ => var_applied(name, u1) || var_applied(name, u2)
    }
  | DeferredAp(u1, us) =>
    switch (u1.term) {
    | Var(x) => x == name ? true : List.exists(var_applied(name), us)
    | _ => List.exists(var_applied(name), us)
    }
  | Pipeline(u1, u2) =>
    switch (u2.term) {
    | Var(x) => x == name ? true : var_applied(name, u1)
    | _ => var_applied(name, u1) || var_applied(name, u2)
    }
  | Cons(u1, u2)
  | Seq(u1, u2)
  | ListConcat(u1, u2)
  | BinOp(_, u1, u2) => var_applied(name, u1) || var_applied(name, u2)
  | If(u1, u2, u3) =>
    var_applied(name, u1) || var_applied(name, u2) || var_applied(name, u3)
  | Match(g, l) =>
    var_applied(name, g)
    || List.fold_left(
         (acc, pe) => {
           let (p, e) = pe;
           var_mention_upat(name, p) ? false : acc || var_applied(name, e);
         },
         false,
         l,
       )
  };
};

/*
 Check whether all functions bound to variable name are recursive.
 */
let is_recursive = (name: string, uexp: Term.UExp.t): bool => {
  let fn_bodies = [] |> find_fn(name, uexp);
  if (List.length(fn_bodies) == 0) {
    false;
  } else {
    List.fold_left(
      (acc, ue) => {acc && var_mention(name, ue)},
      true,
      fn_bodies,
    );
  };
};

/*
 Check if variable name is not mentioned anywhere outside of
 a tail position in uexp. Note that if the variable is not
 mentioned anywhere in the expression, the function returns true.
 */
let rec tail_check = (name: string, uexp: Term.UExp.t): bool => {
  switch (uexp.term) {
  | EmptyHole
  | Triv
  | Deferral(_)
  | Invalid(_)
  | MultiHole(_)
  | Bool(_)
  | Int(_)
  | Float(_)
  | String(_)
  | Constructor(_)
  | Var(_) => true
  | Fun(args, body) =>
    var_mention_upat(name, args) ? false : tail_check(name, body)
  | Let(p, def, body) =>
    var_mention_upat(name, p) || var_mention(name, def)
      ? false : tail_check(name, body)
  | ListLit(l)
  | Tuple(l) =>
    //If l has no recursive calls then true
    !List.fold_left((acc, ue) => {acc || var_mention(name, ue)}, false, l)
  | Test(_) => false
  | TyAlias(_, _, u)
  | Filter(_, _, u)
  | TypFun(_, u)
  | TypAp(u, _)
  | Parens(u) => tail_check(name, u)
  | UnOp(_, u) => !var_mention(name, u)
  | Ap(u1, u2) => var_mention(name, u2) ? false : tail_check(name, u1)
  | DeferredAp(fn, args) =>
    tail_check(
      name,
      {ids: [], term: Ap(fn, {ids: [], term: Tuple(args)})},
    )
  | Pipeline(u1, u2) => var_mention(name, u1) ? false : tail_check(name, u2)
  | Seq(u1, u2) => var_mention(name, u1) ? false : tail_check(name, u2)
  | Cons(u1, u2)
  | ListConcat(u1, u2)
  | BinOp(_, u1, u2) => !(var_mention(name, u1) || var_mention(name, u2))
  | If(u1, u2, u3) =>
    var_mention(name, u1)
      ? false : tail_check(name, u2) && tail_check(name, u3)
  | Match(g, l) =>
    var_mention(name, g)
      ? false
      : List.fold_left(
          (acc, (p, e)) => {
            var_mention_upat(name, p) ? false : acc && tail_check(name, e)
          },
          true,
          l,
        )
  };
};

/*
 Check whether all functions bound to variable name are tail recursive.
 */
let is_tail_recursive = (name: string, uexp: Term.UExp.t): bool => {
  let fn_bodies = [] |> find_fn(name, uexp);
  if (List.length(fn_bodies) == 0) {
    false;
  } else {
    List.fold_left(
      (acc, ue) => {acc && var_mention(name, ue) && tail_check(name, ue)},
      true,
      fn_bodies,
    );
  };
};

let check =
    (uexp: Term.UExp.t, predicates: list(Term.UExp.t => bool)): syntax_result => {
  let results = List.map(pred => {uexp |> pred}, predicates);
  let length = List.length(predicates);
  let passing = Util.ListUtil.count_pred(res => res, results);

  {
    results,
    percentage:
      //vacuously passes if there are no tests
      length == 0 ? 1. : float_of_int(passing) /. float_of_int(length),
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type predicate =
  | VarApplied(string)
  | IsRecursive(string)
  | IsNotRecursive(string)
  | IsTailRecursive(string);

let predicate_fn = predicate => {
  switch (predicate) {
  | VarApplied(name) => var_applied(name)
  | IsRecursive(name) => is_recursive(name)
  | IsNotRecursive(name) => (uexp => !is_recursive(name, uexp))
  | IsTailRecursive(name) => is_tail_recursive(name)
  };
};
