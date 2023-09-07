open Haz3lcore;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type syntax_result = {
  results: list(bool),
  percentage: float,
};

let rec find_var_upat = (name: string, upat: Term.UPat.t): bool => {
  switch (upat.term) {
  | Var(x) => x == name
  | Cons(up1, up2) => find_var_upat(name, up1) || find_var_upat(name, up2)
  | ListLit(l)
  | Tuple(l) =>
    List.fold_left((acc, up) => {acc || find_var_upat(name, up)}, false, l)
  | Parens(up) => find_var_upat(name, up)
  | Ap(up1, up2) => find_var_upat(name, up1) || find_var_upat(name, up2)
  | TypeAnn(up, _) => find_var_upat(name, up)
  | _ => false
  };
};

let rec var_mention = (name: string, uexp: Term.UExp.t): bool => {
  switch (uexp.term) {
  | Var(x) => x == name
  | Fun(args, body) =>
    find_var_upat(name, args) ? false : var_mention(name, body)
  | ListLit(l)
  | Tuple(l) =>
    List.fold_left((acc, ue) => {acc || var_mention(name, ue)}, false, l)
  | Let(p, def, body) =>
    find_var_upat(name, p)
      ? false : var_mention(name, def) || var_mention(name, body)
  | Test(u)
  | Parens(u)
  | UnOp(_, u)
  | TyAlias(_, _, u) => var_mention(name, u)
  | Ap(u1, u2)
  | Seq(u1, u2)
  | Cons(u1, u2)
  | ListConcat(u1, u2)
  | BinOp(_, u1, u2) => var_mention(name, u1) || var_mention(name, u2)
  | If(u1, u2, u3) =>
    var_mention(name, u1) || var_mention(name, u2) || var_mention(name, u3)
  | Match(g, l) =>
    var_mention(name, g)
    || List.fold_left(
         (acc, pe) => {
           let (p, e) = pe;
           find_var_upat(name, p) ? false : acc || var_mention(name, e);
         },
         false,
         l,
       )
  | _ => false
  };
};

let rec var_applied = (name: string, uexp: Term.UExp.t): bool => {
  switch (uexp.term) {
  | Fun(args, body) =>
    find_var_upat(name, args) ? false : var_applied(name, body)
  | ListLit(l)
  | Tuple(l) =>
    List.fold_left((acc, ue) => {acc || var_applied(name, ue)}, false, l)
  | Let(p, def, body) =>
    find_var_upat(name, p)
      ? false : var_applied(name, def) || var_applied(name, body)
  | Test(u)
  | Parens(u)
  | UnOp(_, u)
  | TyAlias(_, _, u) => var_applied(name, u)
  | Ap(u1, u2) =>
    switch (u1.term) {
    | Var(x) => x == name ? true : var_applied(name, u2)
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
           find_var_upat(name, p) ? false : acc || var_applied(name, e);
         },
         false,
         l,
       )

  | _ => false
  };
};

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
  | (Var(x), Fun(_)) =>
    if (x == name) {
      [def, ...l];
    } else {
      l;
    }
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
  | _ => l
  };
};

let rec find_fn =
        (name: string, uexp: Term.UExp.t, l: list(Term.UExp.t))
        : list(Term.UExp.t) => {
  switch (uexp.term) {
  | Let(up, def, body) =>
    l |> find_in_let(name, up, def) |> find_fn(name, body)
  | ListLit(ul)
  | Tuple(ul) =>
    List.fold_left((acc, u1) => {find_fn(name, u1, acc)}, l, ul)
  | Fun(_, body) => l |> find_fn(name, body)
  | Parens(u1)
  | UnOp(_, u1)
  | TyAlias(_, _, u1) => l |> find_fn(name, u1)
  | Ap(u1, u2)
  | Seq(u1, u2)
  | Cons(u1, u2)
  | ListConcat(u1, u2)
  | BinOp(_, u1, u2) => l |> find_fn(name, u1) |> find_fn(name, u2)
  | If(u1, u2, u3) =>
    l |> find_fn(name, u1) |> find_fn(name, u2) |> find_fn(name, u3)
  | Match(u1, ul) =>
    List.fold_left(
      (acc, (_, ue)) => {find_fn(name, ue, acc)},
      l |> find_fn(name, u1),
      ul,
    )
  | _ => l
  };
};

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

let is_not_recursive = (name: string, uexp: Term.UExp.t): bool =>
  !is_recursive(name, uexp);

let rec tail_check = (name: string, uexp: Term.UExp.t): bool => {
  switch (uexp.term) {
  | Fun(args, body) =>
    find_var_upat(name, args) ? false : tail_check(name, body)
  | Let(p, def, body) =>
    find_var_upat(name, p) || var_mention(name, def)
      ? false : tail_check(name, body)
  | ListLit(l)
  | Tuple(l) =>
    //If l has no recursive calls then true
    !List.fold_left((acc, ue) => {acc || var_mention(name, ue)}, false, l)
  | Test(_) => false
  | TyAlias(_, _, u)
  | Parens(u) => tail_check(name, u)
  | UnOp(_, u) => !var_mention(name, u)
  | Ap(u1, u2) => var_mention(name, u2) ? false : tail_check(name, u1)
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
            find_var_upat(name, p) ? false : acc && tail_check(name, e)
          },
          true,
          l,
        )

  | _ => true
  };
};

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
