open Haz3lcore;
open Sexplib.Std;

module StringMap = Map.Make(String);

[@deriving (show({with_path: false}), sexp, yojson)]
type syntax_tests = {
  var_mention: list((string, float)),
  recursive: list((string, float)),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type syntax_result = {
  hinted_results: list((bool, string)),
  percentage: float,
};

type fmap = StringMap.t(list(Term.UExp.t));

let add_flist = (l: list(string), m: fmap): fmap => {
  List.fold_left((m, name) => {StringMap.add(name, [], m)}, m, l);
};

let rec find_funcs = (p: Term.UPat.t, def: Term.UExp.t, m: fmap): fmap => {
  switch (p.term, def.term) {
  | (Parens(up), Parens(ue)) => find_funcs(up, ue, m)
  | (Parens(up), _) => find_funcs(up, def, m)
  | (_, Parens(ue)) => find_funcs(p, ue, m)
  | (TypeAnn(up, _), _) => find_funcs(up, def, m)
  | (Var(x), Fun(_)) =>
    switch (StringMap.find_opt(x, m)) {
    | None => m
    | Some(l) => StringMap.add(x, [def, ...l], m)
    }
  | (Tuple(pl), Tuple(ul)) =>
    if (List.length(pl) != List.length(ul)) {
      m;
    } else {
      List.fold_left2(
        (acc, upat, uexp) => {find_funcs(upat, uexp, acc)},
        m,
        pl,
        ul,
      );
    }
  | _ => m
  };
};

let rec mk_fmap = (uexp: Term.UExp.t, m: fmap): fmap => {
  switch (uexp.term) {
  | Let(p, def, body) => find_funcs(p, def, m) |> mk_fmap(body)
  | Fun(_, body) => m |> mk_fmap(body)
  | Tuple(l) => List.fold_left((acc, ue) => {mk_fmap(ue, acc)}, m, l)
  | Ap(u1, u2) => m |> mk_fmap(u1) |> mk_fmap(u2)
  | If(u1, u2, u3) => m |> mk_fmap(u1) |> mk_fmap(u2) |> mk_fmap(u3)
  | Seq(u1, u2) => m |> mk_fmap(u1) |> mk_fmap(u2)
  | Parens(u) => mk_fmap(u, m)
  | Cons(u1, u2) => m |> mk_fmap(u1) |> mk_fmap(u2)
  | UnOp(_, u) => mk_fmap(u, m)
  | BinOp(_, u1, u2) => m |> mk_fmap(u1) |> mk_fmap(u2)
  | Match(g, l) =>
    List.fold_left((acc, (_, ue)) => {mk_fmap(ue, acc)}, mk_fmap(g, m), l)
  | _ => m //Unlikely that we would need to check inside a test
  };
};

let rec find_var_upat = (upat: Term.UPat.t, name: string): bool => {
  switch (upat.term) {
  | Var(x) => x == name
  | Cons(up1, up2) => find_var_upat(up1, name) || find_var_upat(up2, name)
  | Tuple(l) =>
    List.fold_left((acc, up) => {acc || find_var_upat(up, name)}, false, l)
  | Parens(up) => find_var_upat(up, name)
  | Ap(up1, up2) => find_var_upat(up1, name) || find_var_upat(up2, name)
  | TypeAnn(up, _) => find_var_upat(up, name)
  | ListLit(l) =>
    List.fold_left((acc, up) => {acc || find_var_upat(up, name)}, false, l)
  | _ => false
  };
};

let rec find_var_uexp = (uexp: Term.UExp.t, name: string): bool => {
  switch (uexp.term) {
  | Var(x) => x == name
  | Fun(_, body) => find_var_uexp(body, name)
  | Tuple(l) =>
    List.fold_left((acc, ue) => {acc || find_var_uexp(ue, name)}, false, l)
  | Let(_, def, body) =>
    find_var_uexp(def, name) || find_var_uexp(body, name)
  | Ap(u1, u2) => find_var_uexp(u1, name) || find_var_uexp(u2, name)
  | If(u1, u2, u3) =>
    find_var_uexp(u1, name)
    || find_var_uexp(u2, name)
    || find_var_uexp(u3, name)
  | Seq(u1, u2) => find_var_uexp(u1, name) || find_var_uexp(u2, name)
  | Test(u) => find_var_uexp(u, name)
  | Parens(u) => find_var_uexp(u, name)
  | Cons(u1, u2) => find_var_uexp(u1, name) || find_var_uexp(u2, name)
  | UnOp(_, u) => find_var_uexp(u, name)
  | BinOp(_, u1, u2) => find_var_uexp(u1, name) || find_var_uexp(u2, name)
  | Match(g, l) =>
    find_var_uexp(g, name)
    || List.fold_left(
         (acc, pe) => {
           let (_, u) = pe;
           acc || find_var_uexp(u, name);
         },
         false,
         l,
       )
  | ListLit(l) =>
    List.fold_left((acc, ue) => {acc || find_var_uexp(ue, name)}, false, l)

  | _ => false
  };
};

//This function checks if the expression uexp
//uses the variable with name "name" and is never
//replaced with another variable with the same name
let rec is_recursive = (uexp: Term.UExp.t, name: string): bool => {
  switch (uexp.term) {
  | Var(x) => x == name
  | Fun(args, body) =>
    if (find_var_upat(args, name)) {
      false;
    } else {
      is_recursive(body, name);
    }
  | Tuple(l) =>
    List.fold_left((acc, ue) => {acc || is_recursive(ue, name)}, false, l)
  | Let(p, def, body) =>
    if (find_var_upat(p, name)) {
      false;
    } else {
      is_recursive(def, name) || is_recursive(body, name);
    }
  | Ap(u1, u2) => is_recursive(u1, name) || is_recursive(u2, name)
  | If(u1, u2, u3) =>
    is_recursive(u1, name)
    || is_recursive(u2, name)
    || is_recursive(u3, name)
  | Seq(u1, u2) => is_recursive(u1, name) || is_recursive(u2, name)
  | Test(u) => is_recursive(u, name)
  | Parens(u) => is_recursive(u, name)
  | Cons(u1, u2) => is_recursive(u1, name) || is_recursive(u2, name)
  | UnOp(_, u) => is_recursive(u, name)
  | BinOp(_, u1, u2) => is_recursive(u1, name) || is_recursive(u2, name)
  | Match(g, l) =>
    is_recursive(g, name)
    || List.fold_left(
         (acc, pe) => {
           let (p, e) = pe;
           if (find_var_upat(p, name)) {
             false;
           } else {
             acc || is_recursive(e, name);
           };
         },
         false,
         l,
       )
  | ListLit(l) =>
    List.fold_left((acc, ue) => {acc || is_recursive(ue, name)}, false, l)

  | _ => false
  };
};

let check = (uexp: Term.UExp.t, p: syntax_tests): syntax_result => {
  let var_mention_names = List.map(((name, _)) => name, p.var_mention);
  let var_mention_weights = List.map(((_, w)) => w, p.var_mention);

  let recursive_names = List.map(((name, _)) => name, p.recursive);
  let recursive_weights = List.map(((_, w)) => w, p.recursive);

  let total =
    List.fold_left((+.), 0., var_mention_weights @ recursive_weights);

  let m = StringMap.empty |> add_flist(recursive_names) |> mk_fmap(uexp);
  /*StringMap.iter(
      (k, l) => {
        print_endline(k);
        print_endline(string_of_int(List.length(l)));
        List.iter(ue => {print_endline(Term.UExp.show(ue))}, l);
      },
      m,
    );*/

  let var_mention_res =
    List.map(name => {find_var_uexp(uexp, name)}, var_mention_names);

  let recursive_res =
    List.map(
      name => {
        let l = StringMap.find(name, m);
        if (l == []) {
          false;
        } else {
          List.fold_left(
            (acc, ufun) => {acc && is_recursive(ufun, name)},
            true,
            StringMap.find(name, m),
          );
        };
      },
      recursive_names,
    );

  let passing =
    List.fold_left2(
      (acc, w, res) => {res ? acc +. w : acc},
      0.,
      var_mention_weights @ recursive_weights,
      var_mention_res @ recursive_res,
    );

  let var_mention_hinted_results =
    List.map2(
      (r, name) => {(r, name ++ " is mentioned anywhere")},
      var_mention_res,
      var_mention_names,
    );

  let recursive_hinted_results =
    List.map2(
      (r, name) => {(r, name ++ " is recursive")},
      recursive_res,
      recursive_names,
    );

  {
    hinted_results: var_mention_hinted_results @ recursive_hinted_results,
    percentage: Float.equal(total, 0.) ? 1. : passing /. total,
  };
};
